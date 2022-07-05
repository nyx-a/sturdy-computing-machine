
(ns ll.core
  (:gen-class)
  (:require
   [clojure.string :as s]
   [clojure.spec.alpha :as spec]
   [ragtime.jdbc   :as jdbc]
   [ragtime.repl   :as repl]
   [korma.core     :as k]
   [korma.db       :as db])
  (:import
   [java.time LocalDateTime]
   [java.time.format DateTimeFormatter]
   [org.sqlite SQLiteException SQLiteErrorCode]))

;;
;; 定数
;;

(def default-target
  "~/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/lifelog.txt")
(def db-file "resources/db.sqlite3")
(def formatter (DateTimeFormatter/ofPattern "YYYY-MM-dd HH:mm"))
(def date-pattern #"(?m)^\s*\[(\d+)\s+(\d+)/(\d+)\s+(\d+):(\d+)\]")

;;
;; 関数
;;

(defn raise [s]
  (-> s Exception. throw))

(defn re-comp [fmt & xs]
  (re-pattern (apply format fmt xs)))

(defn expand-home [s]
  (if (.startsWith s "~")
    (s/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn s-to-ldt [s]
  (let [[Y M D h m] (map #(Integer/parseInt %) (re-seq #"\d+" s))]
    (LocalDateTime/of Y M D h m)))

(defn ldt-to-s [ldt]
  (.format ldt formatter))

(defn sqlite-ymdhm-utc [ldt]
  "korma で sqlite に UTC 時刻を格納するための専用関数 ( ldt はローカル時刻 )"
  (k/raw
   (format
    "datetime('%d-%02d-%02d %02d:%02d', 'utc')"
    (.getYear ldt)
    (.getMonthValue ldt)
    (.getDayOfMonth ldt)
    (.getHour ldt)
    (.getMinute ldt))))

(defmacro assert-nil [sym]
  `(assert (nil? ~sym) ~(s/join ["\"" sym "\" is already there."])))

(defmacro diplopia [& xs]
  "[foo bar baz] --> {:foo foo , :bar bar , :baz baz}"
  (cons 'array-map (mapcat #(list (keyword %) %) xs)))

(defn re-pull [re s & capture-names]
  "1. マッチした部分の前と後を繋げて返す
   2. 名前付きキャプチャのうち指定されたもののみをマップに入れて返す"
  (let [m (re-matcher re s)]
    (if (.find m)
      [(s/join [(subs s 0 (.start m)) (subs s (.end m))]) ;; 左+右
       (into {} (map #(vector (keyword %) (.group m (name %))) capture-names))])))

(defn re-split [re s]
  "分割に使用した正規表現（にマッチした部分）も結果のベクタに含める"
  (let [m (re-matcher re s)]
    (loop [last   0
           hit?   (.find m)
           result []]
      (if hit?
        (let [start (.start m)
              end   (.end   m)]
          (recur end          ;; last
                 (.find m)    ;; hit?
                 (conj result ;; result
                       (subs s last  start)
                       (subs s start end))))
        (conj result
              (subs s last))))))

(defn structuralize [s]
  "立体化"
  (let [coll  (re-split date-pattern s)
        first (first coll)
        rest  (rest coll)]

    ;; ファイルの一番最初の日付前に文字列があってはいけない
    (if (re-find #"\S" first)
      (-> "there is a some string before first timestamp" Exception. throw))

    (for [[time text] (partition 2 rest)]
      [(s-to-ldt time) (s/trim text)])))

;;
;; 日付時刻の指定をできるだけ解釈しようとする
;;

(def p-year   #"\b(?<year>\d{4})\b")
(def p-month  #"\b(?<month>0?[1-9]|1[0-2])\b")
(def p-day    #"\b(?<day>0?[1-9]|[12][0-9]|3[01])\b")
(def p-hour   #"\b(?<hour>[01]?[0-9]|2[0-3])\b")
(def p-minute #"\b(?<minute>[0-5]?[0-9])\b")
(def p-second #"\b(?<second>[0-5]?[0-9])\b") ;; グループ名だけ違う

(def p-md  (re-comp "%s\\s*[/-]\\s*%s" p-month p-day))
(def p-hms (re-comp "%s\\s*:\\s*%s(?:\\s*:\\s*%s)?" p-hour p-minute p-second))

(defn- ymdhms-puller
  "for use on trampoline"
  ([rest]
   #(ymdhms-puller rest {}))
  ([rest {:keys [year month day hour minute]}]
   ;; 内側のletで同名シンボルを隠したいけど外側の同名シンボルにアクセスもしたい
   (let [_year   year
         _month  month
         _day    day
         _hour   hour
         _minute minute]
     (or
      ;; 月日らしきものがある
      (when-let [[rest {:keys [month day]}] (re-pull p-md rest :month :day)]
        (assert-nil _month)
        (assert-nil _day)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; 時分(秒)らしきものがある
      (when-let [[rest {:keys [hour minute]}] (re-pull p-hms rest :hour :minute)]
        (assert-nil _hour)
        (assert-nil _minute)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; 年らしきものがある
      (when-let [[rest {:keys [year]}] (re-pull p-year rest :year)]
        (assert-nil _year)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; もう何もなさそうだ
      (diplopia year month day hour minute rest)))))

(defn parse-ymdhm [s]
  (trampoline ymdhms-puller s))

;;
;; migration
;;

(defn ragtime-config []
  {:datastore  (jdbc/sql-database {:connection-uri (s/join ["jdbc:sqlite:" db-file])})
   :migrations (jdbc/load-resources "migrations")})

(defn migrate []
  (repl/migrate (ragtime-config)))

(defn rollback []
  (repl/rollback (ragtime-config)))

;;
;; db
;;

(db/defdb mydatabase (db/sqlite3 {:db db-file}))

(k/defentity lifelog
  (k/table :lifelog)
  (k/pk :id)
  (k/database mydatabase))

(defn matrix [path]
  (-> path expand-home str slurp structuralize))

;;
;; main
;;

(defn import-data
  ([]
   (import-data default-target))
  ([filename]
   (doseq [[ldt text] (matrix filename)]
     (print (ldt-to-s ldt))
     (try
       (k/insert lifelog (k/values {:date (sqlite-ymdhm-utc ldt) :text text}))
       (catch SQLiteException e
         (if (= (.getResultCode e) SQLiteErrorCode/SQLITE_CONSTRAINT_UNIQUE)
           (print " already exists")
           (throw e))))
     (println))))

(defn -main [& args]
  (apply import-data args))

