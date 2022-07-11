
(ns ll.core
  (:gen-class)
  (:require
   [clojure.java.io    :as io]
   [clojure.string     :as s]
   [clojure.spec.alpha :as spec]
   [ragtime.jdbc       :as jdbc]
   [ragtime.repl       :as repl]
   [korma.core         :as k]
   [korma.db           :as db]
   [ll.datetime        :refer :all])
  (:import
   [java.nio.file    FileSystem FileSystems]
   [java.io          File]
   [java.time        LocalDateTime]
   [java.time.format DateTimeFormatter]
   [org.sqlite       SQLiteException SQLiteErrorCode]))

;;
;; 定数
;;

(def default-dir "~/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents")
(def default-files "lifelog*.txt")
(def default-date-pattern #"(?m)^\s*\[\s*(\d+)\s+(\d+)/(\d+)\D+(\d+):(\d+)\s*\]")

(def db-file "resources/db.sqlite3")
(def formatter (DateTimeFormatter/ofPattern "YYYY-MM-dd HH:mm"))

(def preview-width 20)

;;
;; 関数
;;

(defn expand-home [s]
  (if (.startsWith s "~")
    (s/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn glob [d f]
  (let [d (io/as-file (expand-home d)) ;; java.io.File
        f (str "glob:" (.getPath (io/file d f)))
        m (. (FileSystems/getDefault) (getPathMatcher f))]
    (map
     str
     (filter
      #(.matches m (.toPath %))
      (file-seq d)))))

(defn s-to-ldt [s]
  (let [[Y M D h m] (map #(Integer/parseInt %) (re-seq #"\d+" s))]
    (LocalDateTime/of Y M D h m)))

(defn ldt-to-s [ldt]
  (.format ldt formatter))

(defn head [n s]
  (s/join (concat
           (take n s)
           (if (< n (count s)) ".."))))

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

(defn structuralize [s date-pattern]
  (let [coll  (re-split date-pattern s)
        first (first coll)
        rest  (rest coll)]

    ;; ファイルの一番最初の日付前に文字列があってはいけない
    (if (re-find #"\S" first)
      (-> "there is a some string before first timestamp" Exception. throw))

    (for [[time text] (partition 2 rest)]
      [(s-to-ldt time) (s/trim text)])))

(defn matrix [d f dp]
  (sort
   (mapcat
    #(-> % slurp (structuralize dp))
    (glob d f))))

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

(defn save-db [matrix]
  (doseq [[ldt text] matrix]
    (try
      (k/insert lifelog (k/values {:date (sqlite-ymdhm-utc ldt) :text text}))
      (println (ldt-to-s ldt) (head preview-width text))
      (catch SQLiteException e
        (if (not= (.getResultCode e) SQLiteErrorCode/SQLITE_CONSTRAINT_UNIQUE)
          (throw e))))))

(defn load-db []
  (k/select lifelog
            (k/fields
             (k/raw "datetime(`date`, 'localtime') as lt")
             :text)
            (k/order :date)))

;;
;; main
;;

(defn import-data
  ([]       (save-db (matrix default-dir default-files default-date-pattern)))
  ([d f]    (save-db (matrix d f default-date-pattern)))
  ([d f dp] (save-db (matrix d f dp))))

(defn export-data []
  (doseq [{:keys [lt text]} (load-db)]
    (println lt text)))

(defn -main [& args]
  (apply import-data args))

