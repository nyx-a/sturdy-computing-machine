
(ns ll.datetime
  (:require [clojure.string :as s]))

;;
;; 関数,マクロ
;;

(defn overlay [lower upper]
  "これをやる関数は既にあるはず。見つけたら消す。"
  (map
   #(or (nth upper % nil) (nth lower % nil))
   (range (max (count lower) (count upper)))))

(defn ldt-to-list [ldt]
  (list
   (.getYear       ldt)
   (.getMonthValue ldt)
   (.getDayOfMonth ldt)
   (.getHour       ldt)
   (.getMinute     ldt)))

(defn now []
  (ldt-to-list (java.time.LocalDateTime/now)))

(defmacro assert-nil [sym]
  `(assert (nil? ~sym) ~(s/join ["\"" sym "\" is already there."])))

(defmacro diplopia [& xs]
  "[foo bar baz] --> {:foo foo , :bar bar , :baz baz}"
  (cons 'array-map (mapcat #(list (keyword %) %) xs)))

(defn re-format [fmt & xs]
  (re-pattern (apply format fmt xs)))

(defn re-pull [re s]
  "[マッチしなかった部分からのみ成る文字列 マッチ全体 グループ1 2 3..]
   という平坦なリストを返す"
  (let [m (re-matcher re s)]
    (if (.find m)
      (cons
       (s/join [(subs s 0 (.start m)) (subs s (.end m))])
       (map #(.group m %) (-> m .groupCount inc range))))))

;;
;; 定義
;;

(def p-year  #"\b(\d{4})\b")
(def p-month #"\b(0?[1-9]|1[0-2])\b")
(def p-day   #"\b(0?[1-9]|[12][0-9]|3[01])\b")
(def p-24    #"\b([01]?[0-9]|2[0-3])\b")
(def p-60    #"\b([0-5]?[0-9])\b")

(def cp-md  (re-format "%s\\s*[/-]\\s*%s" p-month p-day))
(def cp-hms (re-format "%s\\s*:\\s*%s(?:\\s*:\\s*%s)?" p-24 p-60 p-60))

;;
;; 本体
;;

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
      (when-let [[rest _ month day] (re-pull cp-md rest)]
        (assert-nil _month)
        (assert-nil _day)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; 時分(秒)らしきものがある
      (when-let [[rest _ hour minute second] (re-pull cp-hms rest)]
        (assert-nil _hour)
        (assert-nil _minute)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; 年らしきものがある
      (when-let [[rest _ year] (re-pull p-year rest)]
        (assert-nil _year)
        #(ymdhms-puller rest (diplopia year month day hour minute)))

      ;; もう何もなさそうだ
      (diplopia year month day hour minute rest)))))

(defn parse-into-ymdhm [s]
  (trampoline ymdhms-puller s))

(defn ymdhm-to-list [ymdhm]
  (map
   #(if % (Integer/parseInt %))
   (map ymdhm [:year :month :day :hour :minute])))

(defn ymdhm-to-ldt [ymdhm]
  "ここで省略された場所に現在時刻を補う"
  (apply
   #(java.time.LocalDateTime/of %1 %2 %3 %4 %5)
   (overlay (now) (ymdhm-to-list ymdhm))))

(defn parse-into-ldt [s]
  (ymdhm-to-ldt (parse-into-ymdhm s)))

