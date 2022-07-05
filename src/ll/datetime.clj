
(ns ll.datetime
  (:require [clojure.string :as s]))

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

(defn re-comp [fmt & xs]
  (re-pattern (apply format fmt xs)))

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

