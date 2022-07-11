
(defproject ll "0.1.0-SNAPSHOT"
  :description "lifelog"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [dev.weavejester/ragtime "0.9.1"]
                 [org.xerial/sqlite-jdbc "3.36.0.3"]
                 ;;[org.clojure/java.jdbc "0.7.12"]
                 [korma "0.5.0-RC1"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :repl-options {:init-ns ll.core}
  :aliases {"migrate"  ["run" "-m" "ll.core/migrate"]
            "rollback" ["run" "-m" "ll.core/rollback"]
            "import"   ["run" "-m" "ll.core/import-data"]
            "export"   ["run" "-m" "ll.core/export-data"]}
  :main ll.core
  :aot [ll.core])

