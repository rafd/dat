(defproject com.github.rafd/dat "0.0.1-20261013-0"
  :description "Database library wrapping datomic/datascript/datalevin with a common API, and exposing malli schemas and a pathom graph."
  :url "https://github.com/rafd/dat"
  :license {:name "MIT"}

  :dependencies [[metosin/malli "0.20.0"]
                 [com.rpl/specter "1.1.6"]
                 [com.wsscode/pathom3 "2025.01.16-alpha"]
                 [com.hyperfiddle/rcf "20220926-202227"]
                 [datascript "1.7.8"]]

  :profiles {:dev {:source-paths ["dev-src"]}
             ;; generate-tests makes rcf/tests blocks expand to clojure.test vars,
             ;; so that `lein test` finds and counts them.
             ;; dev-src is dropped so that its (rcf/enable!) does not also run
             ;; every block at load time
             :test {:source-paths ^:replace ["src"]
                    :jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]}})
