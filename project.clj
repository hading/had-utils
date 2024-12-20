(defproject org.clojars.hading/had-utils "7.0.10"
  :description "Personal utilites"
  :url "https://github.com/hading/had-utils"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0.txt"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [ubergraph "0.9.0"]]
  :plugins [[lein-codox "0.10.8"]
            [lein-cloverage "1.2.2"]]
  :codox {:output-path "doc"
          :metadata {:doc/format :markdown}}
  :repl-options {:init-ns had-utils.core}
  :repositories [["releases" {:url "https://repo.clojars.org"
                              :username "hading"
                              :password [:env/lein_password :gpg]}]])
