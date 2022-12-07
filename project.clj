(defproject org.clojars.hading/had-utils "2.0.0"
  :description "Personal utilites"
  :url "https://github.com/hading/had-utils"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0.txt"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns had-utils.core}
  :repositories [["releases" {:url "https://repo.clojars.org"
                              :username "hading"
                              :password [:env/lein_password :gpg]}]])
