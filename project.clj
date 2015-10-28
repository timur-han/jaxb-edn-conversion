(defproject uni-stuttgart.ipsm/jaxb-edn-conversion "0.0.1-SNAPSHOT"
  :description "A library to convert jaxb objects into edn maps and vice versa"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :comments "same as Clojure"}
  :min-lein-version "2.5.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [camel-snake-kebab "0.3.2"]
                 [metam/core "1.0.7"]]
  :source-paths ["src/main/clj"]
  :plugins [[lein-modules "0.3.11"]]
  :resource-paths ["src/main/resources"])
