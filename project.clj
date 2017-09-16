(defproject hask-tools "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 ;;
                 ;;[org.clojure/spec.alpha "0.1.123"]
                 ;; Emulator
                 ;;TODO remove; this is for
                 ;; manga script
                 [clj-tagsoup/clj-tagsoup "0.3.0"]
                 [clj-http "3.6.1"]
                 [crouton "0.1.2"]
                 [org.clojure/data.zip "0.1.1"]
                 ;;manga reading]
                 [seesaw "1.4.5"]
                 ;;Parsing
                 [instaparse "1.4.7"]]
                 ;;[clj-discord "0.1.0-SNAPSHOT"]]
                 ;;[org.van-clj/zetta-parser "0.1.0"]]

  :main hask-tools.core)
