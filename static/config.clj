(ns static.config
  (:require [clojure.tools.logging :as log])
  (:import (java.io File)))

(let [defaults {:site-title "A Static Blog"
                :site-description "Default blog description"
                :site-url "https://github.com/
