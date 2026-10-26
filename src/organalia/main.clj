(ns organalia.main-test
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint *print-right-margin*]]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [clojure.test :refer [testing deftest is are]]))

;; The goal of this project is to be able to generate Org Mode output from
;; a Clojure program, similar to what Marginalia does for Clojure-to-HTML.

;; Ideally, we do this without writing an entire parser from scratch.
;; rewrite-clj already parses Clojure source into a tree of nodes,
;; preserving comments and whitespace, so we use that instead of
;; mutating the reader.

(defn parse-forms-and-comments [source]
  (let [children (n/children (p/parse-string-all source))]
    (loop [[node & more] children
           acc []
           comments []]
      (cond
        (nil? node) (conj acc comments)

        (= (n/tag node) :comment)
        (recur more acc (conj comments {:content (n/string node)}))

        (#{:whitespace :newline :comma} (n/tag node))
        (recur more acc comments)
        :else
        (recur more (conj acc {:comments comments
                               :form-string (n/string node)
                               :form (n/sexpr node)}) [])))))



(defn -main []
  (println "Hello, World!"))

(defn pformat [form]
  (with-out-str
    (binding [*print-right-margin* 60]
      (pprint form))))

(defn ->code [s]
  (format "#+BEGIN_SRC\n%s\n#+END_SRC" s))

(defn ->org [forms-and-comments]
  (str/join "\n"
            (for [{:keys [comments form-string]} forms-and-comments]
              (str
               (str/join "\n"
                         (map (comp #(str/replace % #"^;+\s*" "")
                                    str/trim-newline
                                    :content)
                              comments))
               "\n"
               (->code form-string)))))

(comment
  (spit "/tmp/output.org"
        (->org
         (parse-forms-and-comments
          (slurp *file*)))))

(->org (parse-forms-and-comments "
;; hi
;; there

;; new paragraph"))

;;=>
"\n#+BEGIN_SRC\nnull\n#+END_SRC"
