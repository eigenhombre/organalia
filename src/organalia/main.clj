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

(defn- strip-comment [s]
  (-> s
      str/trim-newline
      (str/replace #"^;+\s*" "")))

(defn- paragraphs [items]
  (->> items
       (partition-by nil?)
       (map (partial str/join "\n"))
       (remove #(= % ""))))

(defn parse-forms-and-comments [source]
  (->>
   (n/children (p/parse-string-all source))
   (reduce
    (fn [[acc items] node]
      (case (n/tag node)
        :comment [acc (conj items (strip-comment (n/string node)))]
        :newline (if (and (seq items) (some? (peek items)))
                   [acc (conj items nil)]
                   [acc items])
        :whitespace [acc items]
        :comma [acc items]
        [(conj acc {:paragraphs (paragraphs items)
                    :form-string (n/string node)
                    :form (n/sexpr node)}) []]))
    [[] []])
   (apply (fn [acc items] (conj acc {:paragraphs (paragraphs items)})))))

(defn- code-block [s]
  (format "#+BEGIN_SRC\n%s\n#+END_SRC" s))

(defn ->org [forms-and-comments]
  (str/join "\n"
            (for [{:keys [paragraphs form-string]} forms-and-comments
                  :when (or (seq paragraphs) form-string)]
              (str (when (seq paragraphs)
                     (str (str/join "\n\n" paragraphs) "\n"))
                   (when form-string
                     (code-block form-string))))))

(deftest parse-forms-and-comments-test
  (testing "single form with no comments"
    (is (= [{:paragraphs () :form-string "(def x 1)" :form '(def x 1)}
            {:paragraphs ()}]
           (parse-forms-and-comments "(def x 1)"))))

  (testing "comments before form become one block"
    (let [result (parse-forms-and-comments ";; line 1\n;; line 2\n(def x 1)")]
      (is (= 2 (count result)))
      (is (= "(def x 1)" (:form-string (first result))))
      (is (= '("line 1\nline 2") (:paragraphs (first result))))
      (is (= () (:paragraphs (second result))))))

  (testing "blank line between comments splits paragraphs"
    (let [result (parse-forms-and-comments ";; para 1 line 1
;; para 1 line 2

;; para 2
(def x 1)")]
      (is (= 2 (count result)))
      (is (= '("para 1 line 1\npara 1 line 2" "para 2")
             (:paragraphs (first result))))
      (is (= "(def x 1)" (:form-string (first result))))
      (is (= () (:paragraphs (second result))))))

  (testing "trailing comments after last form are captured"
    (let [result (parse-forms-and-comments "(def x 1)\n;; trailing")]
      (is (= 2 (count result)))
      (is (= () (:paragraphs (first result))))
      (is (= '("trailing") (:paragraphs (second result)))))))

(defn -main []
  (println "Hello, World!"))

(comment
  (spit "/tmp/output.org"
        (->org
         (parse-forms-and-comments
          (slurp *file*)))))
