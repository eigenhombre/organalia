;; This program generates Org Mode output from a Clojure program,
;; similar to what Marginalia does for Clojure-to-HTML.

;; Ideally, we do this without writing an entire parser from scratch.
;; ~rewrite-clj~ already parses Clojure source into a tree of nodes,
;; preserving comments and whitespace, so we use that instead of
;; mutating the reader.

(ns organalia.main
  (:gen-class)
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

;; Code blocks are slightly uglier in Org Mode, and there are many variations.
;; We'll stick with one of the simplest.

(defn- code-block [s]
  (format "\n#+BEGIN_SRC\n%s\n#+END_SRC\n" s))

;; When we encounter a comment, we strip the leftmost semicolons.

(defn- strip-comment [s]
  (-> s
      str/trim-newline
      (str/replace #"^;+\s*" "")))

(defn- paragraphs [items]
  (->> items
       (partition-by nil?)
       (map (partial str/join "\n"))
       (remove #(= % ""))))

(defn- process-node [[acc items] node]
  (case (n/tag node)
    :comment [acc (conj items (strip-comment (n/string node)))]
    :newline (if (and (seq items)
                      (some? (peek items)))
               [acc (conj items nil)]
               [acc items])
    :whitespace [acc items]
    :comma [acc items]

    ;; Else,
    [(conj acc {:paragraphs (paragraphs items)
                :form-string (n/string node)
                :form (n/sexpr node)}) []]))

(defn- add-paragraphs [acc items]
  (conj acc {:paragraphs (paragraphs items)}))

(defn parse-forms-and-comments [source]
  (->> source
       p/parse-string-all
       n/children
       (reduce process-node [[] []])
       (apply add-paragraphs)))

(defn ->org [forms-and-comments]
  (str/join "\n"
            (for [{:keys [paragraphs form-string]} forms-and-comments
                  :when (or (seq paragraphs) form-string)]
              (str (when (seq paragraphs)
                     (str (str/join "\n\n" paragraphs) "\n"))
                   (when form-string
                     (code-block form-string))))))

;; REPL example parsing this file:
(comment
  (->> *file*
       slurp
       parse-forms-and-comments
       ->org
       (spit "/tmp/output.org")))

;; Generally, we want to make a ~.org~ file next to the input file:
(defn- org-file-name [fname]
  (str/replace (str fname)
               #"\.[^.]+$"
               ".org"))

;; Finally, when the program runs for real, we want to apply this
;; to every file in the argument list:
(defn -main [& files]
  (doseq [f files
          :let [orgfile (org-file-name f)]]
    (->> f
         slurp
         parse-forms-and-comments
         ->org
         (spit orgfile))))

