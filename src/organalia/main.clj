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
            [rewrite-clj.parser :refer [parse-string-all]]))

;; ~parse-string-all~ returns a map-like thing (a
;; "~FormsNode~") with one key: ~:children~.  ~:children~ is a
;; collection of things: newlines, comments, keywords, seqs, etc.

;; For example, given this example program,

(def example-program "

;; This is my Clojure program

;; There are many like it, but this one is mine.
;; Or is it?

:a

(def a-form 333)
")

;; the parse tree generates the following top-level nodes:

(->> example-program
     parse-string-all
     :children
     (map (comp str type))
     vec)

;;=>
["class rewrite_clj.node.whitespace.NewlineNode"
 "class rewrite_clj.node.comment.CommentNode"
 "class rewrite_clj.node.whitespace.NewlineNode"
 "class rewrite_clj.node.comment.CommentNode"
 "class rewrite_clj.node.comment.CommentNode"
 "class rewrite_clj.node.whitespace.NewlineNode"
 "class rewrite_clj.node.keyword.KeywordNode"
 "class rewrite_clj.node.whitespace.NewlineNode"
 "class rewrite_clj.node.seq.SeqNode"
 "class rewrite_clj.node.whitespace.NewlineNode"]

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
       parse-string-all
       n/children
       (reduce process-node [[] []])
       (apply add-paragraphs)))

(defn- entry->org [{:keys [paragraphs form-string]}]
  (not-empty
   (str (when (seq paragraphs)
          (str (str/join "\n\n" paragraphs) "\n"))
        (when form-string
          (code-block form-string)))))

(defn- base-file-name [file-name]
  (-> file-name java.io.File. .getName))

(defn- org-preamble [file-name]
  (format "#+TITLE: ~%s~\n#+OPTIONS: toc:nil num:nil\n"
          (base-file-name file-name)))

(defn ->org [file-name forms-and-comments]
  (str (org-preamble file-name)
       (str/join "\n" (keep entry->org forms-and-comments))))

;; REPL example parsing this file:
(comment
  (->> *file*
       slurp
       parse-forms-and-comments
       (->org *file*)
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
         (->org f)
         (spit orgfile))))

(->> *file*
     slurp
     parse-forms-and-comments
     (->org *file*)
     (spit "/tmp/output.org"))
