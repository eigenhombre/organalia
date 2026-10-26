(ns organalia.main-test
  (:require [clojure.test :refer [testing deftest is are]]
            [organalia.main :refer :all]))

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


