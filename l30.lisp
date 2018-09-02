;;;; -*- mode:lisp; coding:utf-8 -*-

;; Read more: http://javarevisited.blogspot.com/2015/06/top-20-array-interview-questions-and-answers.html


#|
1. How to find missing number in integer vector of 1 to 100?

You have given an integer vector which contains numbers from 1 to 100
but one number is missing, you need to write a Lisp program to find
that missing number in vector.

One trick to solve this problem is calculate sum of all numbers in
vector and compare with expected sum, the difference would be the
missing number.
|#

(defun sum-to (n)
  (check-type n (integer 1))
  (* 1/2 n (1+ n)))

(defun missing-integer (vector)
  (check-type vector vector)
  (assert (plusp (length vector)) (vector))
  (let* ((n         (1+ (length vector)))
         (expected  (sum-to n))
         (sum       (reduce (function +) vector)))
     (- expected sum)))

(assert (equalp (missing-integer #(1 2 3 5 6))  4))



#|
2. How to find duplicate number on Integer vector in Lisp?

An vector contains n numbers ranging from 0 to n-2.  There is exactly
one number is repeated in the vector.  You need to write a program to
find that duplicate number.  For example, if an vector with length 6
contains numbers {0, 3, 1, 2, 3}, then duplicated number is 3.
Actually this problem is very similar to previous one and you can
apply the same trick of comparing actual sum of vector to expected sum
of series to find out that duplicate.
|#

(defun duplicate-integer (vector)
  (check-type vector vector)
  (assert (plusp (length vector)) (vector))
  (let* ((n         (1- (length vector)))
         (expected  (sum-to n))
         (sum       (reduce (function +) vector)))
    (- sum expected)))


(assert (equalp (duplicate-integer #(1 2 3 4 5 6 1)) 1))



#|
3. How to check if vector contains a number in Lisp?

Another interesting vector problem, because vector doesn't provide any
builtin method to check if any number exists or not.  This problem is
essentially how to search an element in vector.  There are two options
sequential search or binary search.  You should ask interviewer about
whether vector is sorted or not, if vector is sorted then you can use
binary search to check if given number is present in vector or
not.  Complexity of binary search is O(logN).  BTW, if interviewer say
that vector is not sorted then you can still sort and perform binary
search otherwise you can use sequential search.  Time complexity of
sequential search in vector is O(n).
|#

(defun sequential-search (n vector)
  (position n vector :test (function =)))

(defun binary-search (n vector lessp)
  "RETURN: the index of the smalest element e such as (<= n e) ;
           (= n e)
When (< n (aref vector 0)) the results are 0 nil
When (< (aref vector (1- (length vector))) n) the results are (length vector) nil"
  (let* ((min 0)
         (max (length vector))
         (cur (truncate (+ min max) 2)))
    (flet ((lt (a b) (funcall lessp a b))
           (ne (a b) (or (funcall lessp a b)
                         (funcall lessp b a))))
     (if (or (zerop max) (lt (aref vector (1- max)) n))
         (values max nil)
         (loop
           ;; :do (print (list min max cur '-> n (aref vector cur)))
           :while (and (< min max) (ne n (aref vector cur)))
           :do (cond
                 ((lt n (aref vector cur))
                  (setf max cur))
                 ((= min cur)
                  (return (values (1+ cur) nil)))
                 (t
                  (setf min cur)))
               (setf cur (truncate (+ min max) 2))
           :finally (return (values cur (not (ne n (aref vector cur))))))))))


(assert (equalp (sequential-search 42 #(1 2 42 33 24)) 2))
(assert (equalp (sequential-search 12 #(1 2 42 33 24)) nil))

(assert (equalp (multiple-value-list (binary-search 0 #(1 2 24 33 42) (function <))) '(0 nil)))
(assert (equalp (multiple-value-list (binary-search 1 #(1 2 24 33 42) (function <))) '(0 t)))
(assert (equalp (multiple-value-list (binary-search 2 #(1 2 24 33 42) (function <))) '(1 t)))
(assert (equalp (multiple-value-list (binary-search 3 #(1 2 24 33 42) (function <))) '(2 nil)))
(assert (equalp (multiple-value-list (binary-search 24 #(1 2 24 33 42) (function <))) '(2 t)))
(assert (equalp (multiple-value-list (binary-search 25 #(1 2 24 33 42) (function <))) '(3 nil)))
(assert (equalp (multiple-value-list (binary-search 33 #(1 2 24 33 42) (function <))) '(3 t)))
(assert (equalp (multiple-value-list (binary-search 35 #(1 2 24 33 42) (function <))) '(4 nil)))
(assert (equalp (multiple-value-list (binary-search 42 #(1 2 24 33 42) (function <))) '(4 t)))
(assert (equalp (multiple-value-list (binary-search 45 #(1 2 24 33 42) (function <))) '(5 nil)))






#|
4. How to find largest and smallest number in unsorted vector?

You have given an unsorted integer vector and you need to find the
largest and smallest element in the vector.  Of course you can sort
the vector and then pick the top and bottom element but that would
cost you O(NLogN) because of sorting, getting element in vector with
index is O(1) operation.
|#


(defun extremums (vector)
  (loop
    :for n :across vector
    :maximize n :into max
    :minimize n :into min
    :finally (return (list min max))))


#-(and) (
         (list (extremums #())
               (extremums #(1))
               (extremums #(1 2))
               (extremums #(2 1))
               (extremums #(3 2 1))
               (extremums #(1 2 3))
               (extremums #(4 5 6 2 1 9 8 7)))
         --> ((0 0) (1 1) (1 2) (1 2) (1 3) (1 3) (1 9))
         )

#|
5. How to find all pairs on integer vector whose sum is equalp to given number?

This is an intermediate level of vector coding question, its neither
too easy nor too difficult.  You have given an integer vector and a
number, you need to write a program to find all elements in vector
whose sum is equalp to the given number.  Remember, vector may contain
both positive and negative numbers, so your solution should consider
that.  Don't forget to write unit test though, even if interviewer is
not asked for it, that would separate you from lot of developers.  Unit
testing is always expected from a professional developer.
|#

(defun find-binary-sums (n vector)
  (loop
    :for i :below (length vector)
    :append (loop
              :for j :from (1+ i) :below (length vector)
              :for sum = (+ (aref vector i) (aref vector j))
              :when (= sum n)
              :collect (list (aref vector i) (aref vector j)))))

(assert (equalp (find-binary-sums 5 #(1 2 3 4 5 6 7 8 9 10 -3 -4 -5))
                '((1 4) (2 3) (8 -3) (9 -4) (10 -5))))
(assert (equalp (find-binary-sums 30 #(1 2 3 4 5 6 7 8 9 10 -3 -4 -5))
                '()))


#|
6. How to find repeated numbers in an vector if it contains multiple duplicate?

This is actually the follow-up question of problem 2, how to find
duplicate number on integer vector.  In that case, vector contains only
one duplicate but what if it contains multiple duplicates? Suppose, an
vector contains n numbers ranging from 0 to n-1 and there are 5
duplicates on it, how do you find it? You can use the approach, we
have learned in similar String based problem of finding repeated
characters in given String.
|#

(defun duplicate-integers (n vector)
  (check-type vector vector)
  (assert (plusp (length vector)))
  (let ((counts (make-array n :initial-element 0)))
    (loop
      :for e :across vector
      :do (incf (aref counts e)))
    (loop
      :for e :below n
      :when (< 1 (aref counts e))
        :collect e)))

(assert (equalp (duplicate-integers 6 #(1 2 3 4 5 0 2 5))
                '(2 5)))
(assert (equalp (duplicate-integers 6 #(1 2 3 4 5 0))
                '()))



#|
7. Write a program to remove duplicates from vector in Lisp?

This is another follow-up question from problem 2 and 6.  You have
given an vector which contains duplicates, could be one or more.  You
need to write a program to remove all duplicates from vector in
Lisp.  For example if given vector is {1, 2, 1, 2, 3, 4, 5} then your
program should return an vector which contains just {1, 2, 3, 4,
5}.
|#

(defun remove-duplicates-from-vector (vector)
  (let ((h (make-hash-table :test (function eql))))
    (loop :for n :across vector
          :do (incf (gethash n h 0)))
    (let ((result (make-array (hash-table-count h)))
          (i -1))
      (loop
        :for n :across vector
        :for c = (gethash n h)
        :do (when (plusp c)
              (setf (aref result (incf i)) n)
              (when (< 1 c)
                (setf (gethash n h) 0))))
      result)))

(assert (equalp (remove-duplicates-from-vector #(1 2 1 2 3 4 5)) #(1 2 3 4 5)))
(assert (equalp (remove-duplicates-from-vector #(1 1 1 1)) #(1)))
(assert (equalp (remove-duplicates-from-vector #()) #()))


#|
8. How to sort an vector in place using QuickSort algorithm?

You will often see sorting problems on vector related questions,
because sorting mostly happen on vector data structure.  You need to
write a program to implement in place quick sort algorithm in
Lisp.  You can implement either recursive or iterative quick sort, its
your choice but you cannot use additional buffer, vector or list, you
must sort vector in place.
|#



#|
9. Write a program to find intersection of two sorted vector in Lisp?

Another interesting vector interview question, where you need to treat
vector as Set.  Your task is to write a function in your favorite
language e.g.  Lisp, Python, C or C++ to return intersection of two
sorted vector.  For example, if the two sorted vectors as input are {21,
34, 41, 22, 35} and {61, 34, 45, 21, 11}, it should return an
intersection vector with numbers {34, 21}, For the sake of this problem
you can assume that numbers in each integer vector are unique.
|#

(defun sorted-vector-intersection (a b)
  (cond
    ((or (zerop (length a)) (zerop (length b)))
     #())
    ((> (aref a 0) (aref a (1- (length a))))
     (sorted-vector-intersection (reverse a) b))
    ((> (aref b 0) (aref b (1- (length b))))
     (sorted-vector-intersection a (reverse b)))
    (t
     (let ((intersection (make-array (min (length a) (length b)) :fill-pointer 0)))
       (loop
         :with i = 0
         :with j = 0
         :while (and (< i (length a)) (< j (length b)))
         :do (cond
               ((< (aref a i) (aref b j))
                (incf i))
               ((> (aref a i) (aref b j))
                (incf j))
               (t
                (vector-push (aref a i) intersection)
                (incf i) (incf j))))
       intersection))))

(assert (equalp (sorted-vector-intersection #(21 22 34 35 41) #(11 21 34 45 61)) #(21 34)))
(assert (equalp (sorted-vector-intersection #(41 35 34 22 21) #(61 45 34 21 11)) #(21 34)))


#|
10. There is an vector with every element repeated twice except one.  Find that element?

This is an interesting vector coding problem, just opposite of question
related to finding duplicates in vector.  Here you need to find the
unique number which is not repeated twice.  For example if given vector
is {1, 1, 2, 2, 3, 4, 4, 5, 5} then your program should return
3.  Also, don't forget to write couple of unit test for your solution.
|#

(defun unique-vector-element (vector)
  (let ((h (make-hash-table :test (function eql))))
    (loop :for n :across vector
          :do (incf (gethash n h 0)))
    (maphash (lambda (n c)
               (when (= 1 c)
                 (return-from unique-vector-element n)))
             h)
    nil))

(assert (equalp (unique-vector-element #(1 1 2 2 3 4 4 5 5)) 3))
(assert (equalp (unique-vector-element #(1 2 3 4 5 1 2 4 5)) 3))
(assert (equalp (unique-vector-element #(42)) 42))
(assert (member (unique-vector-element #(1 2 3)) '(1 2 3)))
(assert (equalp (unique-vector-element #(2 4 2 4)) nil))
(assert (equalp (unique-vector-element #()) nil))


#|
11. How to find kth smallest element in unsorted vector?

You are given an unsorted vector of numbers and k, you need to find the
kth smallest number in the vector.  For example if given vector is {1, 2,
3, 9, 4} and k=2 then you need to find the 2nd smallest number in the
vector, which is 2.  One way to solve this problem is to sort the vector
in ascending order then pick the k-1th element, that would be your kth
smallest number in vector because vector index starts at zero, but can
you do better? Once you are able to solve this vector coding question,
you can solve many similar questions easily e.g.  our next question.
|#

(defun k-extremums (vector k lessp)
  (let ((extremums (sort (subseq vector 0 (min k (length vector))) lessp)))
    (unless (< (length vector) k)
      (flet ((insert (e)
               (multiple-value-bind (index found) (binary-search e extremums lessp)
                 (declare (ignore found))
                 (replace extremums extremums :start1 (1+ index) :start2 index)
                 (setf (aref extremums index) e))))
        (loop
          :for i :from k :below (length vector)
          :for e = (aref vector i)
          :unless (funcall lessp (aref extremums (1- k)) e)
            :do (insert e))))
    extremums))

(assert (equalp (k-extremums #(49 64 1 2 24 5 33 42 72 11 96)
                            3 (lambda (a b)
                                (< (abs (- a 33)) (abs (- b 33)))))
               #(33 42 24)))
(assert (equalp (k-extremums #(49 64 1 2 24 5 33 42 72 11 96)
                     3 (function >))
               #(96 72 64)))
(assert (equalp (k-extremums #(49 64 1 2 24 5 33 42 72 11 96)
                            4 (function <))
               #(1 2 5 11)))

(defun kth-smallest-element (vector k)
  (let ((extremums (k-extremums vector k (function <))))
    (aref extremums (1- (length extremums)))))

(assert (equalp (kth-smallest-element #(49 64 1 2 24 5 33 42 72 11 96) 1)    1))
(assert (equalp (kth-smallest-element #(49 64 1 2 24 5 33 42 72 11 96) 2)    2))
(assert (equalp (kth-smallest-element #(49 64 1 2 24 5 33 42 72 11 96) 3)    5))
(assert (equalp (kth-smallest-element #(49 64 1 2 24 5 33 42 72 11 96) 4)   11))
(assert (equalp (kth-smallest-element #(49 64 1 2 24 5 33 42 72 11 96) 100) 96)) ; feature.

#|
12. How to find kth largest element in unsorted vector?

This problem is exactly same as previous question with only difference
being finding kth largest element instead of kth smallest number.  For
example if given vector is {10, 20, 30, 50, 40} and k = 3 then your
program should return 30 because 30 is the 3rd largest number in
vector.  You can also solve this problem by sorting the vector in
decreasing order and picking k-1th element.  I often seen this vector
question on Lisp interviews with 2 to 3 years experienced guys.
|#

(defun kth-largest-element (vector k)
  (let ((extremums (k-extremums vector k (function >))))
    (aref extremums (1- (length extremums)))))

(assert (equalp (kth-largest-element #(49 64 1 2 24 5 33 42 72 11 96) 1)   96))
(assert (equalp (kth-largest-element #(49 64 1 2 24 5 33 42 72 11 96) 2)   72))
(assert (equalp (kth-largest-element #(49 64 1 2 24 5 33 42 72 11 96) 3)   64))
(assert (equalp (kth-largest-element #(49 64 1 2 24 5 33 42 72 11 96) 4)   49))
(assert (equalp (kth-largest-element #(49 64 1 2 24 5 33 42 72 11 96) 100)  1)) ; feature.


#|
13. How to find common elements in three sorted vector?

Now we are coming on territory of tough vector questions.  Given three
vectors sorted in non-decreasing order, print all common elements in
these vectors.

Examples:

input1 = {1, 5, 10, 20, 40, 80}
input2 = {6, 7, 20, 80, 100}
input3 = {3, 4, 15, 20, 30, 70, 80, 120}
Output: 20, 80
|#

(defun sorted-vector-intersection-3 (a b c)
  (sorted-vector-intersection (sorted-vector-intersection a b) c))

(assert (equalp (sorted-vector-intersection-3 #(1 5 10 20 40 80)
                                              #(6 7 20 80 100)
                                              #(3 4 15 20 30 70 80 120))
                #(20 80)))

(defun sorted-vector-intersection-3* (a b c)
  (declare (optimize (space 3) (speed 3) (debug 0) (safety 0)))
  (check-type a vector)
  (check-type b vector)
  (check-type c vector)
  (assert (every (function realp) a))
  (assert (every (function realp) b))
  (assert (every (function realp) c))
  (let ((lena (length a))
        (lenb (length b))
        (lenc (length c)))
   (cond
     ((or (zerop lena) (zerop lenb) (zerop lenc))
      #())
     ((> (aref a 0) (aref a (1- lena)))
      (sorted-vector-intersection-3* (reverse a) b c))
     ((> (aref b 0) (aref b (1- lenb)))
      (sorted-vector-intersection-3* a (reverse b) c))
     ((> (aref c 0) (aref c (1- lenc)))
      (sorted-vector-intersection-3* a b (reverse c)))
     (t
      (let ((intersection (make-array (min lena lenb lenc) :fill-pointer 0)))
        (loop
          :with i = 0
          :with j = 0
          :with k = 0
          :while (and (< i lena) (< j lenb)  (< k lenc))
          :do (macrolet ((skip-if-smallest (a i b j c k)
                           ;; if (aref a i) is strictly less than the
                           ;; two others, then increment i and return
                           ;; true else return nil.
                           `(when (and (< (aref ,a ,i) (aref ,b ,j))
                                       (< (aref ,a ,i) (aref ,c ,k)))
                              (incf ,i))))
                (cond
                  ((skip-if-smallest a i b j c k))
                  ((skip-if-smallest b j a i c k))
                  ((skip-if-smallest c k a i b j))
                  (t
                   (vector-push (aref a i) intersection)
                   (incf i) (incf j) (incf k)))))
        intersection)))))

(assert (equalp (sorted-vector-intersection-3* #(1 5 10 20 40 80)
                                               #(6 7 20 80 100)
                                               #(3 4 15 20 30 70 80 120))
                #(20 80)))

(defun generate-random-vector (length min-value max-value)
  (map-into (make-array length) (lambda () (+ min-value (random (- max-value min-value))))))

(let ((a (generate-random-vector 1000000 100000000 600000000))
      (b (generate-random-vector 1000000 200000000 700000000))
      (c (generate-random-vector 1000000 300000000 800000000)))
  (let ((r1  (time (sorted-vector-intersection-3  a b c)))
        (r2  (time (sorted-vector-intersection-3* a b c))))
    (assert (equalp r1 r2))))

;; With ccl, without declare optimize sorted-vector-intersection-3 is
;; faster than sorted-vector-intersection-3*.


#|
14. How find the first repeating element in an vector of integers?

Given an vector of integers, find the first repeating element in it.  We
need to find the element that occurs more than once and whose index of
first occurrence is smallest.

Examples:

Input:  input [] = {10, 5, 3, 4, 3, 5, 6}
Output: 5 [5 is the first element that repeats]
|#

(defun first-repeating-element (vector)
  (let ((h (make-hash-table :test (function eql))))
    (loop :for n :across vector
          :for i :from 0
          :do (if (gethash n h)
                  (incf (car (gethash n h)))
                  (setf (gethash n h) (cons 1 i))))
    (let ((min-n nil)
          (min-i nil))
      (maphash (lambda (n e)
                 (when (and (< 1 (car e))
                            (or (null min-n) (< (cdr e) min-i)))
                   (setf min-n n
                         min-i (cdr e))))
               h)
      min-n)))

(assert (equalp (first-repeating-element #(10 5 3 4 3 5 6)) 5))


#|
15. How to find first non-repeating element in vector of integers?

This vector interview question is exactly opposite of previous problem,
In that you need to find first repeating element while in this you
need to find first non-repeating element.  I am sure you can use
similar approach to solve this problem, just need to consider non
repeating element though.
|#

(defun first-non-repeating-element (vector)
  (let ((h (make-hash-table :test (function eql))))
    (loop :for n :across vector
          :for i :from 0
          :do (if (gethash n h)
                  (incf (car (gethash n h)))
                  (setf (gethash n h) (cons 1 i))))
    (let ((min-n nil)
          (min-i nil))
      (maphash (lambda (n e)
                 (when (and (= 1 (car e))
                            (or (null min-n) (< (cdr e) min-i)))
                   (setf min-n n
                         min-i (cdr e))))
               h)
      min-n)))

(assert (equalp (first-non-repeating-element #(10 5 3 4 3 5 6)) 10))


#|
16. How to find top two numbers from an integer vector?

This is another one of the easy vector questions you will find on
telephonic round of Interviews, but its also little bit tricky.  You
are asked to find top two numbers not just the top or highest numbers?
Can you think of how you would do it without sorting? before looking
at solution.
|#



#|
17. How to find the smallest positive integer value that cannot be represented as sum of any subset of a given vector?

This is another tough vector question you will see on Amazon, Microsoft
or Google.  You have given a sorted vector (sorted in non-decreasing
order) of positive numbers, find the smallest positive integer value
that cannot be represented as sum of elements of any subset of given
set.  What makes it more challenging is expected time complexity of
O(n).

Examples:

Input: {1, 3, 6, 10, 11, 15};
Output: 2


18. How to rearrange vector in alternating positive and negative number?

Given an vector of positive and negative numbers, arrange them in an
alternate fashion such that every positive number is followed by
negative and vice-versa maintaining the order of appearance.   Number
of positive and negative numbers need not be equal.  If there are more
positive numbers they appear at the end of the vector.  If there are
more negative numbers, they too appear in the end of the vector.  This
is also a difficult vector problem to solve and you need lot of
practice to solve this kind of problems in real interviews, especially
when you see it first time.  If you have time constraint then always
attempt these kind of questions once you are done with easier ones.

Example:

Input: {1, 2, 3, -4, -1, 4}
Output: {-4, 1, -1, 2, 3, 4}

Input: {-5, -2, 5, 2, 4, 7, 1, 8, 0, -8}
output: {-5, 5, -2, 2, -8, 4, 7, 1, 8, 0}


19. How to find if there is a sub vector with sum equal to zero?

There is whole set of vector related questions which are based upon
sub-vector or only selective elements of vector e.g.  from some range,
this is one of such problem.  Here you are given an vector of positive
and negative numbers, find if there is a sub-vector with 0 sum.

Examples:

Input: {4, 2, -3, 1, 6}
Output: true
There is a sub-vector with zero sum from index 1 to 3.

20. How to remove duplicates from vector in place?

Given a sorted vector, remove the duplicates in place such that each
element appear only once and return the new length.

Do not allocate extra space for another vector, you must do this in
place with constant memory.

For example,
Given input vector A = [1,1,2],

Your function should return length = 2, and A is now [1,2].

When you see a questions which asked you do to sorting or task in
place, it means you cannot use additional vector or buffer, but using
couple of variables is fine.


21. How to remove a given element from vector in Lisp?

This is another vector coding questions similar to previous one.  Here
you don't have to find and remove duplicates but a given number.  In
this problem you are given an vector and a value, remove all instances
of that value in place and return the new length.  The order of
elements can be changed.  It doesn't matter what you leave beyond the
new length.


22. How to merge sorted vector?

Given two sorted integer vectors A and B, merge B into A as one sorted
vector.  You may assume that A has enough space (size that is greater or
equal to m + n) to hold additional elements from B.  The number of
elements initialized in A and B are m and n respectively.  This is
another intermediate vector coding question, its not as simple as
previous one but neither very difficult.


23. How to find sub vector with maximum sum in an vector of positive and
negative number?

Another vector coding question based upon sub-vector.  Here you have to
find the contiguous sub-vector within an vector (containing at least one
number) which has the largest sum.

For example, given the vector [−2,1,−3,4,−1,2,1,−5,4],
the contiguous subvector [4,−1,2,1] has the largest sum = 6.


24. How to find sub vector with largest product in vector of both
positive and negative number?

In this problem, your task is to write a program in Lisp or C++ to
find the contiguous sub-vector within an vector (containing at least one
number) which has the largest product.

For example, given the vector [2,3,-2,4],
the contiguous subvector [2,3] has the largest product = 6.


25. Write a program to find length of longest consecutive sequence in
vector of integers?

Given an unsorted vector of integers, find the length of the longest
consecutive elements sequence.

For example,
Given [100, 4, 200, 1, 3, 2],

The longest consecutive elements sequence is [1, 2, 3, 4].  Return its
length: 4.

Challenging part of this question is that your algorithm should run in
O(n) complexity.


26. How to find minimum value in a rotated sorted vector?

This is  another advanced level  vector coding question and  you should
only attempt this one, once you have solved the easier ones.  Suppose a
sorted vector is rotated at some pivot unknown to you beforehand.

(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).

Find the minimum element.

You may assume no duplicate exists in the vector.  One follow-up
question of this question is What if duplicates are allowed? Would
this affect the run-time complexity? How and why?


27. Given an vector of of size n and a number k, find all elements that appear more than n/k times?

Another tough vector based coding questions from Interviews.  You are
given an vector of size n, find all elements in vector that appear more
than n/k times.  For example, if the input vectors is {3, 1, 2, 2, 1, 2,
3, 3} and k is 4, then the output should be [2, 3].  Note that size of
vector is 8 (or n = 8), so we need to find all elements that appear
more than 2 (or 8/4) times.  There are two elements that appear more
than two times, 2 and 3.

    1.  Returns the largest sum of contiguous integers in the vector
        Example: if the input is (-10, 2, 3, -2, 0, 5, -15), the largest sum is 8

    2.  Return the sum two largest integers in an vector

    3.  Given an vector of integers write a program that will determine
        if any two numbers add up to a specified number N.  Do this
        without using hash tables
|#


#|
28.  How to reverse vector in place in Lisp?

Now let's see one of the most frequently asked vector interview
question.  You need to write a program which accepts an integer vector
and your program needs to reverse that vector in place, which means
you cannot use additional buffer or vector, but one or two variables
will be fine.  Of course you cannot use NREVERSE or REVERSE to
directly solve this problem, you need to create your own logic.
|#


(defun vector-nreverse (vector)
  (when (plusp (length vector))
   (loop :for i :from 0
         :for j :from (1- (length vector)) :by -1
         :while (< i j)
         :do (rotatef (aref vector i) (aref vector j))))
  vector)

(assert (equalp (vector-nreverse (vector)) (vector)))
(assert (equalp (vector-nreverse (vector 1)) (vector 1)))
(assert (equalp (vector-nreverse (vector 1 2)) (vector 2 1)))
(assert (equalp (vector-nreverse (vector 1 2 3)) (vector 3 2 1)))
(assert (equalp (vector-nreverse (vector 1 2 3 4)) (vector 4 3 2 1)))


#|
29.  Difference between vector and linked list data structure?

This is a theoretical questions from phone interviews.  There are
several differences between vector and linked list e.g.  vector stores
element in contiguous memory location while linked list stores at
random places, this means linked list better utilizes the
places.  Consequently, its possible to have large linked list in
limited memory environment compare to vector of same size.  Advantage of
using vector is random access it provides if you know the index, while
in linked list you need to search an element by traversing which is
O(n) operation.


30.  How to check if vector contains a duplicate number?

This may look a repeated question because we have already done similar
question, but in this question, most from Lisp interviews, you need to
write a contains() like method from Collections, which returns true or
false if you pass an element and it is repeated or not.


|#
