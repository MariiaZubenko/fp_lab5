(defun read-csv (path hash-table key)
  (with-open-file (stream path :direction :input)
    (let ((header (split-string-standard (read-line stream))))
      (loop for line = (read-line stream nil 'eof)
            until (eq line 'eof)
            do (let* (
                      (fields (split-string-standard line #\,))
                      (record (make-hash-table))
                      (id (parse-integer (first fields)))
                      )
                 (case key
                   (:article
                    (setf (gethash :id record) id
                          (gethash :specialty record) (trim-whitespace (second fields))
                          (gethash :article record) (trim-whitespace (third fields))))
                   (:specialty
                    (setf (gethash :id record) id
                          (gethash :specialty record) (trim-whitespace (second fields))))
                   (t (error "Unknown key: ~A" key)))
                 (setf (gethash id hash-table) record))))))

(defun split-string-standard (string &optional (separator ","))
  "Splits a STRING into substrings using SEPARATOR."
  (let ((start 0)
        (result nil))
    (loop for end = (position separator string :start start)
          while end
          do (push (subseq string start end) result)
             (setf start (1+ end)))
    (push (subseq string start) result)
    (nreverse result)))


(defun trim-whitespace (string)
  "Delete not needed symbols"
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun select (path key &rest filters)
  (lambda (&rest filters)
    (let* ((hash-table (make-hash-table :test #'equal))
           (result '()))

      (read-csv path hash-table key)

      (if (null filters)
          (maphash (lambda (_ value)
                     (push value result))
                   hash-table)
        (let ((filter-hash (make-hash-table :test #'equal)))
          (loop for (filter-key filter-value) on filters by #'cddr
                do (setf (gethash filter-key filter-hash) filter-value))

          (maphash (lambda (_ value)
                     (let ((matches t))
                       (maphash (lambda (filter-key filter-value)
                                  (unless (equal (gethash filter-key value)
                                                 filter-value)
                                    (setf matches nil)))
                                filter-hash)
                       (when matches
                         (push value result))))
                   hash-table)))

      (reverse result))))

(defun write-to-csv (path hash-tables)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let* ((keys (loop for k being the hash-keys of (first hash-tables) collect k))
           (header (mapcar #'string keys)))
      (format stream "~{~a~^,~}~%" header)
      (dolist (hash-table hash-tables)
        (let ((values (mapcar (lambda (key)
                                (let ((value (gethash key hash-table)))
                                  (if value 
                                      (write-to-string value)
                                      "")))
                              keys)))
          (format stream "~{~a~^,~}~%" values))))))

(defun hash-table-to-alist (hash-table)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

(defun print-hash-table (hash-tables)
  (let ((field (let ((keys '()))
                 (maphash (lambda (key value) (push key keys))
                          (first hash-tables))
                 (reverse keys))))
    (format t "~%")
    (format t "~{~15A~}" (mapcar #'symbol-name field))
    (format t "~%")
    (dolist (table hash-tables)
      (let ((values (mapcar (lambda (key) (gethash key table)) field)))
        (format t "~{~15A~}" values)
        (format t "~%")))))

(defun test-read-csv ()
  (format t "~%Tables from article.csv:~%")
  (print-hash-table (funcall (select "lab5/article.csv" :article)))

  (format t "~%Tables from specialty.csv:~%")
  (print-hash-table (funcall (select "lab5/specialty.csv" :specialty)))

  (format t "~%Article with id 3:~%")
  (print-hash-table (funcall (select "lab5/article.csv" :article) :id 3))

  (format t "~%Scientific articles for the specialty Chemistry:~%")
  (print-hash-table (funcall (select "lab5/article.csv" :article) :specialty "Chemistry")))

(defun test-write-to-csv ()
  (format t "~%Data that will be written to the psychology_article.csv file:~%")
  (print-hash-table (funcall (select "lab5/article.csv" :article) :specialty "Psychology"))

  (write-to-csv "lab5/psychology_article.csv" (funcall (select "lab5/article.csv" :article) :specialty "Psychology"))

  (format t "~%Tables from psychology_article.csv:~%")
  (print-hash-table (funcall (select "lab5/psychology_article.csv" :article))))

(defun test-hash-table-to-alist ()
  (let* ((selected-articles (funcall (select "lab5/article.csv" :article) :id 8))
         (test-hash-table (first selected-articles))
         (expected-alist '((:id . 8)
                           (:specialty . "Mathematics")
                           (:article . "Innovations in Computational Algebra"))))
    (if test-hash-table
        (let ((generated-alist (hash-table-to-alist test-hash-table)))
          (if (equal expected-alist generated-alist)
              (format t "The result is correct: ~a~%" generated-alist)
              (format t "The result is not correct. ~%Generated: ~a~%Expected:  ~a~%" generated-alist expected-alist))))))

(test-read-csv)
(test-write-to-csv)
(test-hash-table-to-alist)