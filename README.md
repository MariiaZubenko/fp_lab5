<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Зубенко Марія Олексіївна КВ-11</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 8
База даних: Наукові статті
Тип записів: Геш-таблиця
## Лістинг реалізації завдання
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Вміст тестових файлів
article.csv
```
ID,Specialty,Article
1,Psychology,Personal boundaries at work
2,Psychology,How to get rid of stress
3,Chemistry,Machine learning methods for chemistry
4,Chemistry,Chemical water purification
5,Statistics,Average job satisfaction rates
6,Statistics,The law of growth of market information
7,Mathematics,Mathematical Foundations of Machine Learning
8,Mathematics,Innovations in Computational Algebra
```
specialty.csv
```
ID,Specialty
1,Psychology
2,Chemistry
3,Statistics
4,Mathematics
```
### Тестування
```lisp
(test-read-csv)

Tables from article.csv:

ID             SPECIALTY      ARTICLE
1              Psychology     Personal boundaries at work
2              Psychology     How to get rid of stress
3              Chemistry      Machine learning methods for chemistry
4              Chemistry      Chemical water purification
5              Statistics     Average job satisfaction rates
6              Statistics     The law of growth of market information
7              Mathematics    Mathematical Foundations of Machine Learning
8              Mathematics    Innovations in Computational Algebra

Tables from specialty.csv:

ID             SPECIALTY
1              Psychology
2              Chemistry
3              Statistics
4              Mathematics

Article with id 3:

ID             SPECIALTY      ARTICLE
3              Chemistry      Machine learning methods for chemistry

Scientific articles for the specialty Chemistry:

ID             SPECIALTY      ARTICLE
3              Chemistry      Machine learning methods for chemistry
4              Chemistry      Chemical water purification

(test-write-to-csv)

Data that will be written to the psychology_article.csv file:

ID             SPECIALTY      ARTICLE
1              Psychology     Personal boundaries at work
2              Psychology     How to get rid of stress

Tables from psychology_article.csv:

ID             SPECIALTY      ARTICLE
1              "Psychology"   "Personal boundaries at work"
2              "Psychology"   "How to get rid of stress"


(test-hash-table-to-alist)
The result is correct: ((ID . 8) (SPECIALTY . Mathematics)
                        (ARTICLE . Innovations in Computational Algebra))

```



