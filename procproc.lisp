(ql:quickload 'read-csv)
(ql:quickload 'cl-fad)

(defconstant csv-file-delimiter #,)
(defparameter *my-path* #P"/home/henry/data-processing/data-to-add/")
(defparameter *test-file* #P"/home/henry/data-processing/data-to-add/designation.csv")
;;(defparameter *measurements-file* #P"/home/henry/data-processing/MEASUREMENTS_Wafer_Processing_Cleanroom_HeaderProvidedOnFirstRow.csv")
(defparameter *measurements-file* #P"/home/henry/processing-data/L/lisp-projects/data-processing/Measurements_Wafer_Processing_Cleanroom_HeaderProvidedOnFirstRow.csv")
;;(defparameter *tracking-file* #P"/home/henry/data-processing/TRACKING_Wafer_Processing_Cleanroom_HeaderProvidedOnFirstRow.csv")
(defparameter *tracking-file* #P"/home/henry/processing-data/L/lisp-projects/data-processing/tracking_Wafer_Processing_Cleanroom_HeaderProvidedOnFirstRow.csv")
(defparameter *path-of-data-to-add-csv-files* #P"/home/henry/data-processing/data-to-add/")
(defparameter *header-vector* (make-array 1 :fill-pointer 0 :adjustable t))

(defun csv-p (file-name)
  "Return the pathname when is has the extension csv and NIL otherwise."
  (if (equal (pathname-type file-name) "csv")
      file-name))

(defclass wafer ()
  ((wafer-name :accessor wafer-name
	       :initarg :wafer-name
	       :initform (error "Error initializing, wafer needs a name."))
   (wafer-alias :accessor wafer-alias
		:initarg :wafer-alias)
   (wafer-properties :accessor wafer-properties)
   (wafer-header :allocation :class
	   :accessor wafer-header)))

(defun combine-headers (csv-file-1 csv-file-2 delimiter)
  (let ((header-1 (read-header-make-vector csv-file-1 delimiter))
	(header-2 (read-header-make-vector csv-file-2 delimiter)))
    (remove "" (concatenate 'vector header-1 header-2) :test #'string=)))

(defun read-header-make-vector (filename delimiter)
  "Return the header in a csv file as a vector"
  (with-open-file (my-stream filename)
    (let ((data (read-csv:parse-csv my-stream delimiter)) 
	  (list-of-ids (make-array 1 :fill-pointer 0 :adjustable t)))
      (loop for elem in (car data) do
	   (vector-push-extend elem list-of-ids))
      list-of-ids)))

;; (setf *header-vector* (read-header-make-vector *test-file* #\;))
(setf *header-vector* (read-header-make-vector *measurements-file* csv-file-delimiter))
;; (position "ridge-height" *header-vector* :test #'string=)

(defun read-data (filename delimiter)
  "Return list of lists, where each inner list contains the data of one row"
  (let ((data (with-open-file (s filename)
		(read-csv:parse-csv s delimiter))))
    data))

;; (define-condition wafer-not-unique (error)
;;   ((text :initarg :text :reader :text)))

(defun check-uniqueness (item my-list)
  "check wether an item allready exists in a list,
if not return the item to the list and return the list"
  (if (member item my-list) 
      (format t "Item ~a allready exists" item)
      (return-from check-uniqueness item)))
  ;; (if (member item my-list)
  ;;     (progn (format t "Item ~a is in list" item)
  ;; 	     (error 'wafer-not-unique :text item))
  ;;     (setf my-list (adjoin item my-list))))

(defun vector<-list (my-list)
  (make-array (length my-list) :initial-contents my-list))

(defun parse-overview-file (filename delimiter)
  "For each line in the csv-file with filename, initiate a wafer object from wafer class"
  (setf (wafer-header (make-instance 'wafer :wafer-name 'DUMMY)) (read-header-make-vector filename delimiter))
  (let ((data (read-data filename delimiter))
	 (header (read-header-make-vector filename delimiter))
	 (list-of-wafer-objects)
	 (list-of-unique-wafers ()))
    ;; remove the first four lines from data
    (dotimes (my-var 4)
      (pop data))
    ;; loop over the entries of wafers
    (dolist (item data)
      (let* ((position-wafer-id (position "wafer-id" header :test #'string=)) ;; find the wafer-name
	     (current-wafer-id (elt item position-wafer-id))
	     (wafer-id-without-spaces-as-symbol))
;;	(print current-wafer-id)
	;; remove spaces from wafer-id
	(setf wafer-id-without-spaces-as-symbol (intern (remove #\Space current-wafer-id)))
	(if  (check-uniqueness wafer-id-without-spaces-as-symbol list-of-unique-wafers)
	     
	     (progn (push (make-instance 'wafer :wafer-name wafer-id-without-spaces-as-symbol) 
			  list-of-wafer-objects)
		    (push wafer-id-without-spaces-as-symbol list-of-unique-wafers)
		    (setf (wafer-properties (first list-of-wafer-objects)) 
			  (clean-vector-from-spaces(vector<-list item))))
	     (format t "Error wafer-name not unique~%"))))
    list-of-wafer-objects))

(defun clean-vector-from-spaces (vec)
  "remove empty cells consisting of just spaces"
  (substitute "" " " vec :test #'string=))

(defun print-overview-of-list-of-wafers (list-of-wafer-objects)
  (dolist (my-wafer-object list-of-wafer-objects)
    (format t "Wafer: ~a - ~a~%" (wafer-name my-wafer-object) (wafer-alias my-wafer-object))))

(defun set-wafer-alias (wafers)
  (dolist (wafer wafers)
    (setf (wafer-alias wafer) 
	  (intern (reverse (subseq (reverse (symbol-name (wafer-name wafer))) 0 4))))))

(defun find-csv-files (my-path)
  "Find all csv files in a given path"
  (let ((all-dirnames (cl-fad:list-directory my-path)) (selected-dir-names (list)))
    (dolist (f all-dirnames)
      (when (equal (pathname-type f) "csv")
	(push f selected-dir-names)))
    selected-dir-names))

(defun list<-vector (vec)
  (concatenate 'list vec))

(defun match-items-in-list-to-vector (list-1 vector-2)
  "Returns NIL if an item is in the first list but not in the second, return T otherwise"
  (dolist (item list-1)
    (when (not (member item (list<-vector vector-2)))
      (progn
	(format t "~a not in second list" item)
	(return-from match-items-in-list-to-vector nil))))
  T)

(defun add-data-to-wafer (wafers data-file)
  (let* ((data (read-data data-file #\;))
	 (header (pop data)))
    ;; check if the headers is found in the properties of the wafer
    (match-items-in-list-to-vector header (wafer-header (first wafers)))
    ;; loop lines
    ;; find wafer
    ;; find right spot in properties of wafer-object
    (print header)))


(defun main ()
  (defparameter *wafers* (parse-overview-file *measurements-file* csv-file-delimiter))
  (set-wafer-alias *wafers*)
  (print-overview-of-list-of-wafers *wafers*)
  (defparameter *first-file* (first (find-csv-files *path-of-data-to-add-csv-files*)))
  (add-data-to-wafer *wafers* *first-file*))

        ;; (if (member wafer-id-without-spaces-as-symbol list-of-unique-wafers)
	;;     (format t "This wafer-id: ~a was allready used.~%" wafer-id-without-spaces-as-symbol)(setf list-of-unique-wafers (adjoin wafer-id-without-spaces-as-symbol list-of-unique-wafers)))
        ;; ;; make sure the wafer-id is unique until so far
;;	(push (make-instance 'wafer :wafer-name (intern current-wafer-id)) list-of-wafer-objects)
    ;; make the wafer-object
    ;; fill the properties of the wafer-object

;; (defun parse-overview-file (filename delimiter)
;;   "For each line in the csv-file with filename, initiate a wafer object from wafer class"
;;   (let ((data (read-data filename delimiter))
;; 	(header (read-header-make-vector filename delimiter))
;; 	(list-of-wafer-objects)
;; 	(list-of-unique-wafers ()))
;;     ;; remove the first four lines from data
;;     (dotimes (my-var 4)
;;       (pop data))
;;     ;; loop over the entries of wafers
;;     (dolist (item data)
;;     ;; find the wafer-name
;;       (let* ((position-wafer-id (position "wafer-id" header :test #'string=))
;; 	     (current-wafer-id (elt item position-wafer-id))
;; 	     (wafer-id-without-spaces-as-symbol))
;; ;;	(print current-wafer-id)
;; 	;; remove spaces from wafer-id
;; 	(setf wafer-id-without-spaces-as-symbol (intern (remove #\Space current-wafer-id)))
;; 	(if  (check-uniqueness wafer-id-without-spaces-as-symbol list-of-unique-wafers)
	     
;; 	     (progn (push (make-instance 'wafer :wafer-name wafer-id-without-spaces-as-symbol) list-of-wafer-objects)
;; 		    (push wafer-id-without-spaces-as-symbol list-of-unique-wafers)
;; 		    (setf (wafer-properties (first list-of-wafer-objects)) item)
;; ;;		    (setf (wafer-properties wafer-id-without-spaces-as-symbol) 'zomaar)
;; 		    )
;; 	     (format t "Error wafer-name not unique~%"))))
;;     list-of-wafer-objects))

















