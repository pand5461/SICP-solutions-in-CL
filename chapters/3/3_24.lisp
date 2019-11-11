;;;; Chapter 3.3.3           
;; Exercise 3.24
(defun make-table (test)
  (let ((local-table (list '*table*)))
    (labels
        ((lookup (key-1 key-2)
           (let ((subtable (assoc key-1 (cdr local-table) :test test)))
             (if subtable
                 (let ((record (assoc key-2 (cdr subtable) :test test)))
                   (if record
                       (cdr record)
                       nil))
                 nil)))
         
         (insert! (key-1 key-2 value)
           (let* ((table-data (cdr local-table))
                  (subtable (assoc key-1 table-data :test test)))
             (if subtable
                 (let* ((sub-data (cdr subtable))
                        (record (assoc key-2 sub-data :test test)))
                   (if record
                       (setf (cdr record) value)
                       (setf (cdr subtable)
                             (cons (cons key-2 value)
                                   sub-data))))
                 (setf (cdr local-table)
                       (cons (list key-1
                                   (cons key-2 value))
                             table-data))))
           'ok)
         
         (dispatch (m)
           (cond
             ((eq m 'lookup-proc) #'lookup)
             ((eq m 'insert-proc!) #'insert!)
             (t (error "Unknown operation ~a: TABLE" m)))))
      
      #'dispatch)))

;; Exercise 3.25
(defun make-multitable (test)
  (let ((local-table (list '*table*)))
    (labels
        ((make-subtable (name)
           (list name))
         
         (lookup (keys table)
           (if (null keys)
               (cdr table)
               (let* ((key-1 (car keys))
                      (other-keys (cdr keys))
                      (subtable (assoc key-1 (cdr table) :test test)))
                 (if subtable
                     (lookup other-keys subtable)
                     nil))))
         
         (insert! (keys value table)
           (if (null keys)
               (error "Cannot insert a value without a key: MULTITABLE")
               (let* ((table-data (cdr table))
                      (key-1 (car keys))
                      (other-keys (cdr keys))
                      (subtable (assoc key-1 table-data :test test)))
                 (if subtable
                     (if (null other-keys)
                         (setf (cdr subtable) value)
                         (insert! other-keys value subtable))
                     (if (null other-keys)
                         (setf (cdr table)
                               (cons (cons key-1 value)
                                     table-data))
                         (let ((new-subtable (make-subtable key-1)))
                           (insert! other-keys value new-subtable)
                           (setf (cdr table)
                                 (cons new-subtable table-data)))))))
               'ok)
         
         (dispatch (m)
           (cond
             ((eq m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
             ((eq m 'insert-proc!) (lambda (keys val)
                                     (insert! keys val local-table)))
             (t (error "Unknown operation ~a: TABLE" m)))))
      #'dispatch)))
