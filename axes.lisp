(in-package :xpath)

;; axes


#|
FIXME: need to implement all axes

[6]   	AxisName	   ::=   	'ancestor'	
			| 'ancestor-or-self'	
			| 'attribute'	
			| 'child'	
			| 'descendant'	
			| 'descendant-or-self'	
			| 'following'	
			| 'following-sibling'	
			| 'namespace'	
			| 'parent'	
			| 'preceding'	
			| 'preceding-sibling'	
			| 'self'	
|#


(defmacro define-axis (name (ordering &optional (principal-node-type :element)) &body body)
  (let ((func-name (hypsym 'axis name)))
    `(progn
       (defun ,func-name (node) ,@body)
       (setf (get ',name 'axis-function) ',func-name
	     (get ',name 'axis-ordering) ',ordering
             (get ',name 'axis-principal-node-type) ',principal-node-type))))

(defun axis-function (axis) (get axis 'axis-function))

(defun axis-properties (axis)
  (values
   (get axis 'axis-function)
   (get axis 'axis-ordering)))

(defun axis-principal-node-type (axis) (get axis 'axis-principal-node-type))

(define-axis :child (:document-order) (xpath-protocol:child-pipe node))

(define-axis :self (:document-order) (list node))

(define-axis :parent (:reverse-document-order) (parent-pipe node))

(define-axis :descendant-or-self (:document-order)
  (make-pipe node
	     (mappend-pipe (axis-function :descendant-or-self)
			   (xpath-protocol:child-pipe node))))

;; internal helper axis
(define-axis reverse-descendant-or-self (:reverse-document-order)
  (append-pipes (mappend-pipe (axis-function 'reverse-descendant-or-self)
			      (reverse
			       (force (xpath-protocol:child-pipe node))))
		(list node)))

(define-axis :descendant (:document-order)
  (mappend-pipe (axis-function :descendant-or-self)
                (xpath-protocol:child-pipe node)))

(define-axis :following-sibling (:document-order)
  (unless (null (xpath-protocol:parent-node node))
    (subpipe-after node (xpath-protocol:child-pipe
			 (xpath-protocol:parent-node node)))))

(define-axis :preceding-sibling (:reverse-document-order)
  (let ((parent (xpath-protocol:parent-node node)))
    (if parent
	(nreverse (force (subpipe-before node (xpath-protocol:child-pipe
					       parent))))
	empty-pipe)))

;; FIXME: test
;; FIXME: order

(define-axis :following (:document-order)
  (mappend-pipe (axis-function :descendant-or-self)
		(mappend-pipe (axis-function :following-sibling)
			      (funcall (axis-function :ancestor-or-self)
				       node))))

(define-axis :preceding (:reverse-document-order)
  (mappend-pipe (axis-function 'reverse-descendant-or-self)
		(mappend-pipe (axis-function :preceding-sibling)
			      (funcall (axis-function :ancestor-or-self)
				       node))))

(define-axis :ancestor (:reverse-document-order)
  (mappend-pipe (axis-function :ancestor-or-self)
                (parent-pipe node)))

(define-axis :ancestor-or-self (:reverse-document-order)
  (make-pipe node
	     (mappend-pipe (axis-function :ancestor-or-self)
			   (parent-pipe node))))

(define-axis :attribute (:document-order :attribute)
  (xpath-protocol:attribute-pipe node))

(define-axis :namespace (:document-order :namespace)
  (xpath-protocol:namespace-pipe node))

;; FIXME: This is a pseudo-axis used to implement absolute paths.
;; Is this the right approach?
(define-axis :root (:reverse-document-order)
  (loop
     for this = node then parent
     for parent = (xpath-protocol:parent-node this)
     while parent
     finally (return (list this))))
