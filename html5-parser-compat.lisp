(in-package :html5-parser)

#+(or)
(defmethod xpath-protocol:node-p-using-navigator (navigator (node node))
  T)

(defmethod xpath-protocol:parent-node-using-navigator (navigator (node node))
  (declare (ignore navigator))
  (node-parent node))

(defmethod xpath-protocol:child-pipe-using-navigator (navigator (node node))
  (declare (ignore navigator))
  (xpath::list->pipe (%node-child-nodes node)))

(defmethod xpath-protocol:node-type-p-using-navigator (navigator (node node) type)
  (declare (ignore navigator))
  (ecase type
    (:comment (typep node 'comment-node))
    (:processing-instruction NIL)
    (:text (typep node 'text-node))
    (:attribute NIL)
    (:element (typep node 'element))
    (:namespace NIL)
    (:document (typep node 'document))))

(defmethod xpath-protocol:namespace-uri-using-navigator (navigator (node node))
  (declare (ignore navigator))
  (or (node-namespace node) ""))

(defmethod xpath-protocol:local-name-using-navigator (navigator (node node))
  (declare (ignore navigator))
  (node-name node))

(defmethod xpath-protocol:node-text-using-navigator (navigator (attribute html5-attribute))
  (declare (ignore navigator))
  (html5-attribute-value attribute))

(defmethod xpath-protocol:node-text-using-navigator (navigator (node node))
  (with-output-to-string (stream)
    (mapc (lambda (node) (write-string (xpath-protocol:node-text-using-navigator navigator node) stream)) (%node-child-nodes node))))

(defmethod xpath-protocol:node-text-using-navigator (navigator (node text-node))
  (node-value node))

(defstruct html5-attribute
  local-name
  uri
  value)

(defstruct html5-namespace
  parent
  uri)

#+(or)
(defmethod xpath-protocol:node-p-using-navigator (navigator (attribute html5-attribute))
  T)

(defmethod xpath-protocol:attribute-pipe-using-navigator (navigator (element element))
  (declare (ignore navigator))
  (xpath::list->pipe
   (mapcar (lambda (attribute)
             (make-html5-attribute :local-name (caar attribute)
                                   :uri (cdar attribute)
                                   :value (cdr attribute)))
           (%node-attributes element))))

(defmethod xpath-protocol:node-type-p-using-navigator (navigator (attribute html5-attribute) type)
  (declare (ignore navigator))
  (eq type :attribute))

(defmethod xpath-protocol:namespace-uri-using-navigator (navigator (attribute html5-attribute))
  (declare (ignore navigator))
  (or (html5-attribute-uri attribute) ""))

(defmethod xpath-protocol:local-name-using-navigator (navigator (attribute html5-attribute))
  (declare (ignore navigator))
  (html5-attribute-local-name attribute))

(defmethod xpath-protocol:node-text-using-navigator (navigator (attribute html5-attribute))
  (declare (ignore navigator))
  (html5-attribute-value attribute))
