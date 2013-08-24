;;;; cepl-classimp.lisp

(in-package #:cepl-classimp)

(defmacro assimp-listify (name pointer struct-type wrapping-type)
  (let ((thing (utils:symb-package '%ai 'm- name))
        (thing-count (utils:symb-package '%ai 'm-num- name))
        (s-type (utils:symb-package '%ai struct-type)))
    `(ai::with-foreign-slots* ((,thing ,thing-count) ,pointer
                               (:struct ,s-type))
       (loop :for i :below ,thing-count :collect
          (make-instance ',wrapping-type :pointer (mem-aref ,thing :pointer i))))))

(defmacro assimp-arrayify (name pointer struct-type dimensions cepl-type)
  (let ((thing (utils:symb-package '%ai 'm- name))
        (s-type (utils:symb-package '%ai struct-type)))
    `(ai::with-foreign-slots* ((,thing) ,pointer (:struct ,s-type))
       (when (not (or (null ,thing) (null-pointer-p ,thing)))
         (cgl:make-c-array-from-pointer 
          ,(if (listp dimensions) dimensions `(list ,dimensions)) 
          ,cepl-type ,thing)))))

(defclass scene ()
  ((pointer :initarg :pointer :accessor pointer)
   (meshes :initarg :meshes :reader meshes)
   (materials :initarg :materials :reader materials)
   (animations :initarg :animations :reader animations)
   (textures :initarg :textures :reader textures)
   (lights :initarg :lights :reader lights)
   (cameras :initarg :cameras :reader cameras)
   (root :initarg :root :reader root)))

(defmethod print-object ((object scene) stream)
  (with-slots (meshes materials animations textures lights cameras) object
    (format stream "#<Scene:~@[ mesh:~a~]~@[ mat:~a~]~@[ anim:~a~]~@[ tex:~a~]~@[ light:~a~]~@[ cam:~a~]>"
            (when meshes (length meshes)) (when materials (length materials)) 
            (when animations (length animations)) 
            (when textures (length textures)) (when lights (length lights))
            (when cameras (length cameras)))))

(defclass mesh () 
  ((pointer :initarg :pointer :reader pointer)
   (name :initarg :name :reader name)
   (faces :initform nil :reader faces)
   (tex-coords :initform nil :reader tex-coords)))

(defmethod initialize-instance :after ((object mesh) &key)
  (setf (slot-value object 'name) 
        (ai::translate-ai-string 
         (foreign-slot-pointer (pointer object)
                               '(:struct %ai:ai-mesh) '%ai:m-name))))

(defmethod verts ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh)
                                    '%ai:m-num-vertices)))
    (assimp-arrayify vertices ptr ai-mesh v-len :vec3)))

(defmethod normals ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh)
                                    '%ai:m-num-vertices)))
    (assimp-arrayify normals ptr ai-mesh v-len :vec3)))

(defmethod tangents ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh)
                                    '%ai:m-num-vertices)))
    (assimp-arrayify tangents ptr ai-mesh v-len :vec3)))

(defmethod bitangents ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh)
                                    '%ai:m-num-vertices)))
    (assimp-arrayify bitangents ptr ai-mesh v-len :vec3)))
 
(defmethod colors ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh) 
                                    '%ai:m-num-vertices)))
    (ai::with-foreign-slots* ((%ai:m-colors) ptr (:struct %ai:ai-mesh))
      (loop :for i :below %ai::+ai-max-number-of-color-sets+ :append
         (let ((p (cffi:mem-aref %ai:m-colors :pointer i)))
           (unless (or (null p) (null-pointer-p p))
             (list (assimp-arrayify colors p ai-mesh (list v-len) :vec4))))))))

(defmethod faces ((object mesh))
  (let* ((ptr (pointer object)))
    (ai::with-foreign-slots* ((%ai:m-faces %ai:m-num-faces) 
                              ptr (:struct %ai:ai-mesh))
      (let ((f-len %ai:m-num-faces)
            (p %ai:m-faces)
            (size (foreign-type-size '(:struct %ai:ai-face))))         
        (unless (or (null-pointer-p p) (null p))
          (loop :for i :below f-len :collect
             (let ((f-p (inc-pointer p (* size i))))
               (ai::with-foreign-slots* ((%ai:m-num-indices %ai:m-indices)
                                         f-p (:struct %ai:ai-face))
                 (loop :for j :below %ai:m-num-indices :collect
                    (mem-aref f-p :unsigned-int j))))))))))

(defmethod tex-coords ((object mesh))
  (let* ((ptr (pointer object))
         (v-len (foreign-slot-value ptr '(:struct %ai:ai-mesh) 
                                    '%ai:m-num-vertices)))
    (ai::with-foreign-slots* ((%ai:m-texture-coords) ptr (:struct %ai:ai-mesh))
      (loop :for i :below %ai::+ai-max-number-of-texturecoords+ :append
         (let ((p (cffi:mem-aref %ai:m-texture-coords :pointer i)))
           (unless (or (null p) (null-pointer-p p))
             (list (assimp-arrayify texture-coords p ai-mesh 
                                    (list v-len) :vec3))))))))

;;-------------------------------------------------------------

(defclass material () 
  ((pointer :initarg :pointer :accessor pointer)))

(defclass texture () 
  ((pointer :initarg :pointer :accessor pointer)))

(defclass animation () 
  ((pointer :initarg :pointer :accessor pointer)))

(defclass light () 
  ((pointer :initarg :pointer :accessor pointer)))

(defclass camera () 
  ((pointer :initarg :pointer :accessor pointer)))

(defclass node () ((pointer :initarg :pointer :accessor pointer)))


;; [TODO] (m-flags :unsigned-int)
(defun import-asset (filename &rest flags)
  (let ((pointer (apply #'ai:import-file-raw (cons filename flags))))
    (if (or (null pointer) (null-pointer-p pointer))
        (error "failed to load asset")
        (make-instance 
         'scene
         :pointer pointer
         :meshes (assimp-listify meshes pointer ai-scene mesh)
         :materials (assimp-listify materials pointer ai-scene material)
         :animations (assimp-listify animations pointer ai-scene animation)
         :textures (assimp-listify textures pointer ai-scene texture)
         :lights (assimp-listify lights pointer ai-scene light)
         :cameras (assimp-listify cameras pointer ai-scene camera)
         :root (make-instance 'node :pointer (foreign-slot-value 
                                              pointer '(:struct %ai:ai-scene)
                                              '%ai:m-root-node))))))





