#|
  This file is a part of cl-coroutine project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coroutine
  (:use :cl :cl-cont)
  (:export :defcoroutine
           :yield
           :coexit
           :make-coroutine
           :with-coroutine
	   :this-coro
	   )
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-coroutine)


;;;
;;; DEFCOROUTINE macro
;;;

(defmacro defcoroutine (name (&optional arg) &body body)
  (if arg
      (defcoroutine-util name body arg)
      (defcoroutine-util name body)))

(defun coroutine-underlying-lambda (name body &optional (arg nil arg-p))
  (with-gensyms (cont it coro)
    (let ((name (or name (gensym "CORO-NAME"))))
      `(let (,cont)
	 (let ((,coro #'(lambda ,(if arg-p `(,arg))
			(if ,cont
			    (funcall ,cont ,@(if arg-p `(,arg)))
			    (cl-cont:with-call/cc
			      (macrolet ((yield (&optional result)
					   (with-gensyms (cc g!-res)
					     ,(if arg-p
						  ``(setf ,',arg
							  (cl-cont:let/cc ,cc
							    (let ((,g!-res ,result))
							      (setf ,',cont ,cc)
							      ,g!-res)))
						  ``(cl-cont:let/cc ,cc
						      (let ((,g!-res ,result))
							(setf ,',cont ,cc)
							,g!-res)))))
					 (coexit (&optional result)
					   (with-gensyms (g!-res)
					     `(cl-cont:let/cc _
						(declare (ignorable _))
						(let ((,g!-res ,result))
						  (setf ,',cont
							#'(lambda (_)
							    (declare (ignorable _))
							    (values)))
						  ,g!-res))))
					 (,name () ',cont))
				;; A KLUDGE to make CONT be always set inside BODY,
				;; so the FLET above works also at the "first" invocation of the coroutine
				(let ((,it (yield nil)))
				  (declare (ignore ,it))
				  ,@body)
				(coexit nil)))))))
	   (funcall ,coro ,@(if arg-p `(nil)))
	   ,coro)))))
  
(defun defcoroutine-util (name body &optional (arg nil arg-p))
  `(progn
     (setf (get ',name 'make-coroutine)
	   #'(lambda ()
	       ,(if arg-p
		    (coroutine-underlying-lambda name body arg)
		    (coroutine-underlying-lambda name body))))
     ',name))

(defmacro lambda-coro (arg &body body)
  (with-gensyms (g!-name)
    (if arg
	(coroutine-underlying-lambda g!-name body (car arg))
	(coroutine-underlying-lambda g!-name body))))

;; OK, this is clearly not the final form of this macro... but let's start somewhere
(defmacro label-coro (name arg &body body)
  (if arg
      (coroutine-underlying-lambda (or name 'this-coro) body (car arg))
      (coroutine-underlying-lambda (or name 'this-coro) body)))


;;;
;;; MAKE-COROUTINE function
;;;

(defun make-coroutine (name)
  (let ((func (get name 'make-coroutine)))
    (unless func
      (error "The coroutine ~S is undefined." name))
    (funcall func)))


;;;
;;; WITH-COROUTINE macro
;;;

(defmacro with-coroutine ((name) &body body)
  (with-gensyms (coroutine)
    `(let ((,coroutine (make-coroutine ',name)))
       (macrolet ((,name (&rest args)
                     `(funcall ,',coroutine ,@args)))
         ,@body))))
