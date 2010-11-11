;;;;;; Hunchentoot based web application to track weight and
;      plot graph described in a hacker's diet.
;

(defun load-systems ()
  (quicklisp:quickload "hunchentoot")
  (quicklisp:quickload "cl-who")
  (quicklisp:quickload "parenscript")
  ;(quicklisp:quickload "cl-sql")
  ;(quicklisp:quickload "html-template"))

(defpackage :my-web
  (:use :hunchentoot :cl-who :parenscript 
   :cl :cl-user))
(defmacro intopack (pack)
 `(in-package ,pack))
(intopack :my-web)

;;; Created a package. Now need to define functions.
(defmacro my-standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" 
            :xml\:lang "en"
            (:head
             (:meta :http-equiv "Content type"
                    :content "text/html;charset=utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/css/stylesheet_default.css"))
            (:body ,@body))))


;(hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242))
(defvar *user-database* (make-hash-table :test #'equal))
(defstruct user
         login
         email
         password
         weight)
(defun add-user (database email password &key login )
  (let ((user (make-user :login login
                         :email email
                         :password password
                         :weight nil)))
    (if (not (gethash (user-email user) database ))
        (push user (gethash (user-email user) database)))))
(defun get-user (database email)
  (gethash email database))


(defun index-page (&key email password)
  (cond ((and email (login? email password)) 
         (my-standard-page (:title (str email))
           (:p "You can enter weight in the box given")
           (:form :action "/add-weight" 
                  (:p "Weight: " (:input :type 'string :name "weight" :value "")
                      (:input :type "submit")))))
                  
        (email (my-standard-page (:title "Error in login")
                 (:p "Oops, looks like you have forgotten your password,")
                 (:p "If you have already registered to weight watch, please try with a different password, or register yourself")))
        ((and *session* (user-email (session-value :user))) 
         (my-standard-page (:title (str (user-email(session-value :user)  )))
           (:p "You can enter weight in the box given")
           (:form :action "/add-weight"
                  (:p "Weight: " (:input :type 'string :name "weight" :value "")
                      (:input :type "submit")))))     
        (t (my-standard-page (:title "Weight watch login")
             (:p "Welcome to weight watch")
             (:p "Login to weight watch if you have already registered")
             (:form 
              "Email: " (:input :type "string" :name "email" :value "")
              (:p "Password: " (:input :type "password" :name "password" :value "")
                  (:input :type "submit")))
             (:p "You can also register to weight watch")
             (:form 
              :action "/register" :method "POST"
              "Email: " (:input :type "string" :name "email" :value "")
              (:p "Password: " (:input :type "password" :name "password" :value ""))
              (:p "Confirm Password: " (:input :type "password" :name "confirmpassword" :value ""))    
              (:input :type "submit"))))))
(define-easy-handler (add-weight :uri "/add-weight")
    ((weight :parameter-type 'string))
  (if (stringp weight)
      (let ((w (read-from-string weight)))
        (if (numberp w)
            (progn 
              (push (make-weight-data :date (get-universal-time) :weight w)
                    (user-weight (session-value :user)))
              (redirect "/index"))
                   
            (my-standard-page (:title "Error adding weight")
              (:p "Please enter only number in weight box"))))))
(define-easy-handler (index :uri "/index")
           ((email :parameter-type 'string)
            (password :parameter-type 'string))
         (index-page :email email :password password))