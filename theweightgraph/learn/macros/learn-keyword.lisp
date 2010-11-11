;;; Was trying to build a standard page macro, which creates
                                        ;   a standard HTML page, given title and body.
                                        ;   the macro was defined like this.
                                        ;

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
;; It is working, I just did not know how to use


; but there was some problem with keyword argument, which caused a syntax error

