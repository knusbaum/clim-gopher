(asdf:defsystem #:clim-gopher
  :name "clim-gopher"
  :description "CLIM Gopher client GUI"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:mcclim #:split-sequence #:iolib #:quri
                        #:trivial-open-browser #:split-sequence
                        #:trivial-mimes)
  :components ((:file "package")
               (:file "icons"
                      :depends-on ("package"))
               (:file "protocol"
                      :depends-on ("package"))
               (:file "bookmarks"
                      :depends-on ("package"))
               (:file "presentation"
                      :depends-on ("package"))
               (:file "clim-gopher"
                      :depends-on ("package"
                                   "protocol"
                                   "icons"
                                   "bookmarks"
                                   "presentation"))))
