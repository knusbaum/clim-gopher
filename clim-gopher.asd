(asdf:defsystem #:clim-gopher
  :name "clim-gopher"
  :description "CLIM Gopher client GUI"
  :license "BSD 2-Clause"
  :author "Kyle Nusbaum"
  :depends-on (#:mcclim #:split-sequence #:usocket #:flexi-streams #:quri
                        #:trivial-open-browser #:split-sequence
                        #:trivial-mimes #:cl-gopher)
  :components ((:file "package")
               (:file "icons"
                      :depends-on ("package"))
               (:file "bookmarks"
                      :depends-on ("package"))
               (:file "presentation"
                      :depends-on ("package"))
               (:file "clim-gopher"
                      :depends-on ("package"
                                   "icons"
                                   "bookmarks"
                                   "presentation"))))
