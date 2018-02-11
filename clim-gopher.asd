(asdf:defsystem #:clim-gopher
  :name "clim-gopher"
  :description "CLIM Gopher client GUI"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:mcclim #:split-sequence #:iolib #:quri
                        #:trivial-open-browser #:split-sequence
                        #:trivial-mimes)
  :components ((:file "clim-gopher-package")
               (:file "clim-gopher-icons"
                      :depends-on ("clim-gopher-package"))
               (:file "clim-gopher-protocol"
                      :depends-on ("clim-gopher-package"))
               (:file "clim-gopher-bookmarks"
                      :depends-on ("clim-gopher-package"))
               (:file "presentation"
                      :depends-on ("clim-gopher-package"))
               (:file "clim-gopher"
                      :depends-on ("clim-gopher-package"
                                   "clim-gopher-protocol"
                                   "clim-gopher-icons"
                                   "clim-gopher-bookmarks"
                                   "presentation"))))
