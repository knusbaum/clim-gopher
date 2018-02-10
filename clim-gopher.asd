(asdf:defsystem #:clim-gopher
  :name "clim-gopher"
  :description "CLIM Gopher client GUI"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:mcclim #:cl-gopher #:split-sequence #:iolib #:quri #:trivial-open-browser)
  :components ((:file "clim-gopher-package")
               (:file "clim-gopher-icons"
                      :depends-on ("clim-gopher-package"))
               (:file "clim-gopher"
                      :depends-on ("clim-gopher-package"))))

