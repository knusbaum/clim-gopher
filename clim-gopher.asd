(asdf:defsystem #:clim-gopher
  :name "clim-gopher"
  :description "CLIM Gopher client GUI"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:mcclim #:cl-gopher #:split-sequence #:iolib #:quri)
  :components ((:file "clim-gopher-package")
               (:file "clim-gopher"
                      :depends-on ("clim-gopher-package"))))

