

package: clim-gopher.tgz

clim-gopher.tgz: gopher
	mkdir -p clim-gopher/res
	cp gopher clim-gopher/
	cp res/*.png clim-gopher/res/
	tar -cvzf clim-gopher.tgz clim-gopher

gopher: *.lisp
	./build-exe.lsh
