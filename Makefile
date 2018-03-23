

package: clim-gopher.tgz

clim-gopher.tgz: gopher
	mkdir -p clim-gopher/res
	cp gopher clim-gopher/
	cp launch-gopher.sh clim-gopher/
	cp gopher-view.sh clim-gopher/
	cp res/*.png clim-gopher/res/
	cp res/bookmarks.dat clim-gopher/res/
	tar -cvzf clim-gopher.tgz clim-gopher

gopher: *.lisp build-exe.lsh
	./build-exe.lsh
