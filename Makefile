

package: clim-gopher.tgz

clim-gopher.tgz: gopher
	mkdir -p clim-gopher
	cp README.md clim-gopher/
	cp LICENSE.md clim-gopher/
	cp gopher clim-gopher/
	tar -cvzf clim-gopher.tgz clim-gopher

gopher: *.lisp build-exe.lsh
	./build-exe.lsh

clean:
	-@rm -rf clim-gopher clim-gopher.tgz gopher
