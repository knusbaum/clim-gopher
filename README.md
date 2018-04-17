# CLIM-GOPHER

Browse Gopherspace in Common Lisp.

![Gopher Browser](https://raw.githubusercontent.com/knusbaum/clim-gopher/master/screencap.png)

## Requirements

For html links to work properly `xdg-utils` must be installed on linux.

This application also depends on the library cl-gopher, available at [https://github.com/knusbaum/cl-gopher](https://github.com/knusbaum/cl-gopher)

It also depends on various libraries available through [quicklisp](https://www.quicklisp.org/beta/)

### To Run

Make sure cl-gopher and clim-gopher are in a location that quicklisp can find them (for instance, `~/quicklisp/local-projects`)

Then execute the following:
```
(ql:quickload 'clim-gopher)
(clim-gopher:browser)
```

#### Controls:

Clicking items should be the main method for exploration. A left click on an item will navigate to that item.

Menu items exist at the top of the browser:
* `Back` and `Refresh` should work as expected.
* `History` Toggles a pane that displays your session's browsing history. These items are clickable.
* `Bookmarks` Toggles a pane that displays your bookmarks. These are saved across sessions.
* `Bookmark This Page` Creates a new bookmark of the current page
* `Toggle Uri Display` should toggle whether or not the gopher URIs are displayed along side the links.

Right-clicking items (including those in the History and Bookmarks panes) should display a menu including the items:
* `Go Line` - Same as left-clicking. Navigate to this item.
* `Add Bookmark` - Create a bookmark to the item.
* `Remove Bookmark` - If a bookmark exists to the item, remove it from the bookmarks.


## Distribution and Other Ways to Launch

#### Building a stand-alone binary
The `build-exe.lsh` script should build a stand-alone application. This script, however, currently relies on sbcl and several system-specific things. It may work as is, but you may have to modify it to make it work.

Modifications you may need:
* Change the shebang line so that it works with your lisp
* Change the `(load "~/quicklisp/setup.lisp") line to work with your lisp environment

Once the binary is built, you should be able to launch the resulting `gopher` binary

The flags are:
* `-h` - Print a help message and exit

A Gopher URL may be passed as an argument to the program, to start the browser on that page:
```
$ gopher sdf.org/
```

If you find any issues with the software, or have ideas for improvements, please create an issue on this repo, or even better, send a Pull Request!
