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

All of these things are a bit shaky. The scripts may work, but [your mileage may vary.](https://i.imgur.com/Ts0ab7w.gif)
However the good news is that all of the scripts are quite minimal, and any modifications you may need to make should be relatively minor.

#### Building a stand-alone binary
The `build-exe.lsh` script should build a stand-alone application. This script, however, currently relies on sbcl and several system-specific things. It may work as is, but it is not unlikely that you will have to modify it to make it work.

Modifications you may need:
* Change the shebang line so that it works with your lisp
* Change the `(load "~/quicklisp/setup.lisp") line to work with your lisp environment

Once the binary is built, you should be able to launch it. It has a couple of flags (one of which, `-r`, is necessary for the item icons and bookmarks to work properly).

The flags are:
* `-h` - Print a help message and exit
* `-r` - A path to the resource directory
* `-u` - A Gopher URL to start on, rather than the default gopher.floodgap.com

The `-r` flag should be a full path to the `res/` directory. This can be the one in this repo, or one from an extracted distribution tarball (instructions on how to create such a tarball below)

#### Creating a Distribution Tarball

There is a Makefile included whose default target builds a tarball with the complete application and resources.

This depends on the build-exe.lsh script above working properly.

All it really does is build the stand-alone binary, create a `clim-gopher` directory, copy the icon images, the various launcher scripts, the default bookmarks, and the gopher binary into it, and then make a tarball of it.

Once you create this tarball, you can copy it anywhere and extract it. From there, you can do the following:
```
$ clim-gopher/gopher -r /full/path/to/clim-gopher/res
```
to launch your gopher instance.

##### Distribution scripts

There is a script named `launch-gopher.sh` which should handle the passing of the correct resource directory. This is my preferred way of launching the application.

The only requirement for the script is that it remains located in the extracted clim-gopher directory, along side the `gopher` binary and the `res` directory.

`launch-gopher.sh` will accept all the same arguments as the primary gopher binary (except `-r` obviously, which it will calculate and pass)

You may add this directory to your $PATH, or create a symlink to the launch-gopher.sh file wherever you like.

There is also a similar script named `gopher-view.sh` which behaves like `launch-gopher.sh`, except it accepts exactly one argument, which should be a gopher URL to view.
```
$ gopher-view.sh gopher://gopher.floodgap.org:70/
```

#### Call to Action:

If you find any issues with the software, including the distribution scripts, please create an issue on this repo, or even better, send a Pull Request!

