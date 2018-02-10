# CLIM-GOPHER

Browse Gopherspace in Common Lisp.

## Requirements

For html links to work properly `xdg-utils` must be installed on linux.

### To Run
```
(asdf:load-system 'clim-gopher)
(clim-gopher:browser)
```

#### Controls:

Clicking items should be the main method for exploration.
Aside from that:

`Back` and `Refresh` should work as expected.
`History` Toggles a pane that displays your session's browsing history. These items are clickable.
`Toggle Uri Display` should toggle whether or not the gopher URIs are displayed along side the links.


 