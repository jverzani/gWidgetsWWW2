== TODO

* gdf seems broken (add)
* why so slow? (Reference class methods?)

=== nginx
* better example: need 3 things change to server, change to http and R
  start up script
* load_app for loading many apps in startup script

=== apache proxy

=== Documentation
* roxygen docs
* vignette
* load_all -- not working

=== Unit tests

* add more

=== App

* indexgw.rthml -- do we need this?
* subclass load app (In Rook already, but might be nicer if google
  apps like)
* session manager -- can we make session accessible to local user? can
  we make ability to broadcast if $listen is done?

=== AJAX calls

* put javascript into a namespace
* can replace run_transport with jRpc call 
* proxy calls: should make configuration of gtable view and gdf view
  user customizable. WOuld need to add user-defined methods

=== Widgets

==== containers

* gformlayout (mebbe)


===== gWidgets

* gedit "validate.type" -- valid.type, rename

* gcombobox 
-- cant get templates to work! Ughh. displayTpl is close,
  but no cigar
-- would like remote queryMode for autocomplete on larger stores

-- fonts -- do we need? Use HTML

* ggooglevis in a panel? XXX This needs work

* galert: no css, too wide. Why? (css missing)

* gcanvas: working, needs documenting

* gtable: 
-- checkbox selection; grouping variables (add), means to limit
  which fields are shown
-- an't be bottom in layout???

* gdf: We can add and edit, but can't delete. WOuld like to use Rest proxy, but isn't working iwth Rook, as the request is denied -- not even parsable

* work on size issues with radio, check box

