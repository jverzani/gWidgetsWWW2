TODO
----

* trim down for use with rook only

* e_cookies?

* gdf expand=TRUE, not same as gtable. Odd

* gcheckboxgroup + use.table

* in ex-gcanvas-motion - error o eventX eventY. Chrome warning

* gfile has bad styling, not sure why the button goes where it does.

* ggooglemmaps no e3 object found. (This is buggy, was fixed, now not ...)

* why so slow? (Reference class methods?)

* Can use template for FastRWeb/load_AJAX.R, but don't

DONE
----

* <del> gformlayout-- simplify forms; pass for now</del>

* <del>gmarkdown (markdown package -> ghtml)</del>

* <del> REMOVED webpage, authenticator, need polish ROauth (httr)</del>

* <del>add_handler (clear up, gtable needs selection/changed tidied up)</del>

* <del>move session code into package bypassing filehash</del>

* <del>json-rpc server for FastRWeb</del>

* <del>work on cacheing JavaScript files for Rook (apache config?). Done through apache config</del>

* <del>decouple gWidgetsWWW2 and the deployment parts so both FastRWeb and Rook can be used (check that urls are not embedded, ...)</del>


* <del>tkdensity examples -- are comboboxes broken? Seems they stop calling "change" handler...</del>


* <del>gtable:</del>
  - <del>paging is broken</del>
  - <del>Reassingment [<- does not allow shortening</del>
  - <del>headers: setting isn't works as ogWidget_ID4.colModel.setColumnHeader(6,'AGE'); doesn't have colModel</del>

* <del>gdf exmaples - changes names, ... isn't working</del>

* <del> issue with render call in subwindows </del>

* <del>issues with gcodemirror. (setValue method)</del>

* <del>gcodemirror -- put in text proxy, not stripping off of '. Can use add_async_javascript_callback or jRpc stuff.</del>

* <del>gdf seems broken (add, remove can be done now see index)</del>


FastRWeb
--------

* work in tempfiles (gsvg say) and tmp url

* 


nginx
-----

* <done>load_app for loading many apps in startup script. See Rook.sh</del>

apache proxy
------------

* <del>give example of proxyPassReverse - not rApache</del>


Documentation
--------------

* roxygen docs

* vignette



Unit tests
----------

* add more

App
---

* session manager -- can we make session accessible to local user? can
  we make ability to broadcast if $listen is done?

* make load_app load an app, be it a script or a directory

* make load_dir load all apps in a directory -- if a script (*.R) or a
  directory. This way we can start server to run headless more simply

AJAX calls
----------

* put javascript into a namespace

* can replace run_transport with jRpc call 

* proxy calls: should make configuration of gtable view and gdf view
  user customizable. WOuld need to add user-defined methods

Widgets
--------

containers
----------

* gformlayout (mebbe)


gWidgets
---------

* gedit "validate.type" -- valid.type, rename

* gcombobox 

- cant get templates to work! Ughh. displayTpl is close,
  but no cigar

- would like remote queryMode for autocomplete on larger stores

- fonts -- do we need? Use HTML

* ggooglevis in a panel? XXX This needs work

* galert: no css, too wide. Why? (css missing)

* gcanvas: working, needs documenting

* gtable: 

- checkbox selection; grouping variables (add), means to limit
  which fields are shown

- --an't be bottom in layout???--

* --gdf: We can add and edit, but can't delete. WOuld like to use Rest proxy, but isn't working iwth Rook, as the request is denied -- not even parsable--

* work on size issues with radio, check box


