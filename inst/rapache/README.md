As of RApache 1.1.15 there is support for Rook apps. In theory then, this should be able to be run under RApache.

Unfortunately there are two issues:

* The parsing of POST requests from Ext is troublesome. This can
  (mostly) be worked around by using GET requests and jQuery. The
  needed functions are included in load_AJAX.rhtml

* With those in place, the whole thing is just too slow. Part of this
  is due to the use of filehash to serialize environments.

That being said, these files are here as a start on the integration.
