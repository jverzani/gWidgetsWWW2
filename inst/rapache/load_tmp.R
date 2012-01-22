tmp_app <- Builder$new(
                       Rook::Static$new(
                                        urls=c("/tmp"),
                                        root=tempdir()
                                        ),
                       Redirect$new(sprintf("/%s", basename(f)))
                       )
  
