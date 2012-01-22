
## this handles the static files
brewery <- Rook:::Brewery$new(url="/",
                              root=system.file("framework/brew", package="gWidgetsWWW2"))

staticfiles_app<-Builder$new(
                             Rook::Static$new(
                                              urls = c("/images", "/javascript", "/css"),
                                              root = system.file("base", package="gWidgetsWWW2")
                 ),
                             Redirect$new('/no_mas.txt')
                             )
