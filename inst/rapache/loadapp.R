app_name <- "RookApp"
script_file <- "/home/verzani/Rook/Test/test.R"

gw_app <- Builder$new(
            Rook:::Brewery$new(url="/",
                   root=system.file("framework/brew", package="gWidgetsWWW2"),
		   app_name=app_name,
                   extra_html_code=character(0),
		   brew_template=""
                   ),
            gWidgetsWWW2:::GWidgetsApp$new(
                 url="/gwapp", 
                 app_name=app_name, 
                 script=script_file),
            Redirect$new(sprintf("/indexgw.rhtml"))
            )
