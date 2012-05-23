w <- gwindow("Example of the gpanel widget", visible=FALSE)
gstatusbar("Powered by Rook and gWidgetsWWW2", cont=w)

g <- ggroup(cont=w, horizontal=FALSE, use.scrollwindow=TRUE)

ghtml("
The gpanel widget lets one incorporate other JavaScript libraries<br/>
into gWidgetsWWW2. Well hopefully. In the following, we use the d3<br/>
library (<a href='http://mbostock.github.com/d3'>d3.js</a>) where this<br/>
example is from.
", cont=g)


pan <- gpanel(cont=g, width=1000, height=500)

## The d3_cmds are defined below. This will be called asychronously
## after load_external is done downloading the libraries
pan$add_handler_onload(function(h,...) {
  pan$add_js_queue(d3_cmds)
})

## load_external can download a series of scripts, this
## worked before d3 went monolithic
#d3_url <- "http://mbostock.github.com/d3/d3.js?2.7.1"
#d3_geo_url = "http://mbostock.github.com/d3/d3.geo.js?2.7.1"
#pan$load_external(c(d3_url, d3_geo_url))

d3_url <- "http://d3js.org/d3.v2.js"
pan$load_external(d3_url)


## template for d3_cmds. The only feature here is the use of #{{div_id}} for the
## d3.select method. This is filled in by whisker using pan$div_id().
tpl <- '
var us_states_json = {{json_data}};

var path = d3.geo.path();

var svg = d3.select("#{{div_id}}")
  .append("svg");


var states = svg.append("g")
    .attr("id", "states");


states.selectAll("path")
    .data(us_states_json.features)
    .enter().append("path")
    .attr("d", path);
'

## states data comes from d3 example page. Processed with:
## x <- readLines("http://mbostock.github.com/d3/data/us-states.json")
## x <- gsub('"', "'",x)
## cat(paste(x, collapse="\n"), file="states.json")
states_json <- readLines(system.file("ex_data","states.json", package="gWidgetsWWW2"))

## make the commands
d3_cmds <- whisker.render(tpl, list(div_id=pan$div_id(),
                                    json_data=paste(states_json, collapse="")))
