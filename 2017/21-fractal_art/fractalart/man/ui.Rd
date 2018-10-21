\name{ui}
\alias{ui}
\docType{data}
\title{ui
%%   ~~ data name/kind ... ~~
}
\description{UI function for the Shiny app
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ui")}
\format{
  The format is:
List of 3
 $ : NULL
 $ : NULL
 $ :List of 1
  ..$ :List of 3
  .. ..$ name    : chr "div"
  .. ..$ attribs :List of 1
  .. .. ..$ class: chr "container-fluid"
  .. ..$ children:List of 2
  .. .. ..$ :List of 2
  .. .. .. ..$ :List of 3
  .. .. .. .. ..$ name    : chr "head"
  .. .. .. .. ..$ attribs : list()
  .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. ..$ name    : chr "title"
  .. .. .. .. .. .. ..$ attribs : list()
  .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. ..$ : chr "Old Faithful Geyser Data"
  .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..$ :List of 3
  .. .. .. .. ..$ name    : chr "h2"
  .. .. .. .. ..$ attribs : list()
  .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. ..$ : chr "Old Faithful Geyser Data"
  .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..- attr(*, "class")= chr [1:2] "shiny.tag.list" "list"
  .. .. ..$ :List of 3
  .. .. .. ..$ name    : chr "div"
  .. .. .. ..$ attribs :List of 1
  .. .. .. .. ..$ class: chr "row"
  .. .. .. ..$ children:List of 2
  .. .. .. .. ..$ :List of 3
  .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. ..$ class: chr "col-sm-4"
  .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. ..$ name    : chr "form"
  .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. ..$ class: chr "well"
  .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ class: chr "form-group shiny-input-container"
  .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "bins"
  .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "Number of bins:"
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 13
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ class                  : chr "js-range-slider"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ id                     : chr "bins"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-min               : chr "1"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-max               : chr "50"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-from              : chr "30"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-step              : chr "1"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-grid              : chr "true"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-grid-num          : num 9.8
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-grid-snap         : chr "false"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-prettify-separator: chr ","
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-prettify-enabled  : chr "true"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-keyboard          : chr "true"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-data-type         : chr "number"
  .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "html_dependencies")=List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 10
  .. .. .. .. .. .. .. .. .. .. .. ..$ name      : chr "ionrangeslider"
  .. .. .. .. .. .. .. .. .. .. .. ..$ version   : chr "2.1.6"
  .. .. .. .. .. .. .. .. .. .. .. ..$ src       :List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ href: chr "shared/ionrangeslider"
  .. .. .. .. .. .. .. .. .. .. .. ..$ meta      : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ script    : chr "js/ion.rangeSlider.min.js"
  .. .. .. .. .. .. .. .. .. .. .. ..$ stylesheet: chr [1:2] "css/ion.rangeSlider.css" "css/ion.rangeSlider.skinShiny.css"
  .. .. .. .. .. .. .. .. .. .. .. ..$ head      : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ attachment: NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ package   : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ all_files : logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "html_dependency"
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 10
  .. .. .. .. .. .. .. .. .. .. .. ..$ name      : chr "strftime"
  .. .. .. .. .. .. .. .. .. .. .. ..$ version   : chr "0.9.2"
  .. .. .. .. .. .. .. .. .. .. .. ..$ src       :List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ href: chr "shared/strftime"
  .. .. .. .. .. .. .. .. .. .. .. ..$ meta      : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ script    : chr "strftime-min.js"
  .. .. .. .. .. .. .. .. .. .. .. ..$ stylesheet: NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ head      : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ attachment: NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ package   : NULL
  .. .. .. .. .. .. .. .. .. .. .. ..$ all_files : logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "html_dependency"
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. ..$ :List of 3
  .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. ..$ class: chr "col-sm-8"
  .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. ..$ id   : chr "distPlot"
  .. .. .. .. .. .. .. .. ..$ class: chr "shiny-plot-output"
  .. .. .. .. .. .. .. .. ..$ style: chr "width: 100% ; height: 400px"
  .. .. .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. ..- attr(*, "class")= chr "shiny.tag"
 - attr(*, "class")= chr [1:2] "shiny.tag.list" "list"
 - attr(*, "html_dependencies")=List of 1
  ..$ :List of 10
  .. ..$ name      : chr "bootstrap"
  .. ..$ version   : chr "3.3.7"
  .. ..$ src       :List of 2
  .. .. ..$ href: chr "shared/bootstrap"
  .. .. ..$ file: chr "/Users/frank/src/advent_of_code/2017/21-fractal_art/packrat/lib/x86_64-apple-darwin15.6.0/3.5.1/shiny/www/shared/bootstrap"
  .. ..$ meta      :List of 1
  .. .. ..$ viewport: chr "width=device-width, initial-scale=1"
  .. ..$ script    : chr [1:3] "js/bootstrap.min.js" "shim/html5shiv.min.js" "shim/respond.min.js"
  .. ..$ stylesheet: chr "css/bootstrap.min.css"
  .. ..$ head      : NULL
  .. ..$ attachment: NULL
  .. ..$ package   : NULL
  .. ..$ all_files : logi TRUE
  .. ..- attr(*, "class")= chr "html_dependency"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ui)
## maybe str(ui) ; plot(ui) ...
}
\keyword{datasets}
