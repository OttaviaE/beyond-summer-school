library(shiny)

ui <- fluidPage(
  # Put your CSS in the <head> so it’ll style all sliders on the page
  tags$head(
    tags$style(HTML("
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { background: royalblue; }
      .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar { background: magenta; }
      .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar { background: seagreen; }
    "))
  ),
  
  # First row of sliders
  fluidRow(
    column(4, sliderInput("b1", "d",  min = 0, max = 1, value = 1, step = 0.1)),
    column(4, sliderInput("b2", "d1", min = 0, max = 1, value = 1, step = 0.1)),
    column(4, sliderInput("b3", "d2", min = 0, max = 1, value = 1, step = 0.1))
  ),
  br(),
  
  # Two plots side by side
  fluidRow(
    column(6, plotOutput("plot",   height = "300px")),
    column(6, plotOutput("tif",    height = "300px"))
  ),
  
  # One centered plot below
  fluidRow(
    column(3), 
    column(6, plotOutput("tifEx", height = "300px")),
    column(3)
  )
)
