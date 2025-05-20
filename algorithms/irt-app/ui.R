library(shiny)

ui <- fluidPage(
  # Put your CSS in the <head> so it’ll style all sliders on the page
  tags$head(
    tags$style(HTML("
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { background: royalblue; }
      .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar { background: magenta; }
      .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar { background: seagreen; }
      .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar { background: royalblue; }
      .js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar { background: magenta; }
      .js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar { background: seagreen; }
    "))
  ),
 sidebarLayout(
   sidebarPanel(
     sliderInput("b1", "b1",
                 min = -3, max = 3,
                 value = 0, step = 0.1),
     sliderInput("b2", "b2",
                 min = -3, max = 3,
                 value = -1, step = 0.1),
     sliderInput("b3", "b3",
                 min = -3, max = 3,
                 value = 1, step = 0.1),
     
     sliderInput("a1", "a1",
                 min = 0.20, max = 3,
                 value = 1, step = 0.1),
     sliderInput("a2", "a2",
                 min = 0, max = 3,
                 value = 1, step = 0.1),
     sliderInput("a3", "a3",
                 min = 0, max = 3,
                 value = 1, step = 0.1),
     checkboxInput("showTif", "Show Test Information Function", value = FALSE)
   ), 
   mainPanel(plotOutput("plot", height = "300px"),
             
             plotOutput("tif", height = "300px") )
 )  

)
