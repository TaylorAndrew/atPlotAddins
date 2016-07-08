scatterPlotAddin <- function() {
library(shiny)
library(shinyjs)
  ui = miniUI::miniPage(
      useShinyjs(),
    miniUI::gadgetTitleBar("Scatter Plot"),
    miniUI::miniTabstripPanel(
    miniUI::miniTabPanel("Parameters", icon = shiny::icon("sliders"),
     miniUI::miniContentPanel(
    shiny::selectInput('dataset', 'Choose Dataset', names(which(unlist(eapply(.GlobalEnv,is.data.frame))))),
    shiny::selectInput('Y', 'Y-axis Variable', NULL),
    shiny::selectInput('X', 'X-axis Variable', NULL),
    shiny::selectInput('shape', 'Grouping Variable', "No Grouping"),
    shiny::selectInput('RegLine', 'Regression Line:', choices = c("None", "Linear", "Loess")),
    shiny::checkboxInput('jitter', "Jitter Points?", F)
  )
  ),
   miniUI::miniTabPanel("Visualize", icon = shiny::icon("area-chart"),
     miniUI::miniContentPanel(
         shiny::plotOutput("Plot")
     )
   ),
  miniUI::miniTabPanel("Export", icon = shiny::icon("share"),
     miniUI::miniContentPanel(
    shiny::textInput('plotName', 'Export to global environment as:', 'plot'),
    shiny::actionButton('export', 'Export'))
  )
  )
  )
  server = function(input, output, session){
  library(ggthemes)
  theme_HFHS <- function() {
  ggplot2::theme(axis.line = ggplot2::element_line(linetype = "solid"),
    panel.grid.major = ggplot2::element_line(colour = "gray78"),
    plot.title = ggplot2::element_text(size = 14, face = "bold"),
    legend.title = ggplot2::element_text(size = 8, face = "italic"),
    panel.background = ggplot2::element_rect(fill = NA),
    legend.key = ggplot2::element_rect(fill = NA),
    legend.background = ggplot2::element_rect(fill = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = ggplot2::element_text(size = 8))
}
    outVar = shiny::reactive({
      mydata = get(input$dataset)
    })
    shiny::observe({
      shiny::updateSelectInput(session, "Y",
      choices = names(outVar())[!unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) &
                                !unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))]
    )})
      shiny::observe({
      shiny::updateSelectInput(session, "X",
      choices = names(outVar())[!unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) &
                                !unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))]
    )})
    shiny::observe({
      shiny::updateSelectInput(session, "shape",
      choices =  c("No Grouping", names(outVar())[unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) |
                                               unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))|
                                               unlist(lapply(names(outVar()),  function(var) length(unique(outVar()[,var]))<=10))])
    )})
  plotInput <- reactive({
    df <- as.data.frame(outVar())
    y = input$Y
    x = input$X
    shape = input$shape
    RegLine = input$RegLine
    if(shape == 'No Grouping') {
      suppressWarnings(g <- ggplot2::ggplot(ggplot2::aes(x = df[, x], y = df[, y]), data = df) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(x) +
      ggplot2::scale_y_continuous(y) +
      theme_HFHS())
    } else {
      df[, shape] <- as.factor(df[, shape])
      suppressWarnings(g <- ggplot2::ggplot(ggplot2::aes(x = df[, x], y = df[, y], colour = df[, shape], group = df[, shape], shape = df[, shape]), data = df) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(x) +
      ggplot2::scale_y_continuous(y) +
      ggplot2::scale_shape_discrete(shape) +
      ggplot2::scale_colour_discrete(shape) +
      theme_HFHS())
    }
    if(RegLine=="Linear") suppressWarnings(g <- g + ggplot2::geom_smooth(method='lm',formula=y~x))
    if(RegLine=="Loess")  suppressWarnings(g <- g + ggplot2::geom_smooth(method='loess',formula=y~x))
    if(input$jitter==T) suppressWarnings(g <- g + ggplot2::geom_jitter())
    g
  })
  output$Plot <- shiny::renderPlot({
    plotInput()
  })
   shiny::observeEvent(input$export, {
      assign(input$plotName, plotInput(), envir = globalenv())
   })
   shiny::observeEvent(input$done, {
       shiny::stopApp()
     })
  }
  viewer <- shiny::paneViewer(300)
  shiny::runGadget(ui, server, viewer = viewer)
}