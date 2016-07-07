 beeswarmAddin <- function() {

  ui = miniUI::miniPage(
    miniUI::gadgetTitleBar("Beeswarm Plot"),
     miniUI::miniContentPanel(
    shiny::selectInput('dataset', 'Choose Dataset', names(which(unlist(eapply(.GlobalEnv,is.data.frame))))),
    shiny::selectInput('Y', 'Y-axis Variable', NULL),
    shiny::selectInput('group', 'Group Variable', "No Group"),
    shiny::selectInput('color', 'Colour Variable', "No Colour"),
    shiny::selectInput('transform', 'Transformation:', choices = c("None", "log", "log1p", "square", "exp", "sqrt")),
    shiny::plotOutput("Plot")
  )
  )
  server = function(input, output, session){
    library(beeswarm)
    outVar = shiny::reactive({
      mydata = get(input$dataset)
    })
    shiny::observe({
      shiny::updateSelectInput(session, "Y",
      choices = names(outVar())[!unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) &
                                !unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))]
    )})
      shiny::observe({
      shiny::updateSelectInput(session, "group",
      choices =  c("No Group", names(outVar())[unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) |
                                               unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))|
                                               unlist(lapply(names(outVar()),  function(var) length(unique(outVar()[,var]))<=10))])
    )})
    shiny::observe({
      shiny::updateSelectInput(session, "color",
      choices =  c("No Colour", names(outVar())[unlist(lapply(names(outVar()), function(var) is.factor(outVar()[,var]))) |
                                               unlist(lapply(names(outVar()),  function(var) is.character(outVar()[,var])))|
                                               unlist(lapply(names(outVar()),  function(var) length(unique(outVar()[,var]))<=10))])
    )})
  output$Plot <- shiny::renderPlot({
    df <- as.data.frame(outVar())
    y = input$Y
    grp = input$group
    color = input$color
    # if(color == 'No Group') color = 1
    if(input$transform=='log') df[,y] <- as.integer(log(df[,y]))
    if(input$transform=='log1p') df[,y] <- log1p(df[,y])
    if(input$transform=='square') df[,y] <- df[,y]^2
    if(input$transform=='exp') df[,y] <- exp(df[,y])
    if(input$transform=='sqrt') df[,y] <- sqrt(df[,y])
    if(grp == "No Group") {
      if(color == 'No Colour') {
      beeswarm::beeswarm(df[y], ylab = y)
      }
      if(color != 'No Colour') {
        beeswarm::beeswarm(df[y], ylab = y,
                         pwcol = 1 + if(color == 'No Colour') {0} else {
                          as.numeric(factor(df[, color])) })
        legend("topright", legend =levels(factor(df[, color])),
               title = color, pch = 16,
               col = 2:(1+length(factor(df[, color]))))
      }
    }
    if(grp != "No Group") {
      if(color == 'No Colour') {
       beeswarm::beeswarm(df[, y] ~ df[, grp], ylab = y, xlab = grp,
                          method = 'swarm', corral = 'wrap')
      }
      if(color != 'No Colour') {
        beeswarm::beeswarm(df[, y] ~ df[, grp], ylab = y, xlab = grp,
                          method = 'swarm', corral = 'wrap',
                          pwcol = 1 +
                          as.numeric(factor(df[, color])))
        legend("topright", legend = levels(factor(df[, color])),
               title = color, pch = 16,
               col = 2:(1+length(factor(df[, color]))))
      }
    }
  })
   shiny::observeEvent(input$done, {
       shiny::stopApp()
     })
  }
  viewer <- shiny::paneViewer(300)
  shiny::runGadget(ui, server, viewer = viewer)

}
