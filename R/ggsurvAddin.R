# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
ggsurvAddin <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Create ggsuv code"),
    miniContentPanel(
      # textInput('model', "survfit model:", ''),
      # textInput('pvalModel', 'survdiff model:', ''),
      # textInput('labels', 'Labels:', 'c()'),
      # textInput('xaxis', 'xaxis', '')
    )
  )

  server <- function(input, output, session) {

    # Set some CSS styles for our clock.
   ggSurvText <-
   "ggSurv(model) +
        scale_y_continuous('Survival', limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
        scale_x_continuous('Time (in years)', limits = c(0, 6.5), breaks = c(0:6)) +
        scale_linetype_manual('p16', values = c(1,1), labels = c('Negative', 'Positive')) +
        scale_color_manual('p16', values = c('red', 'blue'), labels = c('Negative', 'Positive')) +
        ggtitle('Kaplan-Meier Survival Curves for Local Recurrence\nEarly Overall Stage (I-II)') +
        theme(legend.position = 'top') +
        annotate('text', x = 4, y = 25, label = 'Some text')"

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      rstudioapi::insertText(ggSurvText)
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
}

# Try running the clock!
# InsertComment80()
