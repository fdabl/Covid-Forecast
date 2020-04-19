library('shiny')
library('jsonlite')
source('helpers.R')


shinyServer(function(input, output, session) {
  est <- reactiveValues(data = NULL)
  pred <- reactiveValues(data = NULL)
  inter <- reactiveValues(data = NULL)
  nr_inter <- reactiveValues(data = NULL)
  nr_inter_for <- reactiveValues(data = NULL)
  
  
  output$introduction <- renderText({
    '
    <h3>Welcome!</h3>
    
    <p>
    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium,
    totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.
    Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos
    qui ratione voluptatem sequi nesciunt.
    </p>
    
    <p>
    This Web app allows you to interactively explore the model described in 
    van Wees, Osinga, van der Kuip, Tanck, Pluymaekers, Leeuwenburgh, Bijsterveldt, Zindler, and van Furth.
    Forecasting hospitalization and ICU rates of the COVID-19 outbreak: an efficient SEIR model.
    [<a href=\'https://www.who.int/bulletin/online_first/20-256743.pdf\'>Paper</a>]
    [<a href=\'https://github.com/TNO/Covid-SEIR\'>Code</a>]
    </p>
    '
  })
  
  output$structure <- renderText({
    text1 <- '
    <h3>Model Overview</h3>
    
    <p>
    Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium,
    totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.
    </p>
    '
    
    text2 <- '<p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam,
    eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem
    quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.
    Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora
    incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis
    suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse
    quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?</p>'
    
    paste0(text1, '<p style="text-align: center;"><img src="SEIR.png" width=600 height=300></p>', text2)
  })
  
  observeEvent(input$nr_interventions, {
    nr_inter$data <- input$nr_interventions
  })
  
  observeEvent(input$nr_interventions_forecast, {
    nr_inter_for$data <- input$nr_interventions_forecast
  })
  
  output$alphas <- renderUI({
    lapply(seq(nr_inter$data), function(i) {
      alpha <- paste0('alpha', i)
      sliderInput(
        alpha,
        withMathJax(paste0('Uniform Prior on \\( \\alpha', '_', i, '\\)')),
        min = 0, max = 1,
        value = c(0.1, 0.5)
      )
    })
  })
  
  output$alphas_intervention <- renderUI({
    lapply(seq(nr_inter_for$data), function(i) {
      alpha <- paste0('alpha_for', i)
      numericInput(
        alpha,
        withMathJax(paste0('Value of \\( \\alpha', '_', i, '\\)')),
        min = 0, max = 1,
        value = 0.50
      )
    })
  })
  
  
  observeEvent(input$estimate, {
    est$data <- randn_py(100)
  })
  
  observeEvent(input$forecast, {
    pred$data <- randn_py(100)
  })
  
  observeEvent(input$intervene, {
    inter$data <- randn_py(100)
  })
  
  output$estimationPlot <- renderPlot({
    if (!is.null(est$data)) hist(est$data)
  })
  
  output$predictionPlot <- renderPlot({
    if (!is.null(pred$data)) hist(pred$data)
  })
  
  output$interventionPlot <- renderImage({
    # Get size of window from the session
    width <- session$clientData$output_interventionPlot_width
    height <- session$clientData$output_interventionPlot_height
    
    pixelratio <- session$clientData$pixelratio # what is this?
    
    draw_py(inter$data, width, height)
    
    # Return a list containing the filename
    list(
      src = 'Figures/Intervention-Plot.png',
      contentType = 'image/png',
      width = width,
      height = height,
      alt = 'Shows the effect of the Intervention'
    )
    
  }, deleteFile = TRUE)
})
