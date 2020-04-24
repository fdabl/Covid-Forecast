library('shiny')
library('jsonlite')
source('helpers.R')


shinyServer(function(input, output, session) {
  req <- httr::GET("http://worldtimeapi.org/api/ip")
  time <- httr::content(req)$datetime
  
  est <- reactiveValues(data = NULL)
  pred <- reactiveValues(data = NULL)
  inter <- reactiveValues(data = NULL)
  nr_inter <- reactiveValues(data = NULL)
  nr_inter_for <- reactiveValues(data = NULL)
  days_inter <- reactiveValues(data = NULL)
  days_inter_for <- reactiveValues(data = NULL)
  
  
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
    
    res <- paste0(text1, '<p style="text-align: center;"><img src="SEIR.png" width=600 height=300></p>', text2)
    paste(res, '<p>', time, '</p>')
  })
  
  observeEvent(input$nr_interventions, {
    nr_inter$data <- input$nr_interventions
  })
  
  observeEvent(input$nr_interventions_forecast, {
    nr_inter_for$data <- input$nr_interventions_forecast
  })
  
  observeEvent(input$days_intervention, {
    # TODO: Input handling
    days <- strsplit(input$days_intervention, ',')[[1]]
    days_inter$data <- days
  })
  
  observeEvent(input$days_intervention_forecast, {
    # TODO: Input handling
    days <- strsplit(input$days_intervention_forecast, ',')[[1]]
    days_inter_for$data <- days
  })
  
  output$alphas <- renderUI({
    lapply(seq(length(days_inter$data)), function(i) {
      alpha <- paste0('alpha', i)
      sliderInput(
        alpha,
        withMathJax(paste0('Uniform Prior on \\( \\alpha', '_', i, '\\)')),
        min = 0, max = 1,
        value = c(0.1, 0.5)
      )
    })
  })
  
  output$intervention <- renderUI({
    output <- tagList()
    
    for (i in seq(nr_inter_for$data)) {
      day <- paste0('day_for', i)
      alpha <- paste0('alpha_', i)
      
      output[[i]] <- tagList()
      output[[i]][[1]] <- dateInput(day, paste0('Date of Intervention ', i))
      output[[i]][[2]] <- sliderInput(
        alpha,
        # withMathJax(paste0('Percent R0 Reduction \\( \\alpha', '_', i, '\\)')),
        withMathJax(paste0('%\\( R_0 \\) Reduction on Intervention ', i)),
        min = 0, max = 1,
        value = c(0.1, 0.2)
      )
      
    }
    output
  })
  
  output$alphas_intervention <- renderUI({
    lapply(seq(nr_inter_for$data), function(i) {
      alpha <- paste0('alpha_for', i)
      sliderInput(
        alpha,
        withMathJax(paste0('Percent R0 Reduction of \\( \\alpha', '_', i, '\\)')),
        min = 0, max = 1,
        value = c(0.1, 0.2)
      )
    })
  })
  
  
  observeEvent(input$estimate1, { est$data <- randn_py(100) })
  observeEvent(input$estimate2, { est$data <- randn_py(100) })
  observeEvent(input$estimate3, { est$data <- randn_py(100) })
  
  observeEvent(input$forecast, {
    pred$data <- randn_py(100)
  })
  
  observeEvent(input$intervene, {
    inter$data <- randn_py(100)
  })
  
  output$infectedPlot <- renderPlot({
    if (!is.null(est$data)) hist(est$data)
  })
  
  output$hospitalizedPlot <- renderPlot({
    if (!is.null(est$data)) hist(est$data)
  })
  
  output$ICPlot <- renderPlot({
    if (!is.null(est$data)) hist(est$data)
  })
  
  output$deadPlot <- renderPlot({
    if (!is.null(est$data)) hist(est$data)
  })
  
  output$allPlot <- renderPlot({
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
    
    png('www/Intervention-Plot.png', width = width, height = height)
    plot(inter$data)
    dev.off()
    
    # draw_py(inter$data, width, height)
    
    # Return a list containing the filename
    list(
      src = 'www/Intervention-Plot.png',
      contentType = 'image/png',
      width = width,
      height = height,
      alt = 'Shows the effect of the Intervention'
    )
    
  }, deleteFile = TRUE)
})
