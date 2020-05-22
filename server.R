library('shiny')
library('plotly')
library('ggplot2')
library('jsonlite')
source('helpers.R')


shinyServer(function(input, output, session) {
  req <- httr::GET("http://worldtimeapi.org/api/ip")
  time <- httr::content(req)$datetime
  
  model <- reactiveValues(data = NULL)
  config <- reactiveValues(data = NULL)
  config_obj <- reactiveValues(data = NULL)
  has_intervened <- reactiveValues(data = NULL)
  reset <- reactiveValues(data = NULL)
  
  single_run <- reactiveValues(data = NULL)
  nr_inter <- reactiveValues(data = NULL)
  nr_inter_for <- reactiveValues(data = NULL)
  days_inter <- reactiveValues(data = NULL)
  days_inter_for <- reactiveValues(data = NULL)
  show_prior_interventions <- reactiveValues(data = NULL)
  
  observeEvent(input$nr_interventions, { nr_inter$data <- input$nr_interventions })
  observeEvent(input$show_alpha, { show_prior_interventions$data <- input$show_alpha })
  observeEvent(input$nr_interventions_forecast, { nr_inter_for$data <- input$nr_interventions_forecast })
  
  observeEvent(input$reset, {
    reset$data <- TRUE
  })
  
  # User clicks on 'Estimate' to run the model
  run_model <- function() {
    config_obj$data <- create_config(input, single_run = FALSE)
    write(config_obj$data, 'config.json')
    config$data <- fromJSON('config.json')
    res <- run_dashboard_wrapper(toJSON(config$data, auto_unbox = TRUE))
    
    # After running the model, reset this to FALSE
    has_intervened$boolean <- FALSE
    model$data <- res[[1]]
  }
  
  observeEvent(input$run1, { run_model() })
  observeEvent(input$run2, { run_model() })
  observeEvent(input$run3, { run_model() })
  observeEvent(input$reset, { has_intervened$boolean <- FALSE })
  
  
  observeEvent(input$intervene, {
    # Update the config data with 'single_run = TRUE'
    config_obj$data <- create_config(input, single_run = TRUE)
    write(config_obj$data, 'config.json')
    config$data <- fromJSON('config.json')
    
    # Estimate a single run of the model
    res <- run_dashboard_wrapper(toJSON(config$data, auto_unbox = TRUE))
    
    # res <- list('alpha' = seq(1, 348))
    # 
    # for (i in seq(10)) {
    #   d <- run_dashboard_wrapper(toJSON(config$data, auto_unbox = TRUE))
    #   res[['alpha']] <- cbind(res[['alpha']], d[[1]][['alpha']][, 2])
    # }
    
    # print(res[['alpha']])
    # model$single_data <- res
    
    has_intervened$boolean <- TRUE
    model$single_data <- res[[1]]
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

  
  # Allow user to set the prior interventions when box is ticked
  output$prior_intervention <- renderUI({
    if (show_prior_interventions$data) {
      # start <- as.Date(config[['startdate']], tryFormats = '%m/%d/%y')
      startdate <- as.Date('3/1/20', tryFormats = '%m/%d/%y')
      
      output <- tagList()
      
      for (i in seq(nr_inter$data)) {
        day <- paste0('day_', i)
        alpha <- paste0('alpha_', i)
        
        output[[i]] <- tagList()
        output[[i]] <- fluidRow(
          column(6, dateInput(day, paste0('Intervention Date'), value = startdate + DAYALPHAS[i])),
          column(6, numericInput(
              alpha, withMathJax(paste0('% Social Contact')),
              value = 1 - mean(ALPHAS[[i]]), step = 0.01, min = 0, max = 1
          ))
        )
      }
      
      return(output)
    }
  })
  
  
  # Allow user to set counterfactual / future interventions 
  output$intervention <- renderUI({
    output <- tagList()
    
    for (i in seq(nr_inter_for$data)) {
      day <- paste0('day_intervention_', i)
      alpha <- paste0('alpha_intervention_', i)
      
      output[[i]] <- fluidRow(
        column(6, dateInput(day, paste0('Intervention Date'))),
        column(6, numericInput(
            alpha, withMathJax(paste0('% Social Contact')),
            value = 0.50, step = 0.01, min = 0, max = 1
          )
        )
      )
      
    }
    output
  })
  
  output$infectedPlot <- renderPlot({
    cols <- c('#FFE4E1', '#F08080')
    ylab <- 'Confirmed Cases'
    title <- 'Confirmed Cases in The Netherlands'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(config$data, model, 'infected', cols, ylab, title, has_intervened$boolean)
      reset$data <- FALSE
      p
    }
  })
    
  output$hospitalizedPlot <- renderPlot({
    cols <- c('#B0E0E6', '#4682B4')
    ylab <- 'Hospitalized Cases'
    title <- 'Hospitalized Cases in The Netherlands'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(config$data, model, 'hospitalized', cols, ylab, title, has_intervened$boolean)
      reset$data <- FALSE
      p
    }
  })
  
  output$ICPlot <- renderPlot({
    cols <- c('#FFDAB9', '#F4A460')
    ylab <- 'Intensive Care Cases'
    title <- 'Intensive Care Cases in The Netherlands'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(config$data, model, 'ICU', cols, ylab, title, has_intervened$boolean)
      reset$data <- FALSE
      p
    }
  })
  
  output$deadPlot <- renderPlot({
    cols <- c('#C0C0C0', '#808080')
    ylab <- 'Mortalities'
    title <- 'Mortalities in The Netherlands'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(config$data, model, 'dead', cols, ylab, title, has_intervened$boolean)
      reset$data <- FALSE
      p
    }
  })
  
  output$allPlot <- renderPlot({
    if (!is.null(model$data)) {
      p1 <- plot_predictions(
        config$data, model, 'infected', c('#FFE4E1', '#F08080'),
        'Confirmed Cases', 'Confirmed Cases in The Netherlands', has_intervened$boolean
      )
      
      p2 <- plot_predictions(
        config$data, model, 'hospitalized', c('#B0E0E6', '#4682B4'),
        'Hospitalized Cases', 'Hospitalized Cases in The Netherlands', has_intervened$boolean
      )
      
      p3 <- plot_predictions(
        config$data, model, 'ICU', c('#FFDAB9', '#F4A460'),
        'Intensive Care Cases', 'Intensive Care Cases in The Netherlands', has_intervened$boolean
      )
      
      p4 <- plot_predictions(
        config$data, model, 'dead', c('#C0C0C0', '#808080'),
        'Mortalities', 'Mortalities in The Netherlands', has_intervened$boolean
      )
      
      gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
    }
  })
  
  
  output$interventionPlot <- renderPlot({
    cols <- c('#DDA0DD', '#BA55D3')
    
    # Math causes plotly errors
    ylab <- latex2exp::TeX('Social Contacts (%)$')
    title <- latex2exp::TeX('Reduction in Social Contacts')
    
    # ylab <- '1 - alpha'
    # title <- 'Reduction in Rt across Time'
    
    if (!is.null(model$data)) {
      p <- plot_interventions(config$data, model, cols, ylab, title, has_intervened$boolean)
      p
    }
  })
  
  
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
})
