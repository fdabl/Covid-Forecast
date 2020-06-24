library('shiny')
library('plotly')
library('ggplot2')
library('jsonlite')
source('helpers.R')


shinyServer(function(input, output, session) {
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
    config_obj$data <- create_config(input, NULL, single_run = FALSE)
    write(config_obj$data, 'config.json')
    config$data <- fromJSON('config.json')
    res <- run_dashboard_wrapper(toJSON(config$data, auto_unbox = TRUE))
    
    # After running the model, reset this to FALSE
    has_intervened$boolean <- FALSE
    model$data <- res[[1]]
    model$posterior_alphas <- res[[2]][['alpha']]
    model$config <- res[[2]]
  }
  
  observeEvent(input$run1, { run_model() })
  observeEvent(input$run2, { run_model() })
  observeEvent(input$run3, { run_model() })
  observeEvent(input$reset, { has_intervened$boolean <- FALSE })
  
  observeEvent(input$intervene, {
    
    # Need to have run the model before intervention is possible!
    if (!is.null(model$data)) {
      
      # Update the config data with 'single_run = TRUE'
      # and the intervention parameters (alpha and days)
      # save them in the config_int variable, then run the model
      
      # Estimate a single run of the model
      config <- model$config
      config_int <- model$config
      config_int$single_run <- TRUE
      
      alphas_inter <- paste0('alpha_intervention_', seq(input$nr_interventions_forecast))
      alphas_inter <- lapply(alphas_inter, function(alpha) list(1 - input[[alpha]], 0.10))
      alphas <- c(model$posterior_alphas, alphas_inter)
      
      startdate <- as.Date(config_int$startdate, tryFormats = '%m/%d/%y')
      dayalphas_inter <- paste0('day_intervention_', seq(input$nr_interventions_forecast))
      dayalphas_inter <- sapply(dayalphas_inter, function(day) input[[day]] - as.numeric(startdate))
      
      config_int$dayalpha <- c(config$dayalpha, dayalphas_inter)
      config_int$alpha <- alphas
      model$config_int <- config_int
      
      write(prettify(toJSON(config_int)), 'config_updated.json')
      
      res <- run_dashboard_wrapper(toJSON(config_int, auto_unbox = TRUE))
      has_intervened$boolean <- TRUE
      model$single_data <- res[[1]]
    }
  })
  
  
  run_example_intervention <- reactiveValues(data = FALSE)
  
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
        output[[i]] <- splitLayout(
            cellWidths = c('50%', '50%'),
            dateInput(
              day, paste0('Intervention Date'),
              value = startdate + DAYALPHAS[i], min = '2020-03-02', width = '100%'
            ),
            numericInput(
              alpha, withMathJax(paste0('% Social Contact')),
              value = 1 - mean(ALPHAS[[i]]), step = 0.01, min = 0, max = 1, width = '100%'
            )
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
      
      output[[i]] <- splitLayout(
        cellWidths = c('50%', '50%'),
        dateInput(
          day, paste0('Intervention Date'),
          min = '2020-03-02', width = '100%'
        ),
        numericInput(
          alpha, withMathJax(paste0('% Social Contact')),
          value = 0.30, step = 0.01, min = 0, max = 1, width = '100%'
        )
      )
    }
    output
  })
  
  output$infectedPlot <- renderPlot({
    cols <- c('#FFE4E1', '#F08080')
    ylab <- 'Confirmed Cases'
    title <- 'Cumulative Confirmed Cases'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(
        config$data, model, 'infected', cols, ylab,
        title, has_intervened$boolean
      ) + scale_y_continuous(n.breaks = 5)
      reset$data <- FALSE
      p
    }
  })
    
  output$hospitalizedPlot <- renderPlot({
    cols <- c('#B0E0E6', '#4682B4')
    ylab <- 'Hospitalized Cases'
    title <- 'Cumulative Hospitalized Cases'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(
        config$data, model, 'hospitalizedcum', cols, ylab,
        title, has_intervened$boolean
      ) + scale_y_continuous(n.breaks = 5)
      reset$data <- FALSE
      p
    }
  })
  
  output$ICPlot <- renderPlot({
    cols <- c('#FFDAB9', '#F4A460')
    ylab <- 'Intensive Care Cases'
    title <- 'Intensive Care Cases'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(
        config$data, model, 'ICU', cols, ylab,
        title, has_intervened$boolean
      )
      reset$data <- FALSE
      p
    }
  })
  
  output$deadPlot <- renderPlot({
    cols <- c('#C0C0C0', '#808080')
    ylab <- 'Mortalities'
    title <- 'Cumulative Mortalities'
    
    if (!is.null(model$data)) {
      p <- plot_predictions(
        config$data, model, 'dead', cols, ylab,
        title, has_intervened$boolean
      )
      reset$data <- FALSE
      p
    }
  })
  
  output$allPlot <- renderPlot({
    if (!is.null(model$data)) {
      plot_all(config$data, model, has_intervened$boolean)
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
  
  
  output$explanation_plot_predictions <- renderPlot({
    run_model()
    if (!is.null(model$data)) {
      plot_all(config$data, model, has_intervened$boolean)
      run_example_intervention$data <- TRUE
    }
  })
  
  
  output$explanation_plot_intervention <- renderPlot({
    if (!is.null(model$data) && run_example_intervention$data) {
      
      # Estimate a single run of the model
      config <- model$config
      config_int <- model$config
      config_int$single_run <- TRUE
      
      alphas_inter <- list(list(1 - 0.28, 0.10))
      alphas <- c(model$posterior_alphas, alphas_inter)
      
      # Make an intervention today
      startdate <- as.Date(config_int$startdate, tryFormats = '%m/%d/%y')
      intdate <- as.Date('2020-04-06')
      dayalphas_inter <- as.numeric(intdate) - as.numeric(startdate)
      
      config_int$dayalpha <- c(config$dayalpha, dayalphas_inter)
      config_int$alpha <- alphas
      model$config_int <- config_int
      
      res <- run_dashboard_wrapper(toJSON(config_int, auto_unbox = TRUE))
      has_intervened$boolean <- FALSE
      model$single_data <- res[[1]]
        
      plot_all(config_int, model, TRUE)
    }
  })
  
  
  # Adaptive resizing of the model explanation figure
  output$model_explanation <- renderImage({
    width  <- session$clientData$output_model_explanation_width
    height <- session$clientData$output_model_explanation_height
    pixelratio <- session$clientData$pixelratio
    
    list(
      src = 'www/SEIR.png',
      contentType = 'img/png',
      width = width,
      height = height * 1.2,
      alt = 'Model Explanation'
    )
    
  }, deleteFile = FALSE)
  
  
  output$model_overview <- renderText({
    res <- "
    <p>
    Here, we briefly give an overview of the model. For details, please refer to
    Van Wees et al.
    (<a href='https://www.who.int/bulletin/online_first/20-256743.pdf' target='_blank'>2020a</a>,
    <a href='https://www.medrxiv.org/content/10.1101/2020.05.16.20102947v1'>2020b</a>).
    </p>
    
    <p>
    The model follows a SEIR structure which assumes that the population is comprised of
    susceptible, exposed (i.e., latent infection), infectious, and removed subpopulations. Importantly, the model
    assumes that reinfections do not occur, that is, that immunity lasts for life, or at least until the end of the simulation.
    In addition to the basic SEIR structure, the model also incorporates a realistic flow process for hospitalization and intensive care.
    This allows one to split the removed subpopulation into those that have recovered and those that have deceased.
    The Figure on the right gives an overview of the model.
    </p>
    
    <p>
    The fraction of the population that is susceptible to the disease is given by m. R<sub>0</sub> is the basic reproductive number,
    &sigma; gives the rate with which exposed people become infectious, and &gamma; is the rate with which infected people self-quarantine.
    Susceptible people become exposed with a rate given by (1 - &#945;(t))R<sub>0</sub>&gamma;.
    </p>
    
    <p>
    &#945;(t) is a key parameter, which encodes the strength of social distancing: &#945;(t) = 0 when no measures are in place, and
    &#945;(t) = 1 when social distancing would prevent any transmission. Under <i>Interactive Exploration</i>, you can adjust these parameters, make model predictions, and change &#945;(t) to
    assess the effect that interventions could have. An overview of all parameters and sensible default values are given in the Table below.
    </p>
    
    <p>
    As shown in the Figure, the fraction of patients who go to the hospital, which takes about d<sub>hos</sub> days,
    is given by h, while the fraction of patients with mild symptoms, which recover in about d<sub>rec</sub> days, is given by 1 - h. 
    </p>
    
    <p>
    The fraction of hospitalized patients who die immediately without going to the ICU is given by f<sub>hos</sub>, while the
    fraction who goes to the ICU is given by i(t). Some hospitalized patients recover, and this process of recover takes about d<sub>hosrec</sub> days.
    It takes about d<sub>icurec</sub> days for patients to recover from intensive care treatment, if they recover at all.
    A fraction of ICU patients, given by f<sub>icu</sub>, unfortunately die, and this process takes about d<sub>icud</sub> days.
    </p>
    
    <p>
    Each of the parameters can be marked by a priori constants or distributions, which can be adjusted in the data calibration.
    The Table below lists the adopted parameter values including the prior and posterior parameters used for the case study of the Netherlands.
    </p>
    
    <p>
    The model is strongly data-driven and calibrates prior estimates for a number of model parameters,
    including the reproduction number, strength of social distancing, and some of the treatment times for hospitalization and
    ICU from the observed data (see Table below). For data assimilation of all parameters in conjunction with calibration of &#945;(t), we use the ensemble
    smoother with multiple iterations which is a computationally efficient method for ensembles of non-linear forward models.
    The data assimilation is performed by matching modeled hospitalization and intensive care unit usage to reported usage.
    </p>
    
    <br><h3>Model Parameters</h3><br>
    
    <p>
    The Table below gives an overview of all parameters, which are given a Gaussian prior with mean &mu; and standard deviation &sigma;.
    If they do not have an entry for &sigma;, they are assumed to be constant. Parameters that have an entry for &xi; follow a Gamma
    distribution with a mean sampled from Gaussian(&mu;, &sigma;) and a standard deviation given by &xi;.
    </p>
    "
    
    res
  })
  
  
  output$model_parameters <- renderTable({
    params <- c(
      '$$N$$', '$$m$$', '$$R_0$$', '$$\\sigma$$', '$$\\gamma$$',
      '$$d_{\\text{hos}}$$', '$$d_{\\text{hosd}}$$', '$$d_{\\text{hosrec}}$$',
      '$$d_{\\text{icud}}$$', '$$d_{\\text{icurec}}$$', '$$d_{\\text{rec}}$$',
      '$$f_{\\text{hos}}$$', '$$f_{\\text{icu}}$$', '$$h$$', '$$i(t)$$',
      '$$\\text{CFR}_{\\text{hos}} = f_{\\text{hos}} + i(t) f_{\\text{icu}}$$'
    )
    
    values_mean <- c(
      '$$80000$$', '$$0.90$$', '$$3.20$$', '$$0.20$$', '$$0.50$$', '$$7$$',
      '$$3$$', '$$9$$', '$$11$$', '$$23$$', '$$12$$', '$$-$$', '$$0.30$$',
      '$$0.015$$', '$$0.18 - 1$$', '$$0.22$$'
    )
    
    values_std <- c(
      '$$40000$$', '$$-$$', '$$0.50$$', '$$-$$', '$$-$$', '$$-$$',
      '$$1$$', '$$-$$', '$$-$$', '$$1$$', '$$-$$', '$$-$$', '$$0.02$$',
      '$$0.005$$', '$$-$$', '$$0.03$$'
    )
    
    G <- c(
      '$$-$$', '$$-$$', '$$-$$', '$$-$$', '$$-$$', '$$2$$',
      '$$2$$', '$$4$$', '$$8$$', '$$17$$', '$$-$$', '$$-$$', '$$-$$',
      '$$-$$', '$$-$$', '$$-$$'
    )
    
    unit <- c(
      '$$-$$', '$$-$$', '$$\\text{day}^{-1}$$', '$$\\text{day}^{-1}$$', '$$\\text{day}^{-1}$$',
      '$$\\text{days}$$', '$$\\text{days}$$', '$$\\text{days}$$', '$$\\text{days}$$', '$$\\text{days}$$',
      '$$\\text{days}$$', '$$-$$', '$$-$$', '$$-$$', '$$-$$', '$$-$$'
    )
    
    description <- c(
      '$$\\text{1/N is the starting fraction of exposed in SEIR model}$$',
      '$$\\text{Fraction of total population susceptible to COVID-19}$$',
      '$$\\text{Initial reproduction number}$$',
      '$$\\text{Incubation time of 5 days}$$',
      '$$\\text{Removal rate of infected people in self quarantine}$$',
      
      '$$\\text{Days of illness before hospitalization}$$',
      '$$\\text{Days of hospital treatment for mortalities}$$',
      '$$\\text{Days of hospital treatment for recovery}$$',
      '$$\\text{Days of ICU treatment for mortalities (estimated from NICE data)}$$',
      '$$\\text{Days of ICU treatment for recoverable case (estimated from NICE data)}$$',
      '$$\\text{Days required for recovery of mild cases}$$',
      
      '$$f_{\\text{hos}} = \\text{CFR}_{\\text{hos}} - i(t) f_{\\text{icu}}$$',
      '$$\\text{Case Fatality Rate (CFR) of ICU patients (estimated from data)}$$',
      '$$\\text{Fraction of hospitalized cases (estimated from under registration and Sanquin study)}$$',
      '$$\\text{Fraction of hospitalized patients in need for IC treatment (fitted on reported rates from hospitalization and ICU with Gaussian smoothing)}$$',
      '$$\\text{Aggregated CFR of hospitalized and ICU cases (estimated from reported mortality rates by NICE)}$$'
    )
    
    df <- data.frame(
      Parameter = params,
      E_mean = values_mean,
      E_std = values_std,
      G = G,
      Unit = unit,
      Description = description
    )
    
    colnames(df) <- c(
      '$$\\text{Parameter}$$', '$$\\mu$$',
      '$$\\sigma$$', '$$\\xi$$',
      '$$\\text{Unit}$$', '$$\\text{Description}$$'
    )
    
    df
    
  }, spacing = 'xs', align = 'cccccl', rownames = FALSE)
  
})
