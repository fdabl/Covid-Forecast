library('jsonlite')
library('reticulate')
library('RColorBrewer')

# Interventions for The Netherlands
ALPHAS <- list(
  c(0.1,0.5), c(0.1,0.5), c(0.3,0.9), c(0.3,0.9), c(0.3,0.9),
  c(0.3,0.9), c(0.6,0.95), c(0.6,0.95), c(0.7,0.95), c(0.7,0.95),
  c(0.6,0.95), c(0.6,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95)
)

DAYALPHAS <- c(8, 12, 15, 18, 19, 20, 21, 22, 23, 24, 26, 28, 30, 32, 34, 36)


# randn_py <- runif
# use_condaenv("r-reticulate")
# use_python(Sys.which("python3"))
use_python('/anaconda3/bin/python3')

# py_install(c('numpy', 'matplotlib'))
# Setup Python Environment
# virtualenv_create(envname = 'python_env', python = 'python3')
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'requests'), ignore_installed = TRUE)
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'pip==19.0'), ignore_installed = TRUE)
# use_virtualenv('python_env', required = TRUE)

# source_python('source.py')
source_python('bin/dashboard_wrapper.py')
# source_python('Covid-SEIR/bin/corona_esmda.py')
# source_python('Covid-SEIR/src/api_nl_data.py')

# config <- fromJSON('Covid-SEIR/configs/netherlands_dashboard.json')
# config <- fromJSON('config.json')
# res <- run_dashboard_wrapper(toJSON(config, auto_unbox = TRUE))
# 
# config2 <- config
# config2$single_run <- TRUE
# res2 <- run_dashboard_wrapper(toJSON(config2, auto_unbox = TRUE))

# - give you updates json (with posterior means and sds) and pass this to single-run
# - make a button for the hammer


# time, mean, p5, p30, p50, p70, p95, observed

# https://community.rstudio.com/t/python-virtual-environment-in-r-shinyapp-io-stopped-working-suddenly/62561/2
# https://community.rstudio.com/t/using-cron-to-import-data-daily-and-update-shiny-app/24217/4
# https://community.rstudio.com/t/running-code-in-shiny-periodically/27624


merge_runs <- function(model_data, single_data) {
  names <- c('infected', 'hospitalized', 'ICU', 'dead')
  
  res <- list()
  
  for (i in seq(length(names))) {
    name <- names[i]
    res[[name]] <- cbind(model_data[[name]], single_data[[name]][, 2])
  }
  
  res
}

plot_predictions <- function(config, model, type, cols, ylab, title, show_intervention = FALSE) {
  res <- model$data
  dat <- res[[type]]
  colnames(dat) <- c('Time', 'Mean', 'p5', 'p30', 'p50', 'p70', 'p95', 'Observed')
  dat <- data.frame(dat)
  
  start <- as.Date(config[['startdate']], tryFormats = '%m/%d/%y')
  dat$Date <- start + dat$Time - 1
  
  p <- ggplot(dat, aes(x = Date, y = Mean)) +
    geom_ribbon(aes(ymin = p5, ymax = p95, fill = '90% CI'), alpha = 0.50) +
    geom_ribbon(aes(ymin = p30, ymax = p70, fill = '40% CI'), alpha = 0.75) +
    geom_point(aes(y = Observed)) +
    geom_line(aes(color = 'Mean'), size = 0.5) +
    geom_line(aes(y = p50, color = 'Median'), size = 0.5) +
    ggtitle(title) +
    ylab(ylab) +
    scale_colour_manual(
      name = '',
      values = c('Mean' = 'black', 'Median' = 'gray76'),
      labels = c('Mean','Median')
    ) +
    scale_fill_manual(
      name = '',
      values = c('90% CI' = cols[1], '40% CI' = cols[2])
    ) +
    theme_bw() + 
    theme(
      legend.position = 'top',
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, hjust = 0.50),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
  
  if (show_intervention) {
    preddat <- model$single_data[[type]]
    preddat <- data.frame('Time' = preddat[, 1], 'Mean' = preddat[, 2])
    preddat$Date <- start + preddat$Time - 1
    
    cols <- brewer.pal(3, 'Set1')
    p <- p + geom_line(data = preddat, aes(x = Date, y = Mean), color = cols[1])
  }
  
  p #+ scale_x_date(limits = c(startdate, startdate + 4 * 30), breaks = scales::pretty_breaks(n = 8))
}

plot_interventions <- function(config, model, cols, ylab, title, show_intervention = FALSE) {
  
  res <- model$data
  alpha <- res[['alpha']][['posterior']]
  
  dat <- cbind(alpha[, 1], 1 - alpha[, -1])
  colnames(dat) <- c('Time', 'p5', 'p30', 'p50', 'p70', 'p95')
  dat <- data.frame(dat)
  
  start <- as.Date(config[['startdate']], tryFormats = '%d/%m/%y')
  dat$Date <- start + dat$Time - 1
  
  p <- ggplot(dat, aes(x = Date, y = p50)) +
    geom_ribbon(aes(ymin = p5, ymax = p95, fill = '90% CI'), alpha = 0.50) +
    geom_ribbon(aes(ymin = p30, ymax = p70, fill = '40% CI'), alpha = 0.75) +
    # geom_line(aes(color = 'Mean'), size = 0.5) +
    geom_line(aes(y = p50, color = 'Median'), size = 0.5) +
    ggtitle(title) +
    ylab(ylab) +
    scale_colour_manual(
      name = '',
      values = c('Mean' = 'black', 'Median' = 'gray76'),
      labels = c('Mean','Median')
    ) +
    scale_fill_manual(
      name = '',
      values = c('90% CI' = cols[1], '40% CI' = cols[2])
    ) +
    theme_bw() + 
    theme(
      legend.position = 'top',
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, hjust = 0.50),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )

  if (show_intervention) {
    preddat <- model$single_data[['alpha']]
    preddat <- data.frame('Time' = preddat[, 1], 'Mean' = preddat[, 2])
    preddat$Date <- start + preddat$Time - 1

    cols <- brewer.pal(3, 'Set1')
    p <- p + geom_line(data = preddat, aes(x = Date, y = Mean), color = cols[1])
  }
  
  p #+ scale_x_date(limits = c(startdate, startdate + 4 * 30), breaks = scales::pretty_breaks(n = 8))
}



create_config <- function(input, single_run = FALSE) {
  
  # If user shows the alphas, use the alpha input
  # Otherwise use the global variables (defined above)
  # The global variables are also what is shown as default input
  if (input$show_alpha) {
    alphas_prior <- paste0('alpha_', seq(input$nr_interventions))
    dayalphas_prior <- paste0('day_', seq(input$nr_interventions))

    ALPHAS <- sapply(alphas_prior, function(alpha) 1 - input[[alpha]])
    DAYALPHAS <- sapply(dayalphas_prior, function(day) input[[day]])
  }

  # If the user has intervened (single_run = TRUE), add the intervention alphas and
  # the days on which the intervention took place to ALPHAS and DAYALPHAS
  if (single_run) {
    print('has intervened!')
    alphas_inter <- paste0('alpha_intervention_', seq(input$nr_interventions_forecast))
    dayalphas_inter <- paste0('day_intervention_', seq(input$nr_interventions_forecast))

    ALPHAS_INTER <- lapply(alphas_inter, function(alpha) c(1 - input[[alpha]], 1 - input[[alpha]] + 0.10))
    DAYALPHAS_INTER <- sapply(dayalphas_inter, function(day) input[[day]])

    startdate <- as.Date('3/1/20', tryFormats = '%m/%d/%y')

    ALPHAS <- c(ALPHAS, ALPHAS_INTER)
    DAYALPHAS <- c(DAYALPHAS, DAYALPHAS_INTER - as.numeric(startdate))

    print(input[['day_intervention_1']])
    # print(ALPHAS)
    # print(DAYALPHAS)
  }
  
  json <- list(
    'worldfile' = FALSE,
    'country' = 'res/corona_dataNL_main.txt', # TODO: Do not hardcode
    'dt' = 0.1,
    't_max' = 360,
    'startdate' = '3/1/20',
    'time_delay' = 12,
    'population' = 17e6,
    'nr_prior_samples' = 50,
    'nr_forecast_samples' = 1500,
    'esmda_iterations' = input$esmda_iterations,
    'N' = list(
      'type' = 'uniform',
      'min' = 25000,
      'max' = 150000
    ),

    'sigma' = 0.20,
    'gamma' = 0.50,
    'R0' = list(
      'type' = 'normal',
      'mean' = input$R0_mean,
      'stddev' = input$R0_sd
    ),
    
    'm' = 0.9, # TODO: What is this?
    
    'delayHOS' = list(
      'type' = 'normal',
      'mean'= input$delayHOS_mean,
      'stddev'= input$delayHOS_sd,
      'smooth_sd'= 3
    ),
    
    'delayHOSREC' = list(
      'type' = 'normal',
      'mean' = input$delayHOS_mean,
      'stddev' = input$delayHOSREC_sd,
      'smooth_sd'= 4
    ),
    
    'delayHOSD' = list(
      'type' = 'normal',
      'mean' = input$delayHOSD_mean,
      'stddev'= input$delayHOSD_sd,
      'smooth_sd' = 2,
      'smooth_sd_sd' = 1
    ),
    
    'delayREC' = input$delayREC,
    
    'delayICUCAND' = list(
      'type' = 'normal',
      'mean' = input$delayICUCAND_mean,
      'stddev' = input$delayICUCAND_sd,
      'smooth_sd' = 0,
      'smooth_sd_sd' = 0
    ),
    
    'delayICUD' = list(
      'type' = 'normal',
      'mean' = input$delayICUD_mean,
      'stddev' = input$delayICUD_sd,
      'smooth_sd' = 5,
      'smooth_sd_sd' = 2
    ),
    
    'delayICUREC' = list(
      'type' = 'normal',
      'mean' = input$delayICUREC_mean,
      'stddev' = input$delayICUREC_sd,
      'smooth_sd' = 15,
      'smooth_sd_sd' = 0
    ),
    
    'hosfrac' = list(
      'type' = 'normal',
      'mean' = input$hosfrac,
      'stddev' = 0.01
    ),
    
    'dfrac' = list(
      'type' = 'normal',
      'mean' = input$dfrac_mean,
      'stddev' = input$dfrac_sd
    ),
    
    'icudfrac' = list(
      'type' = 'normal',
      'mean' = input$icudfrac_mean,
      'stddev' = input$icudfrac_sd
    ),
    
    'ICufrac' = input$ICUfrac,
    
    'calibration_mode' = c('hospitalizedcum', 'ICU'),
    'observation_error' = c(100.0, 50.0),
    'hist_time_steps' = c(30, 35, 40, 60),
    'p_values' =  c(0.05, 0.3, 0.5, 0.7, 0.95),
    
    'alpha' = ALPHAS,
    'dayalpha' = DAYALPHAS,
    
    'icufracscale' = list(
      'type' = 'normal',
      'mean' = 1,
      'stddev' = 0.1
    ),

    'icufracfile' =  '../output/netherlands_dashboard_icufrac.txt',
    'icdatafile' = 'res/icdata_main.txt',
    'single_run' = single_run,
    'output_base_filename' = 'netherlands_dashboard',
    
    'YMAX' = 150e3,
    'XMAX' = 240,
    'plot' = list(
      'legendloc' = 'best',
      'legendloczoom' = 'lower left',
      'legendfont' = 'x-small',
      'y_axis_log' = FALSE,
      'hindcast_plume' = TRUE,
      'xmaxalpha' = 240,
      'casename' = 'Netherlands',
      'daily' =  FALSE,
      'figure_size' = c(10.0, 4.0)
    )
  )
  
  toJSON(json, pretty = TRUE, auto_unbox = TRUE)
}
