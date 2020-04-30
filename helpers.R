library('jsonlite')
library('reticulate')

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

source_python('source.py')
source_python('bin/dashboard_wrapper.py')
# config <- fromJSON('config.json')
# res <- run_esmda(toJSON(config, auto_unbox = TRUE))

# - speed up computation (factor of 5)
# - make single run computations for interventions


# time, mean, p5, p30, p50, p70, p95, observed

# https://community.rstudio.com/t/python-virtual-environment-in-r-shinyapp-io-stopped-working-suddenly/62561/2
# https://community.rstudio.com/t/using-cron-to-import-data-daily-and-update-shiny-app/24217/4
# https://community.rstudio.com/t/running-code-in-shiny-periodically/27624


plot_predictions <- function(config, res, type) {
  cols <- brewer.pal(n = 3, 'Set1')
  dat <- res[[type]]
  colnames(dat) <- c('Time', 'Mean', 'p5', 'p30', 'p50', 'p70', 'p95', 'Observed')
  dat <- data.frame(dat)
  start <- config[['startdate']]
  
  time <- dat[, 1]
  p5 <- dat[, 3]
  p95 <- dat[, 7]
  
  ared <- adjustcolor(cols[1], 0.50)
  
  ggplot(dat, aes(x = Time, y = Mean)) +
    geom_line(color = cols[1]) +
    geom_line(aes(y = p5), color = ared) +
    geom_line(aes(y = p95), color = ared) +
    geom_point(aes(y = Observed)) +
    theme_bw()
  
  # plot(
  #   time, dat[, 2], type = 'l', lwd = 2, axes = FALSE,
  #   xlab = 'Time', ylab = type, col = cols[1]
  # )
  # 
  # lines(time, dat[, 3], col = adjustcolor(cols[1], .5))
  # lines(time, dat[, 7], col = adjustcolor(cols[1], .5))
  # points(time, dat[, 8], pch = 20)
  # 
  # axis(1)
  # axis(2, las = 2)
}


create_config <- function(input) {
  json <- list(
    'worldfile' = FALSE,
    'country' = 'res/corona_dataNL_april14.txt', # TODO: Do not hardcode
    'dt' = 0.1,
    't_max' = 150,
    'startdate' = '3/1/20',
    'time_delay' = 12,
    'population' = 17e6,
    'nr_prior_samples' = 50,
    'nr_forecast_samples' = 50,
    'esmda_iterations' = input$esmda_iterations,
    'N' = list(
      'type' = 'uniform',
      'min' = 50000,
      'max' = 300000
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
      'stddev'= input$delayHOSD_sd
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
    
    'hosfrac' = input$hosfrac,
    
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
    # 'calibration_mode' = c(4, 5, 2),
    # 'observation_error' = c(50.0, 25.0, 100.0),
    
    # 'calibration_mode' = c(4, 5),
    'calibration_mode' = c('hospitalizedcum', 'ICU'),
    'observation_error' = c(100.0, 50.0),
    'hist_time_steps' = c(30, 35, 40, 60),
    'p_values' =  c(0.05, 0.3, 0.5, 0.7, 0.95),
    
    'alpha' = list(
      c(0.1,0.5), c(0.1,0.5), c(0.3,0.9), c(0.3,0.9), c(0.3,0.9), c(0.3,0.9), c(0.6,0.95), c(0.6,0.95), c(0.7,0.95),
      c(0.7,0.95),  c(0.6,0.95), c(0.6,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95)
    ),
    'dayalpha' = c(8, 12, 15, 18, 19, 20, 21, 22, 23, 24, 26, 28, 30, 32, 34, 36),

    'icufracfile' =  'res/corona_dataNL_icufrac_april14.txt'
  )
  
  toJSON(json, pretty = TRUE, auto_unbox = TRUE)
}
