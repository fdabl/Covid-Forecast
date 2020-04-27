library('reticulate')

# randn_py <- runif

# py_install(c('numpy', 'matplotlib'))
# Setup Python Environment
# virtualenv_create(envname = 'python_env', python = 'python3')
# # virtualenv_install('python_env', packages = c('numpy', 'matplotlib'), ignore_installed = TRUE)
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'pip==19.0'), ignore_installed = TRUE)
# use_virtualenv('python_env', required = TRUE)

source_python('source.py')

# https://community.rstudio.com/t/python-virtual-environment-in-r-shinyapp-io-stopped-working-suddenly/62561/2
# https://community.rstudio.com/t/using-cron-to-import-data-daily-and-update-shiny-app/24217/4
# https://community.rstudio.com/t/running-code-in-shiny-periodically/27624


create_config <- function(input) {

  json <- list(
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
    'alpha' = c(),
    'dayalpha' = c(),
    
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
      'stddev' = input$delayICUCAND_stdev,
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
    'calibration_mode' = c(4, 5, 2),
    'observation_error' = c(50.0, 25.0,100.0),
    'hist_time_steps' = c(30, 35, 40, 60),
    'p_values' =  c(0.05, 0.3, 0.5, 0.7, 0.95)
  )
  
  json

    # 'alpha' : [
    #   [0.1,0.5], [0.1,0.5], [0.3,0.9], [0.3,0.9], [0.3,0.9], [0.3,0.9], [0.6,0.95], [0.6,0.95], [0.7,0.95],
    #   [0.7,0.95],  [0.6,0.95], [0.6,0.95], [0.5,0.95], [0.5,0.95], [0.5,0.95], [0.5,0.95]
    # ],
    # 'dayalpha' : [8, 12, 15, 18, 19, 20, 21, 22, 23, 24, 26, 28, 30, 32, 34, 36],
    
    # 'icufracfile':  '../res/corona_dataNL_icufrac_april14.txt',
  # }
}