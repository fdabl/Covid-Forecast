library('jsonlite')
library('reticulate')
library('RColorBrewer')

# https://github.com/rstudio/rsconnect/issues/359
# https://github.com/ranikay/shiny-reticulate-app

# Interventions for The Netherlands
ALPHAS <- list(
  c(0.1,0.5), c(0.1,0.5), c(0.3,0.9), c(0.3,0.9), c(0.3,0.9),
  c(0.3,0.9), c(0.6,0.95), c(0.6,0.95), c(0.7,0.95), c(0.7,0.95),
  c(0.6,0.95), c(0.6,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95), c(0.5,0.95)
)

DAYALPHAS <- c(8, 12, 15, 18, 19, 20, 21, 22, 23, 24, 26, 28, 30, 32, 34, 36)

LINESIZE <- 0.75
INTERVENTION_COLOR <- '#ADADAD'


# use_condaenv("r-reticulate")
# use_python('/usr/bin/python3')
# py_install(c('numpy', 'matplotlib'))
# use_python('/anaconda3/bin/python3')

# Setup Python Environment
# system('apt-get install python3-tk')
# virtualenv_create(envname = 'python_env', python = 'python3')
# virtualenv_remove(envname = "python_env", packages = "pip")
# print(py_discover_config())
# virtualenv_install('python_env', packages = c('numpy', 'h5py', 'scipy', 'tqdm', 'requests', 'lxml', 'selenium'))#, 'matplotlib==1.5.3'))
# virtualenv_install('python_env', packages = c('pip==19.0.3', 'numpy', 'matplotlib'))
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'requests'), ignore_installed = TRUE)
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'pip==19.0'), ignore_installed = TRUE)
use_virtualenv('python_env', required = TRUE)

# source_python('source.py')
source_python('bin/dashboard_wrapper.py')
# source_python('Covid-SEIR/bin/corona_esmda.py')
# source_python('Covid-SEIR/src/api_nl_data.py')
# 
# config <- fromJSON('Covid-SEIR/configs/netherlands_dashboard.json')
# config <- fromJSON('config.json')
# res <- run_dashboard_wrapper(toJSON(config, auto_unbox = TRUE))
# # 
# config2 <- config
# config2$single_run <- FALSE
# res2 <- run_dashboard_wrapper(toJSON(config2, auto_unbox = TRUE))

# - give you updates json (with posterior means and sds) and pass this to single-run
# - make a button for the hammer


# time, mean, p5, p30, p50, p70, p95, observed

# https://community.rstudio.com/t/python-virtual-environment-in-r-shinyapp-io-stopped-working-suddenly/62561/2
# https://community.rstudio.com/t/using-cron-to-import-data-daily-and-update-shiny-app/24217/4
# https://community.rstudio.com/t/running-code-in-shiny-periodically/27624


plot_predictions <- function(
  config, model, type, cols, ylab, title,
  show_intervention = FALSE
) {
  
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
    geom_line(aes(color = 'Mean'), size = LINESIZE) +
    geom_line(aes(y = p50, color = 'Median'), size = LINESIZE) +
    ggtitle(title) +
    ylab(ylab) +
    scale_colour_manual(
      name = '',
      values = c('Mean' = 'black', 'Median' = 'gray76'),
      labels = c('Mean', 'Median')
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
    
    # Hack to make the plotting of the intervention line work for infected
    if (type == 'infected') {
      type <- 'infected_cum'
    }
    
    preddat <- model$single_data[[type]]
    dat$Mean <- preddat[, 2]
    
    p <- p + 
      geom_line(data = dat, aes(x = Date, y = Mean, color = 'Intervention'), size = LINESIZE) +
      geom_ribbon(
        data = dat,
        aes(ymin = Mean * (1 - (p50 - p5) / p50), ymax = Mean * (1 + (p95 - p50) / p50)),
        alpha = 0.50, fill = INTERVENTION_COLOR
      ) +
      scale_colour_manual(
        name = '',
        values = c('Intervention' = INTERVENTION_COLOR, 'Mean' = 'black', 'Median' = 'gray76'),
        labels = c('Intervention', 'Mean', 'Median')
      )
  }
  
  p + guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    )
    #+ scale_x_date(limits = c(startdate, startdate + 4 * 30), breaks = scales::pretty_breaks(n = 8))
}


plot_interventions <- function(config, model, cols, ylab, title, show_intervention = FALSE) {
  
  res <- model$data
  alpha <- res[['alpha']][['posterior']]
  
  dat <- cbind(alpha[, 1], 1 - alpha[, -1])
  colnames(dat) <- c('Time', 'p5', 'p30', 'p50', 'p70', 'p95')
  dat <- data.frame(dat)
  
  start <- as.Date(config[['startdate']], tryFormats = '%m/%d/%y')
  dat$Date <- start + dat$Time - 1
  
  p <- ggplot(dat, aes(x = Date, y = p50)) +
    geom_ribbon(aes(ymin = p5, ymax = p95, fill = '90% CI'), alpha = 0.50) +
    geom_ribbon(aes(ymin = p30, ymax = p70, fill = '40% CI'), alpha = 0.75) +
    geom_line(aes(color = 'Mean'), size = LINESIZE) +
    geom_line(aes(y = p50, color = 'Median'), size = LINESIZE) +
    ggtitle(title) +
    ylab(ylab) +
    scale_colour_manual(
      name = '',
      values = c('Mean' = 'black', 'Median' = 'gray76'),
      labels = c('Mean', 'Median')
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
    # preddat <- data.frame('Time' = preddat[, 1], 'Mean' = preddat[, 2])
    # preddat$Date <- start + preddat$Time - 1
    dat$Mean <- preddat[, 2]

    p <- p +
      geom_line(data = dat, aes(x = Date, y = Mean, color = 'Intervention')) +
      geom_ribbon(
        data = dat,
        aes(ymin = Mean * (1 - (p50 - p5) / p50), ymax = Mean * (1 + (p95 - p50) / p50)),
        alpha = 0.50, fill = INTERVENTION_COLOR, size = LINESIZE
      ) +
      scale_colour_manual(
        name = '',
        values = c('Intervention' = INTERVENTION_COLOR, 'Mean' = 'black', 'Median' = 'gray76'),
        labels = c('Intervention', 'Mean', 'Median')
      )
  }
  
  p + guides(
    colour = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )
}


plot_all <- function(data, model, has_intervened) {
  p1 <- plot_predictions(
    data, model, 'infected', c('#FFE4E1', '#F08080'),
    'Confirmed Cases', 'Cumulative Confirmed Cases', has_intervened
  ) + scale_y_continuous(n.breaks = 5)
  
  p2 <- plot_predictions(
    data, model, 'hospitalizedcum', c('#B0E0E6', '#4682B4'),
    'Hospitalized Cases', 'Cumulative Hospitalized Cases', has_intervened
  ) + scale_y_continuous(n.breaks = 5)
  
  p3 <- plot_predictions(
    data, model, 'ICU', c('#FFDAB9', '#F4A460'),
    'Intensive Care Cases', 'Intensive Care Cases', has_intervened
  ) + scale_y_continuous(n.breaks = 5)
  
  p4 <- plot_predictions(
    data, model, 'dead', c('#C0C0C0', '#808080'),
    'Mortalities', 'Cumulative Mortalities', has_intervened
  ) + scale_y_continuous(n.breaks = 5)
  
  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
}


create_config <- function(input, posterior_alphas = NULL, single_run = FALSE) {
  
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
    alphas_inter <- paste0('alpha_intervention_', seq(input$nr_interventions_forecast))
    dayalphas_inter <- paste0('day_intervention_', seq(input$nr_interventions_forecast))

    ALPHAS_INTER <- lapply(alphas_inter, function(alpha) c(1 - input[[alpha]], 0.10))
    DAYALPHAS_INTER <- sapply(dayalphas_inter, function(day) input[[day]])

    startdate <- as.Date('3/1/20', tryFormats = '%m/%d/%y')

    # Add intervention alphas
    # ALPHAS <- c(posterior_alphas, ALPHAS_INTER)
    # DAYALPHAS <- c(DAYALPHAS, DAYALPHAS_INTER - as.numeric(startdate))
    
    ALPHAS <- posterior_alphas
    # ALPHAS <- posterior_alphas
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
    
    'calibration_mode' = c('dead', 'ICU'),
    'observation_error' = c(100.0, 50.0),
    'hist_time_steps' = c(30, 35, 40, 60),
    'p_values' =  c(0.05, 0.3, 0.5, 0.7, 0.95),
    
    'alpha_normal' = single_run,
    'alpha' = ALPHAS,
    'dayalpha' = DAYALPHAS,
    
    'icufracscale' = list(
      'type' = 'normal',
      'mean' = 1,
      'stddev' = 0.1
    ),

    'icufracfile' =  'output/netherlands_dashboard_icufrac.txt',
    'icufracfile' =  '../bin/output/netherlands_dashboard_icufrac.txt',
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
