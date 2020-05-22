library('shiny')
library('plotly')
library('shinythemes')
library('shinydashboard')
library('dashboardthemes')
library('shinycssloaders')
  

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Introduction', tabName = 'introduction', icon = icon('file-alt')),
    menuItem('Model Exploration', tabName = 'estimation', icon = icon('dashboard')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = 'grey_light'
  ),
  
  tabItems(
    tabItem(
      tabName = 'introduction',
      fluidPage(
        fluidRow(
          box(
            width = 1000,
            htmlOutput('introduction')
          )
        )
      ),
      box(
        width = 1000,
        htmlOutput('structure')
      )
    ),
    
    tabItem(
      tabName = 'estimation',
      fluidRow(
        column(
          width = 8,
          tabBox(
            width = NULL,
            id = 'datafigures',
            tabPanel(
              'Infected',
              withSpinner(plotOutput('infectedPlot', height = 500), color = '#0dc5c1')
            ),
            tabPanel(
              'Hospitalized',
              withSpinner(plotOutput('hospitalizedPlot', height = 500), color = '#0dc5c1')
            ),
            tabPanel(
              'Intensive Care',
              withSpinner(plotOutput('ICPlot', height = 500), color = '#0dc5c1')
            ),
            tabPanel(
              'Dead',
              withSpinner(plotOutput('deadPlot', height = 500), color = '#0dc5c1')
            ),
            tabPanel(
              'Show All',
              withSpinner(plotOutput('allPlot', height = 500), color = '#0dc5c1')
            )
          )
        ),
        
        column(
          width = 4,
          tabBox(
            width = NULL,
            id = 'tabset1', title = '',
            tabPanel(
              'Core I',
              withMathJax(),
              
              # dateInput(
              #   'startdate',
              #   'Start of Epidemic',
              #   value = '2020-02-01'
              # ),
                
              selectInput(
                'calibration',
                'Calibrate Model on:',
                choices = list('ICU' = 1, 'Dead' = 2, 'Hospitalized' = 3, 'Cumulative Hospitalized' = 4),
                selected = 2
              ),
              
              numericInput(
                'delayREC',
                'Recovery of Non-Hospitalised Patients',
                value = 12
              ),
              
              sliderInput(
                'hosfrac',
                '% Infected People Hospitalized (Average):',
                value = 0.05, min = 0, max = 0.30, step = 0.01
              ),
              
              sliderInput(
                'ICUfrac',
                '% Hospitalized People Needing IC:',
                value = 0.05, min = 0, max = 0.30, step = 0.01
              ),
              
              sliderInput(
                'esmda_iterations',
                'Model Iterations',
                value = 4, min = 2, max = 32
              ),
              
              actionButton('run1', 'Run')
            ),
            
            tabPanel(
              'Core II',
              withMathJax(),
              
              tags$b('Prior on \\( R_0 \\)'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'R0_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 3.40, min = 0, max = 5, step = 0.01
                ), 
                numericInput(
                  'R0_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.20, min = 0, max = 2, step = 0.01
                )
              ),
              
              numericInput(
                'nr_interventions',
                'Number of Interventions',
                min = 0, max = 100,
                value = 16
              ),
              
              checkboxInput(
                'show_alpha',
                'Show Past Interventions',
                value = FALSE
              ),
              uiOutput('prior_intervention'),
              actionButton('run2', 'Run')
            ),
            
            tabPanel(
              'Expert',
              
              tags$b('Prior on % Hospitalized People Dying'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'dfrac_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0.30, min = 0, max = 1, step = 0.01
                ), 
                numericInput(
                  'dfrac_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.50, min = 0, max = 2, step = 0.01
                )
              ),
              
              tags$b('Prior on % IC Patients Dying'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'icudfrac_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0.30, min = 0, max = 1, step = 0.01
                ), 
                numericInput(
                  'icudfrac_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.20, min = 0, max = 2, step = 0.01
                )
              ),
              
              tags$b('Prior on Delay Between Hospitalisation and Recovery'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayHOS_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 9, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayHOS_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 2, min = 0, max = 10, step = 0.01
                )
              ),
              
              tags$b('Prior on Hospitalisation Recovery (no IC)'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayHOSREC_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 12, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayHOSREC_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01
                )
              ),
              
              tags$b('Prior on Death of Hospitalised (no IC)'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayHOSD_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 3, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayHOSD_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 2, min = 0, max = 10, step = 0.01
                )
              ),
              
              tags$b('Prior on Hospitalised to IC'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayICUCAND_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayICUCAND_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01
                )
              ),
              
              tags$b('Prior on Time for IC Patients to Die'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayICUD_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 8, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayICUD_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 4, min = 0, max = 10, step = 0.01
                )
              ),
              
              tags$b('Prior on Time for IC Patients to Recover'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'delayICUREC_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 22, min = 0, max = 50, step = 0.01
                ), 
                numericInput(
                  'delayICUREC_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 2, min = 0, max = 10, step = 0.01
                )
              ),
              
              actionButton('run3', 'Run')
            )
            
            # tabPanel(
            #   'Plotting',
            #   
            #   dateRangeInput(
            #     'zoomdate',
            #     'Zoom in on Dates'
            #   ),
            #   
            #   checkboxInput(
            #     'ylog',
            #     'Logarithmic Plot',
            #     value = FALSE
            #   ),
            #   
            #   actionButton('updatePlot', 'Update Plot')
            # )
          )
        )
        
       
      ),
      
      fluidRow(
        column(
          width = 8,
          box(
            title = 'Effect of Interventions', width = NULL, solidHeader = TRUE, status = 'primary',
            withSpinner(plotOutput('interventionPlot', height = 500), color = '#0dc5c1')
          )
        ),
        
        column(
          width = 4,
          box(
            status = 'warning', width = NULL,
            title = 'Intervention Settings',
            footer = 'Having estimated the model on data, here you can see the effect of different interventions.
                      These can be in the past to answer \'what if\' questions, or in the future.',
            
            numericInput(
              'nr_interventions_forecast',
              'Number of Interventions',
              min = 0, max = 100,
              value = 1
            ),
            
            uiOutput('intervention'),
            actionButton('intervene', 'Intervene'),
            br(),
            br(),
            actionButton('reset', 'Reset Intervention')
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(
          width = 1000,
          HTML(
            "<h3 style = 'text-align: center;'>About</h3>
          <p style = 'font-size: 120%; text-align: center;'>
          This web interface was developed by <a href='https://twitter.com/fdabl' target='_blank'>Fabian Dablander</a>
          in co-operation with <a href='https://nl.linkedin.com/in/lgbrunner' target='_blank'>Logan Brunner</a> and
          <a href='https://www.uu.nl/staff/jdamvanwees' target='_blank'>Jan-Diederik van Wees</a> as a
          <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project
          <p>"
          )
        )
      )
    )
  )
)


dashboardPage(
  dashboardHeader(title = 'Forecasting COVID-19 Hospitalization', titleWidth = 350),
  sidebar,
  body
)