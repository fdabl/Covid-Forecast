library('shiny')
library('plotly')
library('shinythemes')
library('shinydashboard')
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
            # title = 'Model Forecasts',
            tabPanel(
              'Infected',
              plotlyOutput('infectedPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
            ),
            tabPanel(
              'Hospitalized',
              plotOutput('hospitalizedPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
            ),
            tabPanel(
              'Intensive Care',
              plotOutput('ICPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
            ),
            tabPanel(
              'Dead',
              plotOutput('deadPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
            ),
            tabPanel(
              'Show All',
              plotOutput('allPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
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
                '% Infected People Hospitalized:',
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
                value = 8, min = 2, max = 32
              ),
              
              actionButton('estimate1', 'Estimate')
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
                value = 3
              ),
              
              checkboxInput(
                'show_alpha',
                'Show Past Interventions',
                value = FALSE
              ),
              uiOutput('prior_intervention'),
              actionButton('estimate2', 'Estimate')
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
              
              actionButton('estimate3', 'Estimate')
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
            plotOutput('interventionPlot', height = 500) %>% withSpinner(color = '#0dc5c1')
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
            actionButton('intervene', 'Intervene')
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(width = 1000,
          h3('About'),
          p(
           'This Web App was developed by Fabian Dablander from https://scienceversuscorona.com in cooperation with TNO.' 
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