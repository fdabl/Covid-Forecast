library('shiny')
library('shinythemes')
library('shinydashboard')
  

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
              plotOutput('infectedPlot', height = 500)
            ),
            tabPanel(
              'Hospitalized',
              plotOutput('hospitalizedPlot', height = 500)
            ),
            tabPanel(
              'Intensive Care',
              plotOutput('ICPlot', height = 500)
            ),
            tabPanel(
              'Dead',
              plotOutput('deadPlot', height = 500)
            ),
            tabPanel(
              'Show All',
              plotOutput('allPlot', height = 500)
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
              
              # sliderInput(
              #   'Nseed',
              #   'Initially Infected :',
              #   value = 0.05, min = 0, max = 0.30, step = 0.01
              # ),
              
              actionButton('estimate1', 'Estimate')
            ),
            
            tabPanel(
              'Core II',
              withMathJax(),
              
              sliderInput(
                'R0_range',
                'Uniform Prior on \\( R_0 \\)',
                min = 2.5, max = 4.5,
                value = c(3.3, 3.7)
              ),
              
              # numericInput(
              #   'nr_interventions',
              #   'Number of Interventions',
              #   min = 0, max = 100,
              #   value = 3
              # ),
              
              textInput(
                'days_intervention',
                'Days on which the Interventions were applied',
                value = '5, 7, 13'
              ),
              
              uiOutput('alphas'),
              
              sliderInput(
                'esmda_iterations',
                'Iterations for Multiple Data Assimilation',
                value = 8, min = 2, max = 32
              ),
              
              actionButton('estimate2', 'Estimate')
            ),
            
            tabPanel(
              'Expert',
              
              textInput(
                'dfrac',
                'Gaussian Prior on  % Hospitalized People Dying (\\( \\mu, \\sigma \\)):',
                value = '0.30, 0.50'
              ),
              
              textInput(
                'icudfrac',
                'Gaussian Prior on  % IC Patients Dying:',
                value = '0.30, 0.20'
              ),
              
              textInput(
                'delayHOS',
                'Gaussian Prior on Delay Between Hospitalisation and Recovery (\\( a, b \\))',
                value = '9, 2'
              ),
              
              textInput(
                'delayHOSREC',
                'Gaussian Prior on Hospitalisation Recovery (no IC)',
                value = '12, 0'
              ),
              
              textInput(
                'delayHOSD',
                'Gaussian Prior on Death of Hospitalised (no IC)',
                value = '3, 2'
              ),
              
              textInput(
                'delayICUCAND',
                'Gaussian Prior on Hospitalised to IC',
                value = '0, 0'
              ),
              
              textInput(
                'delayICUD',
                'Gaussian Prior on Time for IC Patients to Die',
                value = '8, 4'
              ),
              
              actionButton('estimate3', 'Estimate')
            ),
            
            tabPanel(
              'Plotting',
              
              dateRangeInput(
                'zoomdate',
                'Zoom in on Dates'
              ),
              
              checkboxInput(
                'ylog',
                'Logarithmic Plot',
                value = FALSE
              ),
              
              actionButton('updatePlot', 'Update Plot')
            )
          )
        )
        
       
      ),
      
      fluidRow(
        column(
          width = 8,
          box(
            title = 'Effect of Interventions', width = NULL, solidHeader = TRUE, status = 'primary',
            plotOutput('interventionPlot', height = 500)
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
            
            # textInput(
            #   'days_intervention_forecast',
            #   'Days of Interventions',
            #   value = '25'
            # ),
            
            uiOutput('intervention'),
            # uiOutput('intervention_dates'),
            # uiOutput('alphas_intervention'),
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