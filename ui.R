library('shiny')
library('plotly')
library('shinythemes')
library('shinydashboard')
library('dashboardthemes')
library('shinycssloaders')


IMGWIDTH <- '100%'
IMGHEIGHT <- '550px'
  

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Introduction', tabName = 'introduction', icon = icon('file-alt')),
    menuItem('Interactive Exploration', tabName = 'estimation', icon = icon('dashboard')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = 'grey_light'
  ),
  
  tabItems(
    tabItem(
      withMathJax(),
      tags$style(type="text/css", ".explanation_plot{text-align: center;} p{font-size: 120%;}"),
      tags$style(type="text/css", "p{font-size: 130%;}"),
      tags$style(type="text/css", "#image img {max-width: 100%; width: 100%; height: auto}"),
      tags$head(tags$style(type = "text/css",  "#model_parameters{ overflow-x: scroll; margin-top: 2em;}")),
      tabName = 'introduction',
      fluidPage(
        withMathJax(),
        fluidRow(
          box(
            width = NULL,
            tags$h1(
              'COVID-19 in The Netherlands: Exploring Forecasts and Interventions', align = 'center'
            ),
            
            tags$br(),
            HTML(
             "
             <p>
             This web app allows you to interactively explore observed and forecasted number of confirmed COVID-19 cases,
             hospitalizations, patients in intensive care units, and mortalities. The data is from the Netherlands and is updated daily from
             <a href='https://www.stichting-nice.nl/' target='_blank'>NICE</a>. The forecasts calibrated to the data
             are based on an efficient ensemble based SEIR model described in
             Van Wees et al. (<a href='https://research.vumc.nl/en/publications/forecasting-hospitalization-and-icu-rates-of-the-covid-19-outbrea' target='_blank'>2020a</a>, <a href='https://www.medrxiv.org/content/10.1101/2020.05.16.20102947v1' target='_blank'>2020b</a>).
             </p>
             
             <p>
             Models can be tools that help us make sense of certain aspects of reality, and allow us to answer
             <i>what if?</i> questions. The quality of the answers depends on a number of factors, many of which
             themselves are uncertain in the case of COVID-19. Thus, no model should be taken too seriously; the purpose
             of this app is not to aid decision-makers, but for users to get a feeling for the potential effect of interventions.
             For a blog post introducing this app, see
             <a href='https://scienceversuscorona.com/interactive-exploration-of-covid-19' target='_blank'>here</a>.
             </p>
             " 
            )
          )
        )
      ),
      
      box(
        width = NULL,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        title = 'Model Predictions',
        column(
          width = 5,
          HTML(
            "
            <p>
            The figure on the right shows the raw data (black dots) and the model fit for cumulative confirmed cases
            of COVID-19 (top left), cumulative hospitalized cases (top right), intensive care cases (bottom left), anda
            cumulative mortalities (bottom right). The coloured solid lines show the posterior median of the model estimate,
            while the shaded ribbons indicate the 40% and 90% credible intervals, respectively.
            </p>
            
            <p>
            The model gives a good fit to all data except the cumulative confirmed cases. This is as expected: since we are not testing
            all citizens, the reported cases are a considerable underestimate of the true number of infections. The model also seems to slightly
            understimate the mortalities in the early stages of the epidemic.
            </p>
            "
          )
        ),
        column(
          width = 7,
          withSpinner(plotOutput('explanation_plot_predictions', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
        )
      ),
      
      box(
        width = NULL,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        title = 'Model Intervention',
        column(
          width = 5,
          HTML(
            "
            <p>
            How would the epidemic have unfolded had we acted differently? The figure on the right shows the same as above, with one
            difference: we assume that, on the 1<sup>st</sup> of June, we allowed the virus to spread more
            (e.g. through relaxing social distancing) than it actually has. (For details on how this is formalized,
            see the explanation of the model below). The model predicts an increase in the number of infections, hospitalizations,
            intensive care cases, as well as mortalities; a second peak. This is indicated by the solid grey line, which gives
            the posterior mean predictions for this alternative scenario, while the bands indicate the uncertainty associated with them.
            </p>
            
            <p>
            In addition to asking the model about what could have happened, you can also ask it what might happen under different
            interventions. Under <i>Interactive Exploration</i>, you can add your own interventions and see the effect the model predicts they
            would have. You can also specify a treshold for the intensive care capacity that, if surpassed, automatically leads to a reduction
            in the reproductive number. We call this feature the <i>Automatic Hammer</i> in the <i>Interactive Exploration</i>.
            </p>
            "
          )
        ),
        column(
          width = 7,
          withSpinner(plotOutput('explanation_plot_intervention', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
        )
      ),
      
      box(
        width = NULL,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        title = 'Model Automatic Hammer',
        column(
          width = 5,
          HTML(
            "
            <p>
            What if we again implement measures that reduce virus transmission (such as stronger social distancing)
            once a critical threshold of intensive care capacity is reached? The figure on the right illustrates this.
            In particular, it shows the same scenario as above, except that we have added an automatic 'hammer',
            implementing measures that reduce virus transmission once the intensive care capacity reaches 500. While this
            clearly reduces the extent of the second peak, there is a lag so that we would still overshoot the 500
            intensive care cases. Under <i>Interactive Exploration</i>, you can explore different thresholds and strengths of the 'hammer'.
            </p>
            "
          )
        ),
        column(
          width = 7,
          withSpinner(plotOutput('explanation_plot_hammer', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
        )
      ),
      
      box(
        width = NULL,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        title = 'Model Explanation',
        column(
          width = 6,
          htmlOutput('model_overview')
        ),
        column(
          width = 6,
          div(
            style = 'margin-top: 3%; margin-left: 3%',
            imageOutput('model_explanation')
          )
        ),
        column(12, align = 'left', tableOutput('model_parameters'))
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
              'Intervention',
              withMathJax(),
              
              HTML(
                '
                <p style="font-size: 100%;">
                After running the model, this panel allows you to see the (probabilistic) effect of past and future interventions.
                Interventions are formalized as a change in the amount of restrictions (such as social distancing) taking
                place from a particular date onwards. They range from 0 (no restrictions; virus roams freely) to 100 (total restrictions;
                virus transmission completely stopped). The figure in the bottom left visualizes effects across time.
                </p>
                
                <p style="font-size: 100%;">
                You can also implement an automatic hammer, which increases the amount of restrictions
                after a certain intensive care unit threshold has been reached.
                </p>
                '
              ),
              tags$hr(),
              
              numericInput(
                'nr_interventions_forecast',
                'Number of Your Interventions',
                min = 1, max = 100,
                value = 1, width = '100%'
              ),
              
              uiOutput('intervention'),
              
              checkboxInput(
                'hammer', 'Implement Automatic Hammer', value = FALSE
              ),
              conditionalPanel(
                condition = 'input.hammer == 1',
                uiOutput('hammer_panel')
              ),
              # uiOutput('hammer_panel')
              div(style = 'display: inline-block;',  actionButton('intervene', 'Intervene')),
              div(style = 'display: inline-block;',  actionButton('reset', 'Reset Intervention'))
            ),
            
            tabPanel(
              'Parameters I',
              
              HTML(
                'In this expert panel, you can change the more subtle
                components of the model by changing the priors on key parameters.'
              ),
              
              tags$hr(),
              
              tags$b('Prior on \\( R_0 \\)'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'R0_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 3.20, min = 0, max = 5, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'R0_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.20, min = 0, max = 2, step = 0.01, width = '100%'
                )
              ),
              
              tags$b('Average Fraction of Infected People Needing Hospitalization [h]'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                # Add a standard deviation!
                numericInput(
                  'hosfrac_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0.015, min = 0.001, max = 0.30, step = 0.001, width = '100%'
                ),
                numericInput(
                  'hosfrac_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.005, min = 0.001, max = 0.10, step = 0.001, width = '100%'
                )
              ),
              
              HTML('<b>Prior on Case Fatality Rate of All Patients</b> [CFR<sub>hos</sub>]'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'dfrac_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0.22, min = 0, max = 1, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'dfrac_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.03, min = 0, max = 2, step = 0.01, width = '100%'
                )
              ),
              
              # No smoothing (so no \xi)
              HTML('<b>Prior on Case Fatality Rate of IC Patients</b> [f<sub>icu</sub>]'),
              splitLayout(
                cellWidths = c('50%', '50%'), 
                numericInput(
                  'icudfrac_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 0.30, min = 0, max = 1, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'icudfrac_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0.020, min = 0, max = 2, step = 0.01, width = '100%'
                )
              ),
  
              actionButton('run1', 'Run')
            ),
        
            tabPanel(
              'Parameters II',
              
              HTML(
                'In this expert panel, you can change the more subtle
                components of the model by changing the priors on key parameters.'
              ),
              
              tags$hr(),
              
              HTML('<b>Prior on Days Between Hospitalisation and Recovery</b> [d<sub>hos</sub>]'),
              splitLayout(
                cellWidths = c('33%', '33%', '33%'), 
                numericInput(
                  'delayHOS_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 7, min = 0, max = 50, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'delayHOS_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01, width = '100%'
                ),
                numericInput(
                  'delayHOS_xi', label = withMathJax('\\( \\xi \\)'),
                  value = 2, min = 0, max = 10, step = 0.01, width = '100%'
                )
              ),
              
              HTML('<b>Prior on Days of Hospital Treatment to Recovery (no IC)</b> [d<sub>hosrec</sub>]'),
              splitLayout(
                cellWidths = c('33%', '33%', '33%'), 
                numericInput(
                  'delayHOSREC_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 9, min = 0, max = 50, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'delayHOSREC_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01, width = '100%'
                ),
                numericInput(
                  'delayHOSREC_xi', label = withMathJax('\\( \\xi \\)'),
                  value = 4, min = 0, max = 10, step = 0.01, width = '100%'
                )
              ),
              
              HTML('<b>Prior on Days of Hospital Treatment for Mortalities (no IC)</b> [d<sub>hosd</sub>]'),
              splitLayout(
                cellWidths = c('33%', '33%', '33%'), 
                numericInput(
                  'delayHOSD_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 3, min = 0, max = 50, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'delayHOSD_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01, width = '100%'
                ),
                numericInput(
                  'delayHOSD_xi', label = withMathJax('\\( \\xi \\)'),
                  value = 2, min = 0, max = 10, step = 0.01, width = '100%'
                )
              ),
              
              HTML('<b>Prior on Days for IC Patients to Die</b> [d<sub>icud</sub>]'),
              splitLayout(
                cellWidths = c('33%', '33%', '33%'), 
                numericInput(
                  'delayICUD_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 11, min = 0, max = 50, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'delayICUD_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01, width = '100%'
                ),
                numericInput(
                  'delayICUD_xi', label = withMathJax('\\( \\xi \\)'),
                  value = 8, min = 0, max = 20, step = 0.01, width = '100%'
                )
              ),
              
              HTML('<b>Prior on Days for IC Patients to Recover</b> [d<sub>icurec</sub>]'),
              splitLayout(
                cellWidths = c('33%', '33%', '33%'), 
                numericInput(
                  'delayICUREC_mean', label = withMathJax('\\( \\mu \\)'),
                  value = 23, min = 0, max = 50, step = 0.01, width = '100%'
                ), 
                numericInput(
                  'delayICUREC_sd', label = withMathJax('\\( \\sigma \\)'),
                  value = 0, min = 0, max = 10, step = 0.01, width = '100%'
                ),
                numericInput(
                  'delayICUREC_xi', label = withMathJax('\\( \\xi \\)'),
                  value = 16, min = 0, max = 30, step = 0.01, width = '100%'
                )
              ),
              
              sliderInput(
                'esmda_iterations',
                'Model Iterations',
                value = 4, min = 2, max = 16, width = '100%'
              ),
              
              uiOutput('nr_interventions'),
              checkboxInput(
                'show_alpha',
                'Show Prior for Past Interventions',
                value = FALSE
              ),
              
              uiOutput('prior_intervention'),
              actionButton('run2', 'Run')
            )
          )
        ),
          
        fluidRow(
          column(
            width = 8,
            box(
              title = 'Amount of Restrictions', width = NULL, solidHeader = TRUE, status = 'primary',
              withSpinner(plotOutput('interventionPlot', height = 500), color = '#0dc5c1')
            )
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(
          width = NULL,
          tags$h2(
            'About', align = 'center'
          ),
          
          HTML(
            "
            <p style = 'text-align: center;'>
            This web interface was developed by <a href='https://twitter.com/fdabl' target='_blank'>Fabian Dablander</a>
            together with <a href='https://nl.linkedin.com/in/lgbrunner' target='_blank'>Logan Brunner</a> and
            <a href='https://www.tno.nl/en/professors/jan-diederik-van-wees/' target='_blank'>Jan-Diederik van Wees</a> as a
            <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project.
            </p>
            
            <p style = 'text-align: center;'>
            For questions about the model specifically, please contact Logan or Jan-Diederik.
            For issues regarding the Shiny app, head over to <a href='https://github.com/fdabl/Covid-Forecast'>Github</a>.
            <p>
            "
          )
        )
      )
    )
  )
)


dashboardPage(
  dashboardHeader(title = 'Exploring COVID-19', titleWidth = 350),
  sidebar,
  body
)