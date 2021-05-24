
# Define UI for application 
shinyUI(navbarPage(
  title="Latent Profile Analysis",
  
  ###### First Section ######
  tabPanel("Model Settings",
           
           bsModal(id = "DefineModel", title = "Descriptions of the models",
                   size = "large", trigger = "help",
                   imageOutput("ModelIcons", height = 800/1054 * 695),
                   imageOutput("ModelTable"),
                   helpText("These Figures are adopted from Scrucca, Fop, Murphy, & Raftery (2016). See About section.")
           ),
           sidebarLayout(
             
             # The menu
             sidebarPanel(
               fileInput("file", "Load a datafile", accept=".csv", buttonLabel = icon("desktop")),
               tags$br(), tags$br(),
               
               uiOutput("profiles"),
               uiOutput("model"),
               tags$br(),
               uiOutput("estimate"),
               actionButton("help", "Need some help?", icon = icon("question-circle")),
               tags$br(), tags$br(),
               helpText("Found a bug or experiencing trouble? Post an issue on GitHub:"),
               a(href="https://github.com/Spiritspeak/LPAapp/", icon("github", "fa-3x"))
             ),
             # Main field
             mainPanel(
               tabsetPanel(type="tabs",
                           
                           # Data overview
                           tabPanel("Data",
                                    tags$h3("Select Columns for Analysis"),
                                    DT::dataTableOutput('datatable')
                                    ),
                           
                           # Diagnostisc
                           tabPanel("Data summary",
                                    tags$h3("Summary statistics"),
                                    DT::dataTableOutput("SummaryStats"),
                                    tags$h3("Plots"),
                                    actionButton("plotIt", "Plot selected variables"),
                                    plotOutput("pairs")
                                    ),
                           
                           # Comparison
                           tabPanel("Model comparison",
                                    #verbatimTextOutput('modelText'),
                                    DT::dataTableOutput('BIC'),
                                    plotOutput("plotIC")
                           )#,
                           
                           # Details
                           #tabPanel("Model details",
                           #        DT::dataTableOutput('GroupStats')
                           #          )
               )
             ) # mainPanel
           )  # sidebarLayout
  ),  # tabPanel
  
  ##### Second Section ######
  tabPanel("Output",
           tabsetPanel(type="tabs",
                       
                       # Plots
                       tabPanel("Plots", 
                                fluidRow(splitLayout(
                                  cellwidths = "60%",
                                  tags$h2("Model means and covariances"),
                                  tags$h2("Latent Profile Sizes")
                                  )
                                ),
                                fluidRow(splitLayout(
                                  cellwidths = "40%",
                                  plotOutput("ultraScatterPlot",  width = "800px", height = "800px"),
                                  plotOutput("piePlot")
                                  )
                                )
                                #checkboxInput("WhichPlot", "Group by items"),
                                ),
                       
                       # Parameter Estimater
                       tabPanel("Parameter estimates",
                                downloadButton("DownloadPar",
                                               "Download Parameters"),
                                tags$br(), tags$br(),
                                tags$h2("Group Means"),
                                DT::dataTableOutput('GroupMeans'),
                                tags$br(),
                                tags$h2("Group Variances"),
                                DT::dataTableOutput('GroupVariances'),
                                tags$br(),
                                tags$h2("Variance-covariance matrix per group"),
                                verbatimTextOutput("GroupCovariances")
                                ),
                       
                       # Class membership
                       tabPanel("Group membership",
                                downloadButton("Download",
                                               "Download class membership"),
                                tags$br(), tags$br(),
                                DT::dataTableOutput("GroupMembership"))
           )
  ),  # tabPanel
  
  ###### Third Section ######
  tabPanel("About", withMathJax(), 
           fluidRow(column(3),
                    column(6,
                           includeHTML("documents/IntroBody.html")
                           #includeMarkdown("documents/Intro.Rmd")
                    ),
                    column(3)
           )
  )
))
