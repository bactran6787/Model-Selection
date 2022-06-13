shinyUI(fluidPage(
  
  # Application title
  titlePanel("Model Selection - Bac Tran"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             h3("MixedPairs Numeric - Categorical", align ="center"),
                selectizeInput(inputId = "VariablesA",
                               label = "Choose Categorical Variablest:",
                               choices = cat_list, multiple = TRUE,
                               selected = "BloodType"),
                selectizeInput(inputId = "VariablesB",
                               label = "Choose Numeric Variables:",
                               choices = num_list, multiple = TRUE,
                               selected = num_initial),
                selectizeInput(inputId = "VariablesC",
                              label = "Choose Categorical Variable to colour:",
                              choices = cat_list, multiple = FALSE,
                              selected = "BloodType"),
                actionButton(inputId = "Go_pair", label = "Plot", icon = icon("play")),
                plotOutput(outputId = "MixedPairs"),
             DT::dataTableOutput(outputId = "Table")
             
    ), 
    
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         # maintenance point ------------------------------------------------------------------------------
                         
                         # add further tabs (with controls) here
                         tabPanel("randomForest Model",
                                  verbatimTextOutput(outputId = "rf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rf_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rf_ModelPlots"),
                                  verbatimTextOutput(outputId = "rf_Recipe"),
                                  verbatimTextOutput(outputId = "rf_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "rf_importance")  #  
                                  )
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         tabPanel("svmLinear Model",
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmLinear_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmLinear_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmLinear_Recipe"),
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "svmLinear_importance")    
                                  )
                         ),
                         
                         # maintenance point ------------------------------------------------------------------------------
                        
                         
                         tabPanel("svmRadial Model",
                                  verbatimTextOutput(outputId = "svmRadial_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmRadial_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmRadial_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmRadial_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmRadial_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmRadial_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmRadial_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmRadial_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmRadial_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmRadial_Recipe"),
                                  verbatimTextOutput(outputId = "svmRadial_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "svmRadial_importance")    
                                  )
                         ),
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "cubist_ModelPlots"),
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "cubist_importance")  #  <- typically this is specific to OLS
                                  )
                                  
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("enet Model",
                                  verbatimTextOutput(outputId = "enet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "enet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(enet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = enet_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "enet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "enet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "enet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "enet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "enet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "enet_ModelPlots"),
                                  verbatimTextOutput(outputId = "enet_Recipe"),
                                  verbatimTextOutput(outputId = "enet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "enet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                        
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("qrf Model",
                                  verbatimTextOutput(outputId = "qrf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "qrf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(qrf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = qrf_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "qrf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "qrf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "qrf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "qrf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "qrf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "qrf_ModelPlots"),
                                  verbatimTextOutput(outputId = "qrf_Recipe"),
                                  verbatimTextOutput(outputId = "qrf_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "qrf_importance")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("gaussprPoly Model",
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gaussprPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gaussprPoly_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gaussprPoly_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gaussprPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gaussprPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "gaussprPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "gaussprPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gaussprPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gaussprPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "gaussprPoly_Recipe"),
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "gaussprPoly_importance")  #  
                                  )
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("rvmPoly Model",
                                  verbatimTextOutput(outputId = "rvmPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rvmPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rvmPoly_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rvmPoly_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rvmPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rvmPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rvmPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rvmPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rvmPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rvmPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rvmPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rvmPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rvmPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "rvmPoly_Recipe"),
                                  verbatimTextOutput(outputId = "rvmPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "rvmPoly_importance")  #  
                                  )
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("svmPoly Model",
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmPoly_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmPoly_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmPoly_Recipe"),
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "svmPoly_importance")  #  
                                  )
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("M5 Model",
                                  verbatimTextOutput(outputId = "M5_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "M5_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(M5_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = M5_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "M5_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "M5_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "M5_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "M5_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "M5_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "M5_ModelPlots"),
                                  verbatimTextOutput(outputId = "M5_Recipe"),
                                  verbatimTextOutput(outputId = "M5_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "M5_importance")  #  
                                  )
                         ),
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # maintenance point ------------------------------------------------------------------------------
                         tabPanel("lm Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "lm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lm_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "lm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "lm_ModelPlots"),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    verbatimTextOutput(outputId = "lm_Coef")  # 
                                  )
                         ),
                         
                         # maintenance point ------------------------------------------------------------------------------
                         
                         # maintenance point ------------------------------------------------------------------------------
                         tabPanel("rlm Model",
                                  verbatimTextOutput(outputId = "rlm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rlm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rlm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rlm_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rlm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rlm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rlm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rlm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rlm_ModelPlots"),
                                  verbatimTextOutput(outputId = "rlm_Recipe"),
                                  verbatimTextOutput(outputId = "rlm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    verbatimTextOutput(outputId = "rlm_Coef")  # 
                                  )
                         ),
                         
                         # maintenance point ------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("bagEarthGCV Model",
                                  verbatimTextOutput(outputId = "bagEarthGCV_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bagEarthGCV_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bagEarthGCV_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bagEarthGCV_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "bagEarthGCV_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bagEarthGCV_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bagEarthGCV_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarthGCV_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bagEarthGCV_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bagEarthGCV_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bagEarthGCV_ModelPlots"),
                                  verbatimTextOutput(outputId = "bagEarthGCV_Recipe"),
                                  verbatimTextOutput(outputId = "bagEarthGCV_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bagEarthGCV_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("nnet Model",
                                  verbatimTextOutput(outputId = "nnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "nnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(nnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = nnet_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "nnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "nnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "nnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "nnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "nnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "nnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "nnet_Recipe"),
                                  verbatimTextOutput(outputId = "nnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "nnet_Coef")  #  
                                  )
                         ),
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         tabPanel("mlp Model",
                                  verbatimTextOutput(outputId = "mlp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "mlp_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(mlp_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = mlp_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "mlp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "mlp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "mlp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "mlp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "mlp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "mlp_ModelPlots"),
                                  verbatimTextOutput(outputId = "mlp_Recipe"),
                                  verbatimTextOutput(outputId = "mlp_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "mlp_Coef")  #  
                                  )
                         ),
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
                         
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
    
             
    
    
  )
))
