shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Dataset.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    #d <- d[-c(4),] # Delete negative response value observation to test
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  #Output Mixpair
  output$MixedPairs <- renderPlot({
    req(input$Go_pair)
    isolate({
      d <- getData()
      cat_cols <- c(input$VariablesA)
      num_cols <- c(input$VariablesB)
      colour_1 <- c(input$VariablesC)
      data_mixed <- data.frame(d[,cat_cols], d[,num_cols])
      GGally::ggpairs(data = data_mixed, mapping = ggplot2::aes(colour = d[,colour_1]),
                      columnLabels = c(cat_cols, num_cols),
                      title = "Pairs of Assignment 3 data")
    })
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #
  
  
  #maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #random Forest
  library(randomForest)
  # reactive getrfRecipe ----
  getrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rf_Go ----
  observeEvent(
    input$rf_Go,
    {
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rf_Load,
    {
      method  <- "rf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rf_Delete,
    {
      models[["rf"]] <- NULL
      gc()
    }
  )
  
  # output rf_ModelSummary (text) ----
  output$rf_ModelSummary0 <- renderText({
    description("rf")   # Use the caret method name here
  })
  
  # output rf_Metrics (table) ----
  output$rf_Metrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  # output rf_ModelPlots (plot) ----
  output$rf_ModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  # output rf_Recipe (print) ----
  output$rf_Recipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  # output rf_ModelSummary2 (print) ----
  output$rf_ModelSummary2 <- renderPrint({
    req(models$rf)
    print(models$rf)
  })
  
  # output rf_Importance (print) ----
  output$rf_importance <- renderPlot({
    req(models$rf)
    plot(caret::varImp(models$rf))
    })
  
  
  
  #maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #svmLinear
  
  library(kernlab)
  
  # reactive getsvmLinearRecipe ----
  getsvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmLinear_Go ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )
  
  # output svmLinear_ModelSummary (text) ----
  output$svmLinear_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })
  
  # output svmLinear_Metrics (table) ----
  output$svmLinear_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  # output svmLinear_ModelPlots (plot) ----
  output$svmLinear_ModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  # output svmLinear_Recipe (print) ----
  output$svmLinear_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output svmLinear_ModelSummary2 (print) ----
  output$svmLinear_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })
  
  # output svmLinear_Importance (print) ----
  output$svmLinear_importance <- renderPlot({
    req(models$svmLinear)
    plot(caret::varImp(models$svmLinear))
  })
  
  
  #maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # svmRadial Model
  library(kernlab)
  
  # reactive getsvmRadialRecipe ----
  getsvmRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmRadial_Go ----
  observeEvent(
    input$svmRadial_Go,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmRadial_Load,
    {
      method  <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmRadial_Delete,
    {
      models[["svmRadial"]] <- NULL
      gc()
    }
  )
  
  # output svmRadial_ModelSummary (text) ----
  output$svmRadial_ModelSummary0 <- renderText({
    description("svmRadial")   # Use the caret method name here
  })
  
  # output svmRadial_Metrics (table) ----
  output$svmRadial_Metrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  # output svmRadial_ModelPlots (plot) ----
  output$svmRadial_ModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })
  
  # output svmRadial_Recipe (print) ----
  output$svmRadial_Recipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  # output svmRadial_ModelSummary2 (print) ----
  output$svmRadial_ModelSummary2 <- renderPrint({
    req(models$svmRadial)
    print(models$svmRadial)
  })
  
  # output svmRadial_Importance (print) ----
  output$svmRadial_importance <- renderPlot({
    req(models$svmRadial)
    plot(caret::varImp(models$svmRadial))
  })
  
  
  #maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #Bayesian Ridge Regression
  library(Cubist)
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubist_Go ----
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method  <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      models[["cubist"]] <- NULL
      gc()
    }
  )
  
  # output cubist_ModelSummary (text) ----
  output$cubist_ModelSummary0 <- renderText({
    description("cubist")   # Use the caret method name here
  })
  
  # output cubist_Metrics (table) ----
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubist_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  # output cubist_Recipe (print) ----
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubist_ModelSummary2 (print) ----
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  # output cubist_importance (print) ----
  output$cubist_importance <- renderPlot({
    req(models$cubist)
    plot(caret::varImp(models$cubist))
    
  })
    
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #enet method
  library(elasticnet)
  # reactive getenetRecipe ----
  getenetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$enet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent enet_Go ----
  observeEvent(
    input$enet_Go,
    {
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getenetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$enet_Load,
    {
      method  <- "enet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$enet_Delete,
    {
      models[["enet"]] <- NULL
      gc()
    }
  )
  
  # output enet_ModelSummary (text) ----
  output$enet_ModelSummary0 <- renderText({
    description("enet")   # Use the caret method name here
  })
  
  # output enet_Metrics (table) ----
  output$enet_Metrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  # output enet_ModelPlots (plot) ----
  output$enet_ModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })
  
  # output enet_Recipe (print) ----
  output$enet_Recipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })  
  
  # output enet_ModelSummary2 (print) ----
  output$enet_ModelSummary2 <- renderPrint({
    req(models$enet)
    print(models$enet)
  })
  
  # output enet_importance (print) ----
  output$enet_importance <- renderPlot({
    req(models$enet)
    plot(caret::varImp(models$enet))
  })
  
  # output enet_Coef (print) ----
  output$enet_Coef <- renderTable({
    req(models$enet)
    co <- as.matrix(coef(models$enet$finalModel, s  = models$enet$bestTune$lambda))  # special for enet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
 
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #qrf method
  library(quantregForest )
  # reactive getqrfRecipe ----
  getqrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$qrf_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent qrf_Go ----
  observeEvent(
    input$qrf_Go,
    {
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$qrf_Load,
    {
      method  <- "qrf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$qrf_Delete,
    {
      models[["qrf"]] <- NULL
      gc()
    }
  )
  
  # output qrf_ModelSummary (text) ----
  output$qrf_ModelSummary0 <- renderText({
    description("qrf")   # Use the caret method name here
  })
  
  # output qrf_Metrics (table) ----
  output$qrf_Metrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrf_ModelPlots (plot) ----
  output$qrf_ModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  # output qrf_Recipe (print) ----
  output$qrf_Recipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrf_ModelSummary2 (print) ----
  output$qrf_ModelSummary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
  # output qrf_importance (print) ----
  output$qrf_importance <- renderPlot({
    req(models$qrf)
    plot(caret::varImp(models$qrf))
  })
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #gaussprPoly method
  library(kernlab)
  # reactive getgaussprPolyRecipe ----
  getgaussprPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprPoly_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprPoly_Go ----
  observeEvent(
    input$gaussprPoly_Go,
    {
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gaussprPoly_Load,
    {
      method  <- "gaussprPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gaussprPoly_Delete,
    {
      models[["gaussprPoly"]] <- NULL
      gc()
    }
  )
  
  # output gaussprPoly_ModelSummary (text) ----
  output$gaussprPoly_ModelSummary0 <- renderText({
    description("gaussprPoly")   # Use the caret method name here
  })
  
  # output gaussprPoly_Metrics (table) ----
  output$gaussprPoly_Metrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  # output gaussprPoly_ModelPlots (plot) ----
  output$gaussprPoly_ModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })
  
  # output gaussprPoly_Recipe (print) ----
  output$gaussprPoly_Recipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  # output gaussprPoly_ModelSummary2 (print) ----
  output$gaussprPoly_ModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    print(models$gaussprPoly)
  })
  
  # output gaussprPoly_importance (print) ----
  output$gaussprPoly_importance <- renderPlot({
    req(models$gaussprPoly)
    plot(caret::varImp(models$gaussprPoly))
  })
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #rvmPoly Model
  library(kernlab)
  # reactive getrvmPolyRecipe ----
  getrvmPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rvmPoly_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rvmPoly_Go ----
  observeEvent(
    input$rvmPoly_Go,
    {
      method <- "rvmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rvmPoly_Load,
    {
      method  <- "rvmPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rvmPoly_Delete,
    {
      models[["rvmPoly"]] <- NULL
      gc()
    }
  )
  
  # output rvmPoly_ModelSummary (text) ----
  output$rvmPoly_ModelSummary0 <- renderText({
    description("rvmPoly")   # Use the caret method name here
  })
  
  # output rvmPoly_Metrics (table) ----
  output$rvmPoly_Metrics <- renderTable({
    req(models$rvmPoly)
    models$rvmPoly$results[ which.min(models$rvmPoly$results[, "RMSE"]), ]
  })
  
  # output rvmPoly_ModelPlots (plot) ----
  output$rvmPoly_ModelPlots <- renderPlot({
    req(models$rvmPoly)
    plot(models$rvmPoly)
  })
  
  # output rvmPoly_Recipe (print) ----
  output$rvmPoly_Recipe <- renderPrint({
    req(models$rvmPoly)
    models$rvmPoly$recipe
  })  
  
  # output rvmPoly_ModelSummary2 (print) ----
  output$rvmPoly_ModelSummary2 <- renderPrint({
    req(models$rvmPoly)
    print(models$rvmPoly)
  })
  
  # output rvmPoly_importance (print) ----
  output$rvmPoly_importance <- renderPlot({
    req(models$rvmPoly)
    plot(caret::varImp(models$rvmPoly))
  })
  
  # output rvmPoly_Coef (print) ----
  output$rvmPoly_Coef <- renderTable({
    req(models$rvmPoly)
    co <- as.matrix(coef(models$rvmPoly$finalModel, s  = models$rvmPoly$bestTune$lambda))  # special for rvmPoly
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #svmPoly Model
  library(kernlab)
  # reactive getsvmPolyRecipe ----
  getsvmPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmPoly_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmPoly_Go ----
  observeEvent(
    input$svmPoly_Go,
    {
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmPoly_Load,
    {
      method  <- "svmPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmPoly_Delete,
    {
      models[["svmPoly"]] <- NULL
      gc()
    }
  )
  
  # output svmPoly_ModelSummary (text) ----
  output$svmPoly_ModelSummary0 <- renderText({
    description("svmPoly")   # Use the caret method name here
  })
  
  # output svmPoly_Metrics (table) ----
  output$svmPoly_Metrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmPoly_ModelPlots (plot) ----
  output$svmPoly_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  # output svmPoly_Recipe (print) ----
  output$svmPoly_Recipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmPoly_ModelSummary2 (print) ----
  output$svmPoly_ModelSummary2 <- renderPrint({
    req(models$svmPoly)
    print(models$svmPoly)
  })
  
  # output svmPoly_importance (print) ----
  output$svmPoly_importance <- renderPlot({
    req(models$svmPoly)
    plot(caret::varImp(models$svmPoly))
  })
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #M5 Model
  library(RWeka)
  # reactive getM5Recipe ----
  getM5Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$M5_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent M5_Go ----
  observeEvent(
    input$M5_Go,
    {
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$M5_Load,
    {
      method  <- "M5"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$M5_Delete,
    {
      models[["M5"]] <- NULL
      gc()
    }
  )
  
  # output M5_ModelSummary (text) ----
  output$M5_ModelSummary0 <- renderText({
    description("M5")   # Use the caret method name here
  })
  
  # output M5_Metrics (table) ----
  output$M5_Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  # output M5_ModelPlots (plot) ----
  output$M5_ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  # output M5_Recipe (print) ----
  output$M5_Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  # output M5_ModelSummary2 (print) ----
  output$M5_ModelSummary2 <- renderPrint({
    req(models$M5)
    print(models$M5)
  })
  
  # output M5_importance (print) ----
  output$M5_importance <- renderPlot({
    req(models$M5)
    plot(caret::varImp(models$M5))
  })
  
  # output lm_Coef (print) ----
  output$M5_Coef <- renderPrint({
    req(models$M5)
    print(summary(models$M5))
  })
  
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #lm Model
  library(MASS)
  # reactive getlmRecipe ----
  getlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lm_Load,
    {
      method  <- "lm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lm_Delete,
    {
      models[["lm"]] <- NULL
      gc()
    }
  )
  
  # output lm_ModelSummary (text) ----
  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })
  
  # output lm_Metrics (table) ----
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # output lm_ModelPlots (plot) ----
  output$lm_ModelPlots <- renderPlot({
    req(models$lm)
    plot(models$lm)
  })
  
  # output lm_Recipe (print) ----
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  # output lm_ModelSummary2 (print) ----
  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    print(models$lm)
  })
  
  # output lm_Coef (print) ----
  output$lm_Coef <- renderPrint({
    req(models$lm)
    print(summary(models$lm))
  })
  

 
   # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #gaussprLinear Model
  library(kernlab)
  # reactive getgaussprLinearRecipe ----
  getgaussprLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprLinear_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprLinear_Go ----
  observeEvent(
    input$gaussprLinear_Go,
    {
      method <- "gaussprLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getgaussprLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gaussprLinear_Load,
    {
      method  <- "gaussprLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gaussprLinear_Delete,
    {
      models[["gaussprLinear"]] <- NULL
      gc()
    }
  )
  
  # output gaussprLinear_ModelSummary (text) ----
  output$gaussprLinear_ModelSummary0 <- renderText({
    description("gaussprLinear")   # Use the caret method name here
  })
  
  # output gaussprLinear_Metrics (table) ----
  output$gaussprLinear_Metrics <- renderTable({
    req(models$gaussprLinear)
    models$gaussprLinear$results[ which.min(models$gaussprLinear$results[, "RMSE"]), ]
  })
  
  # output gaussprLinear_ModelPlots (plot) ----
  output$gaussprLinear_ModelPlots <- renderPlot({
    req(models$gaussprLinear)
    plot(models$gaussprLinear)
  })
  
  # output gaussprLinear_Recipe (print) ----
  output$gaussprLinear_Recipe <- renderPrint({
    req(models$gaussprLinear)
    models$gaussprLinear$recipe
  })  
  
  # output gaussprLinear_ModelSummary2 (print) ----
  output$gaussprLinear_ModelSummary2 <- renderPrint({
    req(models$gaussprLinear)
    print(models$gaussprLinear)
  })
  
  # output gaussprLinear_importance (print) ----
  output$gaussprLinear_importance <- renderPlot({
    req(models$gaussprLinear)
    plot(caret::varImp(models$gaussprLinear))
  })
  
  # output gaussprLinear_Coef (print) ----
  # output rf_Importance (print) ----
  output$gaussprLinear_importance <- renderPlot({
    req(models$gaussprLinear)
    plot(caret::varImp(models$gaussprLinear))
  })
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #rlm Model
  library(MASS)
  # reactive getrlmRecipe ----
  getrlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rlm_Go ----
  observeEvent(
    input$rlm_Go,
    {
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rlm_Load,
    {
      method  <- "rlm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rlm_Delete,
    {
      models[["rlm"]] <- NULL
      gc()
    }
  )
  
  # output rlm_ModelSummary (text) ----
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")   # Use the caret method name here
  })
  
  # output rlm_Metrics (table) ----
  output$rlm_Metrics <- renderTable({
    req(models$rlm)
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  # output rlm_ModelPlots (plot) ----
  output$rlm_ModelPlots <- renderPlot({
    req(models$rlm)
    plot(models$rlm)
  })
  
  # output rlm_Recipe (print) ----
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  # output rlm_ModelSummary2 (print) ----
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)
    print(models$rlm)
  })
  
  # output lm_Coef (print) ----
  output$rlm_Coef <- renderPrint({
    req(models$rlm)
    print(summary(models$rlm))
  })
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #bagEarthGCV Model
  library(earth)
  # reactive getbagEarthGCVRecipe ----
  getbagEarthGCVRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bagEarthGCV_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent bagEarthGCV_Go ----
  observeEvent(
    input$bagEarthGCV_Go,
    {
      method <- "bagEarthGCV"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getbagEarthGCVRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.fail)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bagEarthGCV_Load,
    {
      method  <- "bagEarthGCV"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bagEarthGCV_Delete,
    {
      models[["bagEarthGCV"]] <- NULL
      gc()
    }
  )
  
  # output bagEarthGCV_ModelSummary (text) ----
  output$bagEarthGCV_ModelSummary0 <- renderText({
    description("bagEarthGCV")   # Use the caret method name here
  })
  
  # output bagEarthGCV_Metrics (table) ----
  output$bagEarthGCV_Metrics <- renderTable({
    req(models$bagEarthGCV)
    models$bagEarthGCV$results[ which.min(models$bagEarthGCV$results[, "RMSE"]), ]
  })
  
  # output bagEarthGCV_ModelPlots (plot) ----
  output$bagEarthGCV_ModelPlots <- renderPlot({
    req(models$bagEarthGCV)
    plot(models$bagEarthGCV)
  })
  
  # output bagEarthGCV_Recipe (print) ----
  output$bagEarthGCV_Recipe <- renderPrint({
    req(models$bagEarthGCV)
    models$bagEarthGCV$recipe
  })  
  
  # output bagEarthGCV_ModelSummary2 (print) ----
  output$bagEarthGCV_ModelSummary2 <- renderPrint({
    req(models$bagEarthGCV)
    print(models$bagEarthGCV)
  })
  
  # output bagEarthGCV_importance (print) ----
  output$bagEarthGCV_importance <- renderPlot({
    req(models$bagEarthGCV)
    plot(caret::varImp(models$bagEarthGCV))
  })
  
  # output bagEarthGCV_Coef (print) ----
  output$bagEarthGCV_Coef <- renderTable({
    req(models$bagEarthGCV)
    co <- as.matrix(coef(models$bagEarthGCV$finalModel, s  = models$bagEarthGCV$bestTune$lambda))  # special for bagEarthGCV
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
  #nnet method
  library(nnet)
  # reactive getnnetRecipe ----
  getnnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent nnet_Go ----
  observeEvent(
    input$nnet_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getnnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$nnet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$nnet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()
    }
  )
  
  # output nnet_ModelSummary (text) ----
  output$nnet_ModelSummary0 <- renderText({
    description("nnet")   # Use the caret method name here
  })
  
  # output nnet_Metrics (table) ----
  output$nnet_Metrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  # output nnet_ModelPlots (plot) ----
  output$nnet_ModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })
  
  # output nnet_Recipe (print) ----
  output$nnet_Recipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })  
  
  # output nnet_ModelSummary2 (print) ----
  output$nnet_ModelSummary2 <- renderPrint({
    req(models$nnet)
    print(models$nnet)
  })
  
  # output nnet_importance (print) ----
  output$nnet_importance <- renderPlot({
    req(models$nnet)
    plot(caret::varImp(models$nnet))
  })
  
  # output nnet_Coef (print) ----
  output$nnet_Coef <- renderTable({
    req(models$nnet)
    co <- as.matrix(coef(models$nnet$finalModel, s  = models$nnet$bestTune$lambda))  # special for nnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #mlp method
  library(RSNNS)
  # reactive getmlpRecipe ----
  getmlpRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$mlp_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent mlp_Go ----
  observeEvent(
    input$mlp_Go,
    {
      method <- "mlp"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getmlpRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$mlp_Load,
    {
      method  <- "mlp"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$mlp_Delete,
    {
      models[["mlp"]] <- NULL
      gc()
    }
  )
  
  # output mlp_ModelSummary (text) ----
  output$mlp_ModelSummary0 <- renderText({
    description("mlp")   # Use the caret method name here
  })
  
  # output mlp_Metrics (table) ----
  output$mlp_Metrics <- renderTable({
    req(models$mlp)
    models$mlp$results[ which.min(models$mlp$results[, "RMSE"]), ]
  })
  
  # output mlp_ModelPlots (plot) ----
  output$mlp_ModelPlots <- renderPlot({
    req(models$mlp)
    plot(models$mlp)
  })
  
  # output mlp_Recipe (print) ----
  output$mlp_Recipe <- renderPrint({
    req(models$mlp)
    models$mlp$recipe
  })  
  
  # output mlp_ModelSummary2 (print) ----
  output$mlp_ModelSummary2 <- renderPrint({
    req(models$mlp)
    print(models$mlp)
  })
  
  # output mlp_importance (print) ----
  output$mlp_importance <- renderPlot({
    req(models$mlp)
    plot(caret::varImp(models$mlp))
  })
  
  # output mlp_Coef (print) ----
  output$mlp_Coef <- renderTable({
    req(models$mlp)
    co <- as.matrix(coef(models$mlp$finalModel, s  = models$mlp$bestTune$lambda))  # special for mlp
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #krlsPoly method
  library(KRLS)
  # reactive getkrlsPolyRecipe ----
  getkrlsPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$krlsPoly_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent krlsPoly_Go ----
  observeEvent(
    input$krlsPoly_Go,
    {
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$krlsPoly_Load,
    {
      method  <- "krlsPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$krlsPoly_Delete,
    {
      models[["krlsPoly"]] <- NULL
      gc()
    }
  )
  
  # output krlsPoly_ModelSummary (text) ----
  output$krlsPoly_ModelSummary0 <- renderText({
    description("krlsPoly")   # Use the caret method name here
  })
  
  # output krlsPoly_Metrics (table) ----
  output$krlsPoly_Metrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  # output krlsPoly_ModelPlots (plot) ----
  output$krlsPoly_ModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  # output krlsPoly_Recipe (print) ----
  output$krlsPoly_Recipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  # output krlsPoly_ModelSummary2 (print) ----
  output$krlsPoly_ModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    print(models$krlsPoly)
  })
  
  # output krlsPoly_importance (print) ----
  output$krlsPoly_importance <- renderPlot({
    req(models$krlsPoly)
    plot(caret::varImp(models$krlsPoly))
  })
  
  # output krlsPoly_Coef (print) ----
  output$krlsPoly_Coef <- renderTable({
    req(models$krlsPoly)
    co <- as.matrix(coef(models$krlsPoly$finalModel, s  = models$krlsPoly$bestTune$lambda))  # special for krlsPoly
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)

  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #lmStepAIC Model
  library(MASS)
  # reactive getlmStepAICRecipe ----
  getlmStepAICRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lmStepAIC_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lmStepAIC_Go ----
  observeEvent(
    input$lmStepAIC_Go,
    {
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getlmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lmStepAIC_Load,
    {
      method  <- "lmStepAIC"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lmStepAIC_Delete,
    {
      models[["lmStepAIC"]] <- NULL
      gc()
    }
  )
  
  # output lmStepAIC_ModelSummary (text) ----
  output$lmStepAIC_ModelSummary0 <- renderText({
    description("lmStepAIC")   # Use the caret method name here
  })
  
  # output lmStepAIC_Metrics (table) ----
  output$lmStepAIC_Metrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  # output lmStepAIC_ModelPlots (plot) ----
  output$lmStepAIC_ModelPlots <- renderPlot({
    req(models$lmStepAIC)
    plot(models$lmStepAIC)
  })
  
  # output lmStepAIC_Recipe (print) ----
  output$lmStepAIC_Recipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })  
  
  # output lmStepAIC_ModelSummary2 (print) ----
  output$lmStepAIC_ModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    print(models$lmStepAIC)
  })
  
  # output lmStepAIC_importance (print) ----
  output$lmStepAIC_importance <- renderPlot({
    req(models$lmStepAIC)
    plot(caret::varImp(models$lmStepAIC))
  })
  
  # output lmStepAIC_Coef (print) ----
 
  output$lmStepAIC <- renderPrint({
    req(models$lmStepAIC)
    print(summary(models$lmStepAIC))
  })
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  #brnn Model
  library(brnn)
  # reactive getbrnnRecipe ----
  getbrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$brnn_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent brnn_Go ----
  observeEvent(
    input$brnn_Go,
    {
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$brnn_Load,
    {
      method  <- "brnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$brnn_Delete,
    {
      models[["brnn"]] <- NULL
      gc()
    }
  )
  
  # output brnn_ModelSummary (text) ----
  output$brnn_ModelSummary0 <- renderText({
    description("brnn")   # Use the caret method name here
  })
  
  # output brnn_Metrics (table) ----
  output$brnn_Metrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  # output brnn_ModelPlots (plot) ----
  output$brnn_ModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  # output brnn_Recipe (print) ----
  output$brnn_Recipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  # output brnn_ModelSummary2 (print) ----
  output$brnn_ModelSummary2 <- renderPrint({
    req(models$brnn)
    print(models$brnn)
  })
  
  # output brnn_importance (print) ----
  output$brnn_importance <- renderPlot({
    req(models$brnn)
    plot(caret::varImp(models$brnn))
  })

  
  output$brnn_Coef <- renderPrint({
    req(models$brnn_)
    print(summary(models$brnn_))
  })
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
})
