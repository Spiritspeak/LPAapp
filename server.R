library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  output$ModelTable <- renderImage({
    list(src="documents/ModelTable.png",
         contentType = "image/png",
         width = 600,
         alt = "model tables")
    }, deleteFile= FALSE)
  
  output$ModelIcons <- renderImage({
    list(src="documents/ModelIcons.png",
         contentType = "image/png",
         width = 800,
         alt = "model tables")
  }, deleteFile= FALSE)
  # Return the dropdown menu for selecting models
  output$profiles <- renderUI({
    max <- 30 #nrow(data())
    max <- ifelse(max>30, 30, max)
    selectInput("profiles", "Number of latent profiles",
                choices=as.list(1:max), multiple=TRUE)
  })
  
  output$model <- renderUI({
    modelNames <- as.list(mclust::mclust.options()$emModelNames)
    modelTypes <- sapply(modelNames, function(x) mclust::mclustModelNames(x)$type)
    models <- paste(modelNames, modelTypes, sep = ": ")
    names(modelNames) <- models
    selectInput("model", "What kind of models to fit?",
                choices=modelNames, multiple = TRUE)
  })
  
  
  fulldata <- reactive({    
    inFile <- input$file
    if(is.null(inFile)){
      read.csv('data/iris2.csv')
    } else {
      validate(
        need(grep("csv", inFile$name) == 1, 
             message = "Only csv files are allowed! \n
             Check documentation for details")
      )
      read.csv(paste(inFile$datapath))
    }
  })
  
  # Select the columns on which the user clicked
  data <- reactive({
    clicked <- input$datatable_columns_selected
    #print(clicked)
    if(is.null(clicked)){
      fulldata()
    } else {
      fulldata()[,clicked+1, drop =FALSE]
    }
  })
  
  # Return the reactive dataTable
  observe({
    output$datatable <- DT::renderDataTable(
      fulldata(),
      selection = list(target="column"),
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
  })
  
  # enable Estimate button after models have been specified
  output$estimate <- renderUI({
    validate(
      need(length(input$profiles)>0 && length(input$model)>0, "Specify the models")
    )
    actionButton("estimate", "Estimate models")
  })
  
  # Summary statistics
  observe({
    output$SummaryStats <- DT::renderDataTable(
      data() %>% gather(key = "Variable", factor_key = TRUE) %>% 
        group_by(Variable) %>% summarise(mean = round(mean(value), 2),
                                         variance = round(var(value), 2),
                                         sd = round(sd(value), 2),
                                         valid.cases = length(value) -sum(is.na(value))),
      selection = list(target = "none"),
      rownames=FALSE)
  })
  
  pairs <- eventReactive(input$plotIt, {
    #clicked <- input$SummaryStats_rows_selected
    
    clicked <- ncol(data())
    #validate(need(!is.null(clicked), "Click on variable summary to plot!"))
    
    if(clicked == 1){
      qplot(x = data(), geom = c('histogram', 'density'), asp = 1) + theme_bw()
    } else {
      GGally::ggpairs(data = data(),
                      lower = list(continuous = "density"),
                      upper = list(continuous = "points"),
                      diag = list(continous = "densityDiag")
                      ) +
        theme(aspect.ratio = 1) + 
        theme_bw()
    }
  })
  
  output$pairs <- renderPlot({pairs()})
  
  
  fit <- eventReactive(input$estimate, {
    mclust::Mclust(data=data(), G = input$profiles, modelNames = input$model,verbose=FALSE)
  })
  
  output$BIC <- DT::renderDataTable({showBIC(fit())})
  
  selectFit <- eventReactive(input$BIC_cells_selected,{
    model <- input$model[input$BIC_cells_selected[,2]]
    profiles <- input$profiles[input$BIC_cells_selected[,1]]
    
    
    mclust::Mclust(data = data(), G = profiles[1], modelNames = model[1], verbose = FALSE)
  })
  
  observeEvent(input$BIC_cells_selected, {
    BIC <- fit()$BIC
    
    #value <- input$BIC_cell_clicked$value
    clicked <- as.vector(input$BIC_cells_selected)
    value <- BIC[clicked[1], clicked[2]]
    #print(value)
    if(is.na(value)){
      showNotification("Select a model that can be estimated!")
    }
  })
  
  #observe({print(input$BIC_cells_selected)})
  
  output$plotIC <- renderPlot({plot(fit(), what = "BIC")})
    
  output$GroupMeans <- DT::renderDataTable({
    showGroupMeans(selectFit())
  })
  output$GroupVariances <- DT::renderDataTable({
    showGroupVariances(selectFit())
  })
  
  output$GroupMembership <- DT::renderDataTable({
    showGroupMembership(selectFit(), fulldata())
  })
  output$ultraScatterPlot <- renderPlot({
    ultrascatterplot(data(), selectFit()$classification, selectFit())
  }, height = 800, width = 800)
  
  output$piePlot <- renderPlot({
    pieplot(fit = selectFit())
  })
  
  
  output$DownloadPar <- downloadHandler(
    
    filename =  function() {
      paste0("Mclust output", ".Rdata")
    },
    
    content = function(con) {
      fullData <- fulldata()
      selectedData <- data()
      allModels <- fit()
      selectedModel <- selectFit()
      save(fullData, selectedData, allModels, selectedModel, file = con)
    }
  )
  
  output$Download <- downloadHandler(
    
    filename =  function() {
      paste0("dataWithMembership", ".csv")
    },
    
    content = function(con) {
      d <- showGroupMembership(selectFit(), fulldata())
      write.csv(d, con)
    }
  )
  
  output$GroupCovariances <- renderPrint({
    selectFit()$parameters$variance$sigma
  })
})
