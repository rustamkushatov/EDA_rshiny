##########################################
####   Shiny server                   ####
##########################################
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(DT)
library(heatmaply)
library(Hmisc)
library(pastecs)


shinyServer(function(input, output, session) {
  
  options(shiny.maxRequestSize = 50*1024^2) 
  
  ###############################
  ######   Page with table   ####
  # z is reactive to a change in the input data
  z <- reactive({
    infile <- input$file
    if (is.null(infile)) {return(NULL)}
    read.csv(infile$datapath, header = TRUE)

  })
  
  # file upload success message
  observeEvent(input$file, {
    sendSweetAlert(
      session = session,
      title = "Success !",
      text = "All in order",
      type = "success"
    )
  })
  
  # Message about not entered file and variables
  observeEvent(input$sidebar,{
    if (is.null(z()) | length(input$choiceColumns) < 2) {
      sendSweetAlert(
        session = session,
        title = "Warning!",
        text = "Upload file and select at least two variables",
        type = "warning"
      )
    }
  },ignoreInit = T)
  
  
  
  
  # changes the input data in the widget
  observe({
    vchoices <- names(z())
    updatePickerInput(session, "choiceColumns", choices = vchoices)
    updatePickerInput(session, "categorial", choices = c("empty",vchoices))
  })
  
  
  # Table Output
  output$table <- DT::renderDataTable({
    expr = z()[ , input$choiceColumns]
    },
    filter="top")
  
  
  
  
  ########################
  ###### Resume page #####

  #information option
  observeEvent(input$funcDescriptive,
               {sendSweetAlert(
                 session = session,
                 title = "Information",
                 text = paste("option 1 - summary()  {base}",
                              "option 2 - describe() {Hmisc}",
                              "option 3 - stat.desc  {pastecs}",
                              "option 4 - cor() {base}"),
                 type = "info"
               )}, 
               ignoreInit = T,
               once = T)
  
  #Save results
  output$save <- downloadHandler(
    
    filename = function() {"summary.txt"},
    content = function(file) {
      # Here you change to csv (write.csv) or excel (xlsx::write.xlsx)
      sink(file); print(save_option()); sink()
    })
  
  #Descriptive statistics
  output$summary <- renderPrint({
    if (input$funcDescriptive == "option 1") {
      summary(z()[, input$choiceColumns], descript = "")
      
    } else if (input$funcDescriptive == "option 2") {
      describe(z()[, input$choiceColumns], descript = "")
      
    } else if (input$funcDescriptive == "option 3") {
      stat.desc(z()[, input$choiceColumns], norm = T)
    } else if (input$funcDescriptive == "option 4"){
      round(cor(z()[, input$choiceColumns]),2)
    }
  })
  
  #return choice option
  save_option <- reactive({
    if (input$funcDescriptive == "option 1") {
      summary(z()[, input$choiceColumns], descript = "")
      
    } else if (input$funcDescriptive == "option 2") {
      describe(z()[, input$choiceColumns], descript = "")
      
    } else if (input$funcDescriptive == "option 3") {
      stat.desc(z()[, input$choiceColumns], norm = T)
    } else if (input$funcDescriptive == "option 4"){
      round(cor(z()[, input$choiceColumns]),2)
    }
  })
  
  #####################
  ##### plot ##########
  
  #Updates variables
  observeEvent(input$choiceColumns, {
    updateSelectInput(session, "scatter_x", choices = input$choiceColumns)
    updateSelectInput(session, "scatter_y", choices = input$choiceColumns)
    updateSelectInput(session, "scatter_color", choices = c("empty",input$choiceColumns))
    ###################################################
    updateSelectInput(session, "hist_x", choices = input$choiceColumns)
    updateSelectInput(session, "hist_2", choices = c("empty",input$choiceColumns))
    ###################################################
    updateSelectInput(session, "boxplot_x", choices = input$choiceColumns)
    updateSelectInput(session, "boxplot_y", choices = c("empty",input$choiceColumns))
    ##################################################
    
    ###################################################
    updateSelectInput(session, "pie_labels", choices = input$choiceColumns)
  })
  
  #########################
  ###### Scatter plots ####
  
  output$scatter <- renderPlotly({
    
    if(input$scatter_color == "empty") {
      plot_ly(
        z(),
        x = ~ get(input$scatter_x),
        y = ~ get(input$scatter_y),
        type = "scatter",
        mode = "markers"
      ) %>%
        layout(
          title =  input$scatter_title,
          xaxis = list(title = if (input$layout_sctatter==1){input$scatter_xaxis} else {input$scatter_x}),
          yaxis = list(title = if (input$layout_sctatter==1){input$scatter_yaxis} else {input$scatter_y})
          
        )
    }else{
      plot_ly(
        z(),
        x = ~ get(input$scatter_x),
        y = ~ get(input$scatter_y),
        color = ~ (get(input$scatter_color)),
        type = "scatter",
        mode = "markers"
      ) %>%
        layout(
          title =  input$scatter_title,
          legend= list(itemsizing='constant'),
          xaxis = list(title = if (input$layout_sctatter==1){input$scatter_xaxis} else {input$scatter_x}),
          yaxis = list(title = if (input$layout_sctatter==1){input$scatter_yaxis} else {input$scatter_y})
          
          
        )
      
    }
  })
  
  #########################
  ######  Histogram   ####
  
  output$histogram <- renderPlotly({
    
  histfull <- plot_ly(
    z(),
    x = ~ get(input$hist_x),
    color = if (input$hist_2 != "empty") {(~get(input$hist_2))} else {NULL},
    type = "histogram"
  
  ) %>% 
    layout(
      title = input$histogram_title,
      xaxis = list(title = if (input$layout_histogram==1){input$histogram_xaxis} else {input$hist_x}),
      barmode = 'overlay'
    )
    
 
  })
   
  #########################
  ######  Boxplot      ####
  
  output$boxplot <- renderPlotly({
    
    if(input$boxplot_y == "empty") {
      plot_ly(
        data = z(),
        x = ~ get(input$boxplot_x),
        type = "box"
      )%>% 
        layout(
          title = input$boxplot_title,
          xaxis = list(title = if (input$layout_boxplot==1){input$boxplot_xaxis} else {input$boxplot_x}),
          yaxis = list(title = if (input$layout_boxplot==1){input$boxplot_yaxis} else {input$boxplot_y})
        )
    }else{
      plot_ly(
        data = z(),
        x = ~ get(input$boxplot_x),
        y = ~ get(input$boxplot_y),
        type = "box"
      )%>% 
        layout(
          title = input$boxplot_title,
          xaxis = list(title = if (input$layout_boxplot==1){input$boxplot_xaxis} else {input$boxplot_x}),
          yaxis = list(title = if (input$layout_boxplot==1){input$boxplot_yaxis} else {input$boxplot_y})
        )
    }
  })
 
  
  #########################
  ######  Heatmap      ####
  z_heat  <- eventReactive( input$Trans_select,{
    
    if (input$Trans_select == "normalize") normalize(z())
    else if (input$Trans_select == "percentize") percentize(z()) 
    else z()
        
  })
  
  output$heatmap <- renderPlotly({
  
    if(input$Trans_select == "correlation") {heatmaply_cor(cor(z()[ ,input$choiceColumns]),main = input$heatmap_title,
                                                           xlab = input$heatmap_xlab, 
                                                           ylab = input$heatmap_ylab,
                                                           row_text_angle = input$row_text_angle,
                                                           column_text_angle = input$column_text_angle)
      
    }else if(input$Trans_select == "is.na10") {heatmaply_na(z()[ ,input$choiceColumns],main = input$heatmap_title,
                                                            xlab = input$heatmap_xlab, 
                                                            ylab = input$heatmap_ylab,
                                                            row_text_angle = input$row_text_angle,
                                                            column_text_angle = input$column_text_angle)
      
    }else {heatmaply(z_heat()[ ,input$choiceColumns],main = input$heatmap_title,
            xlab = input$heatmap_xlab, 
            ylab = input$heatmap_ylab,
            row_text_angle = input$row_text_angle,
            column_text_angle = input$column_text_angle)}
    
  })
  
  #########################
  ######  Pie         ####
  
  output$pie <- renderPlotly({
    
    plot_ly(
      data = z(),
      labels = ~ get(input$pie_labels),
      type = "pie"
    ) %>% 
      layout(
        title = input$pie_title
      )
    
  })
  
})