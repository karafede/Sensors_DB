
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(shinydashboard)
library(lubridate)
library(DT)
library(stringi)
library(plotly)

source("load_DB_data.R")
source("functions_for_shiny_report.R")


shinyServer(function(input, output, session) {
  
# select pollutants and update them automatically  
  observe({
  pollutant_sensor <- DB %>%
    dplyr::filter(model     == input$model)
  updateSelectInput(session, "pollutant", "Pollutant:",
                    c(sort(unique(as.character(pollutant_sensor$pollutant)))))
  if (input$aggregator == TRUE) {
    updateSelectInput(session, "pollutant", "Pollutant:",
                      c("All"))
  }
  })
  
# number of records
  TABLE1 <- reactive({
      Table_1 <- Table_1_func(MODEL      = input$model,
                              POLLUTANT  = input$pollutant,
                              aggregator = input$aggregator)
      # remove link to references
      Table_1 <- Table_1[, names(Table_1)[which(names(Table_1) != "link")]]
        # output table
        return(Table_1) 
        })
  
  
# metrics stats (R2, slope, intercept, uncertainty)
  TABLE2 <- reactive({
    pollutant_sensor <- DB %>%
      dplyr::filter(model     == input$model)
    Table_2 <- Table_2_func(MODEL      = input$model,
                            POLLUTANT  = input$pollutant)
    # output table
    return(Table_2) 
  })
  
  
# calibration models (SS and OEM)
  TABLE3 <- reactive({
    pollutant_sensor <- DB %>%
      dplyr::filter(model     == input$model)
    Table_3 <- Table_3_func(MODEL      = input$model,
                            POLLUTANT  = input$pollutant)
    # remove link to references
    Table_3 <- Table_3[, names(Table_3)[which(names(Table_3) != "link")]]
    # output table
    return(Table_3) 
  })
  
  
  # prices
  TABLE4 <- reactive({
    pollutant_sensor <- DB %>%
      dplyr::filter(model     == input$model)
    Table_4 <- Table_4_func(MODEL      = input$model,
                            POLLUTANT  = input$pollutant)
    # remove link to references
    # Table_4 <- Table_4[, names(Table_4)[which(names(Table_4) != "link")]]
    # output table
    return(Table_4) 
  })
  
  
  # plot for references for calibration
  output$plot_calibration_ref <- renderPlot({
    Figure_3 <- Figure_3_func(MODEL      = input$model,
                              POLLUTANT  = input$pollutant)
    return(Figure_3)
  })
  
  
  # plot for references for comparison
  output$plot_comparison_ref <- renderPlot({
    Figure_4 <- Figure_4_func(MODEL      = input$model,
                              POLLUTANT  = input$pollutant)
    return(Figure_4)
  })
  
  # plot for R2 and slope (SS and OEM)
  output$r2_slope <- renderPlot({
    Figure_8 <- Figure_8_func(MODEL      = input$model,
                              POLLUTANT  = input$pollutant)
    return(Figure_8)
  })
  
  

  # references for Table 1 (n.records)
  output$references_records <- DT::renderDataTable(DT::datatable ({
    Table_1 <- Table_1_func(MODEL      = input$model,
                            POLLUTANT  = input$pollutant,
                            aggregator = input$aggregator)
    ref <- as.data.frame(unique(Table_1$link))
    names(ref) <- "References"
    # ref <- str_split_fixed(ref, "pattern"[], 5)
    ref
  }, escape=FALSE
  ))
  
  # references for Table 3 (calibration models)
  output$references_calib <- DT::renderDataTable(DT::datatable ({
    Table_3 <- Table_3_func(MODEL      = input$model,
                            POLLUTANT  = input$pollutant)
    ref <- as.data.frame(unique(Table_3$link))
    names(ref) <- "References"
    ref
  }, escape=FALSE
  ))
  
  
  
  # tables to diplay
  output$table_1 <- DT::renderDataTable(TABLE1())
  output$table_2 <- DT::renderDataTable(TABLE2())
  output$table_3 <- DT::renderDataTable(TABLE3())
  output$table_4 <- DT::renderDataTable(TABLE4())

  observeEvent(input$refresh, {
    js$refresh();
  })
  
})


