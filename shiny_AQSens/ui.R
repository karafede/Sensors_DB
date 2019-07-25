
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(RCurl)
library(devtools)
library(shinyjs)
library(V8)
library(pbapply)
library(plotly)
library(lubridate)
library(DT)
# devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)


source("functions_for_shiny_report.R")
source("load_DB_data.R")

jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Low Cost Sensors"),
                    
                    dashboardSidebar(
                      width = 290,
                      paste("Time:",Sys.time()),
                      sidebarMenu(
                        
                        selectInput(
                          "model", "Sensor Model:",
                          c(sort(unique(as.character(DB$model))))
                        ),
                        
                         checkboxInput(inputId = "aggregator", 
                                       label   = "aggregate", 
                                       value   = FALSE),

                        selectInput(
                          "pollutant", "Pollutant:",
                           c("All", sort(unique(as.character(DB$pollutant))), selected = "All", multiple = T)
                        ),
                        
                        # menuItem("Map", tabName = "MAP", icon = icon("th")),
                        menuItem("n.Records", tabName = "Table1", icon = icon("th")),
                        menuItem("calibration models", tabName = "Table3", icon = icon("th")),
                        menuItem("prices", tabName = "Prices", icon = icon("th")),
                        
                        fluidRow(
                          column (3,offset = 1,
                                  br(), #hr()
                                  useShinyjs(),
                                  extendShinyjs(text = jscode),
                                  actionButton("refresh", "refresh", icon("paper-plane"),
                                               style="color: #000000; background-color: #ffff00; border-color: #2e6da4", width = 150)
                          ))
                        
                      )),
                    
                    
                    dashboardBody(
                      tags$head(
                        tags$style(
                          "body{
                          min-height: auto;
                          height: auto;
                          max-width: auto;
                          margin: auto;
                          }"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(

                        # First tab content
                        tabItem(tabName = "Table1",
                                fluidRow(
                                  tabBox(
                                    height = 1000, width = 950, selected = tags$b("Table 1: Records by Model & Pollutants"),
                                    tabPanel(
                                      tags$b("Table 1: Records by Model & Pollutants"), 
                                      DT::dataTableOutput('table_1'),
                                      DT::dataTableOutput('references_records'),
                                      plotOutput("plot_comparison_ref", height = "300px", width = "850px")
                                    ),
                                    
                                    tabPanel(
                                      tags$b("Table 2: metrics stats"),
                                      DT::dataTableOutput('table_2')
                                 
                                    )
                                  )
                                )),

                        
                        tabItem(tabName = "Table3",
                                fluidRow(
                                  tabBox(
                                    height = 1000, width = 950, selected = tags$b("Table 3: calibration models"),
                                    tabPanel(
                                      tags$b("Table 3: calibration models"),
                                      DT::dataTableOutput('table_3'),
                                      DT::dataTableOutput('references_calib'),
                                      plotOutput("plot_calibration_ref", height = "300px", width = "850px")
                                  )
                                      # tabPanel(
                                      #   tags$b("plot"),
                                      #   column(3, plotOutput("plot_calibration", height = "700px", width = "850px"))
                                      # )
                                    )
                                )),
                        
                        tabItem(tabName = "Prices",
                                fluidRow(
                                  tabBox(
                                    height = 1000, width = 950, selected = tags$b("Table 3: Prices of sensors"),
                                    tabPanel(
                                      tags$b("Table 3: Prices of sensors"),
                                      DT::dataTableOutput('table_4'),
                                      plotOutput("r2_slope", width = "50%", height = "400px")
                                    )
                                  )
                                ))
                        
                      ))
)


