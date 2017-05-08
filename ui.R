library(shiny)
library(shinythemes)

library(dplyr)
library(lubridate)
library(scales)
library(plotly)

library(rCharts)
library(rjson)
library(zipcode)

library(treemap)
library(d3treeR)

pdf(NULL)

art <- read.csv("artproject.csv")

art$project.START.date <- mdy(art$project.START.date)
art$END.DATE <- mdy(art$END.DATE)

art <- mutate(art, 
              END.DATE = if_else(STATUS == "active",Sys.Date(),END.DATE),
              proj.duration = END.DATE - project.START.date,
              Agency = ifelse(AGENCY %in% c("DWP","CDD","Public Works","Zoo","Harbor"),"Other",as.character(AGENCY)),
              proj.year = year(project.START.date))

agency_list <- unique(art$Agency)
cd_list <- sort(unique(art$project.CD))


shinyUI(navbarPage("LA Public Art Dashboard", tabPanel("Home"),
                   
                   theme = shinytheme("sandstone"),
                   
                   sidebarLayout(
                     sidebarPanel(width=3,
                                  selectizeInput(
                                    'agency', 'Agency (select 1 or more) :', choices = agency_list, multiple = TRUE),
                                  selectizeInput(
                                    'councdist', 'Council District (select 1 or more) :', choices = cd_list, multiple = TRUE),
                                  dateRangeInput('dateRange2',
                                                 label = 'Project Date Filter',
                                                 start = mdy("09-16-1991"), end = Sys.Date(),
                                                 min = mdy("09-16-1991"), max = Sys.Date(),
                                                 separator = " - ", format = "mm/dd/yy",
                                                 startview = 'year', language = 'en', weekstart = 1),
                                  submitButton("Apply Filters")
                     ),
                     
                     mainPanel(
                       tabsetPanel(type = "tabs", 
                                   tabPanel("Project Explorer",
                                            fluidRow(d3tree2Output("tree1", width="100%", height="530px"))),
                                   tabPanel("Cost vs Duration", 
                                            fluidRow(plotlyOutput("scatPlot",height="530px"))),
                                   tabPanel("Expenditures by Year", 
                                            fluidRow(plotlyOutput("barPlot", width="95%", height="530px"))),
                                   tabPanel("Funding Heatmap", 
                                            fluidRow(chartOutput("baseMap",'leaflet'),
                                                     tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
                                                     uiOutput('dataMap')))
                       )
                     )
                   )
))
