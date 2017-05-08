library(shiny)

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

data(zipcode)

# scatter axis definition
xax <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 50,
  ticklen = 0,
  tickwidth = 2,
  tickcolor = toRGB("black"),
  title="Duration (Days)",
  zeroline=F
)
yax <- list(title="Full 1% Amount",
            zeroline=F)

# barplot axis definition

barY <- function(total){
  ticks <- round(total / 10, -5)
  return(ticks)
}

xax2 <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 1991,
  dtick = 1,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("white"),
  title="",
  zeroline=F,
  showline=F
)

#plot margins

m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 2
)


shinyServer(function(input, output) {
  
  # scatterplot function
  
  output$scatPlot <- renderPlotly({
    
    agencyFilter <- if(length(input$agency) == 0){unique(art$Agency)}
    else{input$agency}
    
    councFilter <- if(length(input$councdist) == 0){unique(art$project.CD)}
    else(input$councdist)
    
    dateFilter <- input$dateRange2
    
    
    art <- art %>%
      filter(!is.na(project.START.date)) %>%
      filter(Agency %in% agencyFilter & project.CD %in% councFilter) %>%
      filter(project.START.date >= dateFilter[1] & project.START.date <= dateFilter[2]) %>%
      mutate(proj.duration = round(proj.duration/7,0))
    
    plot_ly(art, type='scatter',
            x = ~proj.duration, 
            y = ~FULL.1..amount, 
            color =~Agency, 
            colors = "Set1",
            symbol = ~STATUS, 
            symbols = c('diamond','circle'),
            hoverinfo='text',
            text = ~paste('Project: ', PROJECT.NAME, 
                          '<br>Agency: ', AGENCY,
                          '<br>Amount: ', round(FULL.1..amount/1000,0),
                          'K<br>Start Date: ', project.START.date,
                          '<br>End Date: ', END.DATE),
            marker = list(size = 14,
                          line = list(color = 'black',width = 1),
                          opacity=.7)) %>%
      layout(xaxis=xax, yaxis=yax, title="Cost vs Project Duration", margin=m)
    
  })
  
  # barplot function
  
  output$barPlot <- renderPlotly({
    
    agencyFilter <- if(length(input$agency) == 0){unique(art$Agency)}
    else{input$agency}
    
    councFilter <- if(length(input$councdist) == 0){unique(art$project.CD)}
    else(input$councdist)
    
    dateFilter <- input$dateRange2
    
    art <- art %>%
      filter(!(is.na(project.START.date))) %>%
      filter(Agency %in% agencyFilter & project.CD %in% councFilter) %>%
      filter(project.START.date >= dateFilter[1] & project.START.date <= dateFilter[2]) %>%
      group_by(proj.year,Agency) %>%
      summarise(total = sum(FULL.1..amount))
    
    yax2 <- list(
      zeroline=F,
      tick0 = 0,
      dtick = barY(max(art$total)),
      title="Project $"
    )
    
    plot_ly(art, type='bar',
            orientation = 'v',
            x = ~proj.year, 
            y = ~total,
            color = ~Agency,
            colors='Set1',
            hoverinfo='text',
            text = ~paste('Total : $', round(total / 1000,0), 
                          'K<br>Agency: ', Agency,sep=""),
            marker = list(line = list(color='rgba(0, 0, 0, 0)',width=.8)),
            opacity=.8)%>%
      layout(barmode = 'stack', yaxis=yax2, xaxis=xax2, title="Expenditures by Year", legend = list(x = 0, y = 1), margin = m)
  })
  
  
  #base map
  
  output$baseMap <- renderMap({
    
    basemap <- Leaflet$new()
    basemap$setView(c(34.038391, -118.326625), 11)
    basemap$tileLayer(provider = 'OpenStreetMap.DE')
    
    basemap$params$width <- 900
    basemap$params$height <- 550
    
    basemap
    
  })
  
  #data overlay
  
  output$dataMap <- renderUI({
    
    zip_art <- art %>%
      filter(project.ZIP != 90050) %>%
      mutate(region = as.character(project.ZIP)) %>%
      group_by(region) %>%
      summarise(value = sum(FULL.1..amount))
    
    zip_art$region <- clean.zipcodes(zip_art$region)
    
    zip_art <- zip_art %>%
      left_join(zipcode, by = c("region" = "zip")) %>%
      select(latitude, longitude, value) %>%
      mutate(value = log(value)*5) %>%
      toJSONArray2(json = F, names = F)
    
    zip_art <- rjson::toJSON(zip_art)
    
    tags$body(tags$script(HTML(sprintf("
                                       var addressPoints = %s
                                       var heat = L.heatLayer(addressPoints).addTo(map)           
                                       ",zip_art))))
    
  })
  
  output$tree1 <- renderD3tree2({
    
    agencyFilter <- if(length(input$agency) == 0){unique(art$Agency)}
    else{input$agency}
    
    councFilter <- if(length(input$councdist) == 0){unique(art$project.CD)}
    else(input$councdist)
    
    dateFilter <- input$dateRange2
    
    art_tm <- art %>%
      filter(!(is.na(project.START.date))) %>%
      filter(Agency %in% agencyFilter & project.CD %in% councFilter) %>%
      filter(project.START.date >= dateFilter[1] & project.START.date <= dateFilter[2]) %>%
      select(AGENCY,PROJECT.NAME, FULL.1..amount)
    
    art_tm1 <- treemap(art_tm,
                       index = c("AGENCY","PROJECT.NAME"),
                       vSize = "FULL.1..amount",
                       type="index",
                       palette = "Paired")
    
    d3tree2(art_tm1, rootname="Art Project Explorer")
    
  })
  
})
