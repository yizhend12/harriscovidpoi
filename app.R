library(ggplot2)
library(dplyr)
library(tidyr)
library(iotools)
library(shiny)
library(tidyverse)
library(data.table)
library(waldo)
library(stringr)
library(readr)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT)
library(usmap)
library(rgdal)
library(dygraphs)
library(xts)
library(htmltools)
library(colorRamps)
library(scales)
library(shinydashboard)
library(lattice)
library(tidycensus)
library(tigris)
library(sp)
library(rgeos)
library(terra)
library(proj4)
library(RColorBrewer)
library(geosphere)
library(magrittr)
library(sf)
library(leaflet)
library(shinythemes)
detach("package:xts", unload=TRUE)
library(rsconnect)

######################################################################
load("./poi.Rda")
poi[c("sub_category")][is.na(poi[c("sub_category")])] <- "indoor"
poi_v2 <- poi %>%
  mutate(type = if_else(sub_category ==  "All Other Amusement and Recreation Industries"| sub_category== "Promoters of Performing Arts, Sports, and Similar Events with Facilities"| sub_category== "Historical Sites"| sub_category== "Fitness and Recreational Sports Centers"| sub_category== "Museums"| sub_category== "Amusement Arcades"| sub_category== "Bowling Centers"| sub_category=="Casinos (except Casino Hotels)"| sub_category== "indoor", "indoor", "outdoor"))
# por_barplot <- poi_v2 %>% group_by(type, start_date) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
por_barplot <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
por_barplot <- arrange(por_barplot, start_date, ZIP)
por_barplot$ZIP = as.character(por_barplot$ZIP)
names(por_barplot)[3] <- 'zipcode'
por_barplot <- complete(por_barplot, type, start_date, zipcode, fill = list(visitation_sum = 0))
por_barplot['year'] <- format(por_barplot$start_date, format="%Y")

#map stuff
map_zip <- readOGR("COH_ZIPCODE.shp")
zipcode <- unique(poi_v2$ZIP)
#load data
load("./covid.Rda")
load("./poi19_co.Rda")
load("./poi20_co.Rda")
load("./poi21_co.Rda")
################################################################
#subset covid data
confirmed <- covid[, -c(2,5,6,7)] %>% rename(cases = TotalConfirmedCases)
active <- covid[, -c(2,4,6,7)] %>% rename(cases = ActiveCases)
death <- covid[, -c(2,4,5,6)] %>% rename(cases = Death)
#covid color
palconfirmed <- colorNumeric(palette = "GnBu", domain = confirmed$cases/confirmed$TotalPop*100, n = 7)
palactive <- colorNumeric(palette = "BuGn", domain = active$cases/active$TotalPop*100, n = 7)
paldeath <- colorNumeric(palette = "OrRd", domain = death$cases/death$TotalPop*100, n = 7)

#subset poi data
set.seed(100)
poi19map <- poi19_co[sample.int(nrow(poi19_co), 100),]
poi20map <- poi20_co[sample.int(nrow(poi20_co), 100),]
poi21map <- poi21_co[sample.int(nrow(poi21_co), 100),]

#subset flow data
flows19 <- gcIntermediate(poi19map[,4:5], poi19map[,6:7], sp = TRUE, addStartEnd = TRUE)
flows19$counts <- poi19map$vhc_number
flows19$origins <- poi19map$ZIP
flows19$destinations <- poi19map$top_category

flows20 <- gcIntermediate(poi20map[,4:5], poi20map[,6:7], sp = TRUE, addStartEnd = TRUE)
flows20$counts <- poi20map$vhc_number
flows20$origins <- poi20map$ZIP
flows20$destinations <- poi20map$top_category

flows21 <- gcIntermediate(poi21map[,4:5], poi21map[,6:7], sp = TRUE, addStartEnd = TRUE)
flows21$counts <- poi21map$vhc_number
flows21$origins <- poi21map$ZIP
flows21$destinations <- poi21map$top_category

# flow color
palflow19 <- colorFactor(palette = "Dark2", flows19$destinations)
palflow20 <- colorFactor(palette = "Dark2", flows20$destinations)
palflow21 <- colorFactor(palette = "Dark2", flows21$destinations)
# 
# 
# #SHINY!!!!!!!!!!!!!!!!!!!
# # Choices for drop-downs
yearvars <- c(
  "2019" = "2019",
  "2020" = "2020",
  "2021" = "2021"
)
covidvars <- c(
  "Total Confirmed Cases" = "confirmed",
  "Active Cases" = "active",
  "Death Cases" = "death"
)

#UI!!!!!!!!!!!!!!!!!!
ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(title = "COVID-19 vs. Visitation in Harris County, Texas",
             id="nav",
             tabPanel("Changes in Visitation",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Changes in Visitation"),
                          tags$h6("Author: Ruben Lopez"), 
                        selectInput("year","Year",
                                                 choices = c("All","before covid", "after covid","difference - only for map", "2019","2020","2021")),
                                     radioButtons("type","type",
                                                  choices = c("All",unique(as.character(por_barplot$type)))),
                                     ),
                        mainPanel(leafletOutput("map_zipcode"),
                                  plotOutput("VisitationBarPlot", height = 350))
                      )
                      ),
             tabPanel("Indoor vs. Outdoor",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Indoor vs. Outdoor"),
                          h6("Author: Jiwoon Jeong"),
                          selectInput("year1","Year",
                                      choices = c("All","before covid", "after covid","difference - only for map", "2019","2020","2021")),
                          radioButtons("type1","type",
                                       choices = c("All", unique(as.character(por_barplot$type))))
                          ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visitationline Plot", 
                                     plotOutput("VisitationlinePlot")),
                            tabPanel("Visitationviolin Plot",  
                                     plotOutput("VisitationviolinPlot")))
                        )
                        )
                      ),
             tabPanel("Visitation Mobility",
                      sidebarLayout(
                        sidebarPanel(
                          h2("COVID vs POI"),
                          h6("Author: Yizhen Ding"),
                          selectInput("year2", "Year", yearvars),
                          selectInput("covid", "COVID Cases Type", covidvars),
                          plotOutput("scatterplot", height = 250)),
                      mainPanel(
                        leafletOutput("basemap", height=800))
                      )
                      ),
             tabPanel("About",
                      tags$h2("About the study "),
                      h4("Last update"),
                      "05 May 2022",
                      
                      tags$br(), tags$br(),h4("Introduction"),
                      "The COVID-19 pandemic has been the most critical global public health crisis during the past two years. However, the pandemic cannot yet be declared that it is over, and due to the prolonged period of the pandemic, people's visits to public places may be subject to change as a result of policy changes or as a result of acclimating to the pandemic. This study aims to understand changes to Arts, Entertainment, and Recreation-related public spaces visitation before and after the pandemic by using the commercial dataset SafeGraph.",
                      tags$br(),"The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through lo-cation-enabled applications. A POI is a point that represents the geographical location of a place that someone may find interesting. POIs were classified based on the North American Industry Classification System and the one categorized as 71- Arts, Entertainment, and Recreation were extracted. We look at the visitation to public places during the pandemic, starting from the beginning of 2019 to the end of 2021, and compare the visitation with those prior to the pandemic. Additionally, we examine which census block each visitor originates from. The second dataset we use is the COVID-19 daily confirmed cases and daily death cases.",
                      tags$br(), tags$br(),h4("Code"),
                      "Code and input data used to generate this Shiny mapping tool are available on Github:https://github.com/dolcejw324/Changes-in-Visitations-during-the-COVID-19",
                      tags$br(), tags$br(),h4("Sources"),
                      "Visitation data: Commercial dataset from SafeGraph. The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through location-enabled applications",
                      tags$br(),"Total confirmed COVID-19 cases: Johns Hopkins Center for Systems Science and Engineering",
                      tags$br(), tags$br(),h4("Authors"),
                      "Ruben Lopez, Texas A&M University",
                      tags$br()," Yizhen Ding, Texas A&M University",
                      tags$br()," Jiwoon Jeong, Texas A&M University")
    )
)
  

server <- function(input, output, session) {
  
  output$VisitationBarPlot = renderPlot({
    por_barplot0 <- por_barplot %>% group_by(type, start_date, year) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    if (input$year == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year != "All" & input$year != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year)
    }
    else {
    }
    if (input$type != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type)
    }
    ggplot(data=por_barplot0, aes(x=start_date, y=visitation_sum)) +
      geom_bar(stat="identity", fill="steelblue")+
      scale_x_date(date_breaks = "1 month", #labels
                   date_labels="%m-%y")+ #date_format("%d-%m-%Y"))+
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      #scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
      scale_y_continuous(labels = scales::label_number_si())+
      #labels = unit_format(unit = "M", scale = 1e-6))+
      xlab("Date (Month-Year)") +
      ylab("Number of visits")  +
      theme(axis.text.x=element_text(size=16,angle=90,hjust=1,vjust=0.5),
            axis.text.y=element_text(size=16))
  })
  
  output$map_zipcode <- renderLeaflet({
    if (input$type != "All") {
      por_barplot <- filter(por_barplot, type == input$type)
    }
    if (input$year == "after covid") {
      por_barplot <- filter(por_barplot, por_barplot$start_date > as.Date("2020-03-01"))
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else if (input$year == "before covid") {
      por_barplot <- filter(por_barplot, por_barplot$start_date < as.Date("2020-04-01"))
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else if (input$year == "difference - only for map") {
      por_barplot1 <- filter(por_barplot, por_barplot$start_date > as.Date("2020-03-01")) %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
      por_barplot0 <- filter(por_barplot, por_barplot$start_date < as.Date("2020-04-01")) %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
      por_barplot2 <- por_barplot0
      por_barplot2['visitation_sum'] <- por_barplot1$visitation_sum - por_barplot0$visitation_sum 
      por_barplot1 <- por_barplot2
    }
    else if (input$year != "All") {
      por_barplot <- filter(por_barplot, year == input$year)
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else {
      por_barplot1 <- por_barplot
    }
    por_barplot1$visitation_sum = por_barplot1$visitation_sum/1000000
    # add data to map
    orderzip <- match(map_zip@data$ZIP_CODE, por_barplot1$zipcode)
    map_zip@data <- por_barplot1[orderzip, ]
    pal <- colorNumeric(palette=matlab.like(256), domain = map_zip$visitation_sum)
    labels <- sprintf("<strong> Zip code: %s</strong><br/>Number of visits %g <sup>2</sup>", map_zip$zipcode, map_zip$visitation_sum) %>% lapply(htmltools::HTML)
    m <- leaflet(map_zip) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(visitation_sum),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~visitation_sum,
        opacity = 0.8, # title = NULL,
        title = "<small>Visits<br> per <br>million</small>"
        
      )
  })
  #second tab
  output$VisitationlinePlot <-  renderPlot({
    por_barplot <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
    por_barplot <- arrange(por_barplot, start_date, ZIP)
    por_barplot$ZIP = as.character(por_barplot$ZIP)
    names(por_barplot)[3] <- 'zipcode'
    por_barplot <- complete(por_barplot, type, start_date, zipcode, fill = list(visitation_sum = 0))
    por_barplot['year'] <- format(por_barplot$start_date, format="%Y")
    
    por_barplot2 <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(TotalConfirmedCases = sum(TotalConfirmedCases)) %>% as.data.frame()
    por_barplot2 <- arrange(por_barplot2, start_date, ZIP)
    por_barplot2$ZIP = as.character(por_barplot2$ZIP)
    names(por_barplot2)[3] <- 'zipcode'
    por_barplot2 <- complete(por_barplot2, type, start_date, zipcode, fill = list(visitation_sum = 0))
    por_barplot2['year'] <- format(por_barplot2$start_date, format="%Y")
    
    por_barplot2 <-  por_barplot2[, c('zipcode','TotalConfirmedCases')]
    por_barplot$TotalConfirmedCases= paste(por_barplot2$TotalConfirmedCases)
    por_barplot0<-por_barplot
    
    por_barplot0$visitation_sum <- as.numeric(por_barplot0$visitation_sum)
    por_barplot0$TotalConfirmedCases <- as.numeric(por_barplot0$TotalConfirmedCases)
    
    colors <- c("indoor" = "blue", "outdoor" = "red", "Total Confirmed Covid-19 Cases" = "green")
    
    #date_range <- which(por_barplot0$start_date %in% as.Date(c("2020-03-01", "2020-04-01")) )
    if (input$year1 == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year1 == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year1 != "All" & input$year1 != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year1)
    }
    else {
    }
    if (input$type1 != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type1)
    }
    ggplot(data=por_barplot0, aes(x=start_date, y=visitation_sum, color=type)) +
      stat_summary(aes(y = visitation_sum), fun=mean, geom = "line") +
      stat_summary(aes(y = TotalConfirmedCases, color="Total Confirmed Covid-19 Cases"), fun=mean, geom = "line", col = "green") +
      scale_x_date(date_breaks = "1 month", #labels
                   date_labels="%b-%Y")+ #date_format("%d-%m-%Y"))+
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      scale_y_continuous(labels = scales::label_number_si())+
      geom_vline(aes(xintercept = as.Date("2020-03-01")), col = "black")+
      geom_vline(aes(xintercept = as.Date("2020-04-01")), col = "black")+
      xlab("Date") +
      ylab("Number of visits") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title="Changes in Visitations", caption = "Black lines: when the lockdown policy started & ended in Texas")+
      scale_colour_manual(name="Lines",values=colors)
    
  })
  
  output$VisitationviolinPlot <- renderPlot({
    
    por_barplot0 <- por_barplot %>% group_by(type, start_date, year) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    if (input$year1 == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year1 == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year1 != "All" & input$year1 != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year1)
    }
    else {
    }
    if (input$type1 != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type1)
    }
    ggplot(data=por_barplot0, aes(x=type, y=visitation_sum, color=type)) +
      geom_violin()+
      geom_point()+
      #scale_x_date(date_breaks = "1 month", #labels
      #             date_labels="%b-%Y")+ #date_format("%d-%m-%Y"))+
      #geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      scale_y_continuous(labels = scales::label_number_si())+
      xlab("Type") +
      ylab("Number of visits") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title="Distribution of Visitations")
    
  }) 
  #basemap
  output$basemap <- renderLeaflet({ 
    if(input$year2 == "2019" & input$covid=="confirmed"){
      basedata=confirmed
      pal=palconfirmed
      poim=poi19map
      flow=flows19
      palflow=palflow19
    }
    if (input$year2 == "2019" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi19map
      flow=flows19
      palflow=palflow19
    }
    if (input$year2 == "2019"& input$covid=="death"){
      basedata=death
      pal=paldeath
      poim=poi19map
      flow=flows19
      palflow=palflow19
    }
    if(input$year2 == "2020" & input$covid=="confirmed"){
      basedata=confirmed
      pal=palconfirmed
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    if (input$year2 == "2020" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    if (input$year2 == "2020"& input$covid=="death"){
      basedata=death
      pal=paldeath
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    if(input$year2 == "2021" & input$covid=="confirmed"){
      basedata=confirmed
      pal=palconfirmed
      poim=poi21map
      flow=flows21
      palflow=palflow21
    }
    if (input$year2 == "2021" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi21map
      flow=flows21
      palflow=palflow21
    }
    if (input$year2 == "2021"& input$covid=="death"){
      basedata=death
      pal=paldeath
      poim=poi21map
      flow=flows21
      palflow=palflow21
    }
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-95.363972, 29.762664, zoom = 9) %>%
      addPolygons(data=basedata,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.5,
                  color = ~ pal(cases/TotalPop*100),
                  popup = ~paste('<strong>ZIP: </strong>',ZIP,
                                 "<br>Cases:", cases,
                                 "<br>Population:", TotalPop,
                                 "<br>Cases per 100 people:", cases/TotalPop*100)) %>%
      addLegend(data=basedata,
                "bottomright", 
                pal = pal, 
                values = ~cases/TotalPop*100,
                title = "Total Cases per 100 people",
                opacity = 1) %>%
      
      addCircles(data=poim,
                 lng = ~lon.end, lat = ~lat.end, 
                 weight = 1,
                 radius = ~log10(raw_visitor_counts)*100, 
                 popup = ~paste('<strong>Name: </strong>',location_name,
                                '<br>Top Category:', top_category,
                                '<br>Sub Category:', sub_category,
                                '<br>Total visitor:',raw_visitor_counts)) %>%
      
      addPolylines(data = flow, weight = ~log10(counts),
                   group = ~destinations, color = ~palflow(destinations),
                   opacity = 1) %>%
      
      addLayersControl(overlayGroups = unique(flow$destinations), 
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  #scatter plot
  output$scatterplot <- renderPlot({
    if (input$year2 == "2019" & input$covid=="confirmed") {
      plot(poi19_co$TotalConfirmedCases/poi19_co$TotalPop, poi19_co$vhc_number/poi19_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    } 
    if (input$year2 == "2019" & input$covid=="active") {
      plot(poi19_co$ActiveCases/poi19_co$TotalPop, poi19_co$vhc_number/poi19_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    if (input$year2 == "2019"& input$covid=="death") {
      plot(poi19_co$Death/poi19_co$TotalPop, poi19_co$vhc_number/poi19_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    if (input$year2 == "2020" & input$covid=="confirmed") {
      plot(poi20_co$TotalConfirmedCases/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    } 
    if (input$year2 == "2020"& input$covid=="active") {
      plot(poi20_co$ActiveCases/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    if (input$year2 == "2020"& input$covid=="death") {
      plot(poi20_co$Death/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    if (input$year2 == "2021" & input$covid=="confirmed") {
      plot(poi21_co$TotalConfirmedCases/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    } 
    if (input$year2 == "2021"& input$covid=="active") {
      plot(poi21_co$ActiveCases/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    if (input$year2 == "2021"& input$covid=="death") {
      plot(poi21_co$Death/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
  })
}

shinyApp(ui = ui, server = server)


rsconnect::deployApp()

