# library(scales)
# library(lattice)
# library(dplyr)
# library(tidycensus)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(readr)
# library(tigris)
# library(sp)
# library(rgdal)
# library(rgeos)
# library(sf)
# library(terra)
# library(proj4)
# library(leaflet)
# library(RColorBrewer)
# library(geosphere)
# library(magrittr)
# library(shiny)
# library(ggplot2)
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
detach("package:xts", unload=TRUE)
library(rsconnect)
load("poi.Rda")
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
# map_zip <- readOGR("harris-county-tx-zipcodes.shp")
map_zip <- readOGR("COH_ZIPCODE.shp")
zipcode <- unique(poi_v2$ZIP)
# map_data <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
#tachometer
# save(por_barplot,file="data.Rda")
# save(zipcode,file="zipcode.Rda")
load("covid.Rda")
load("poi19_co.Rda")
load("poi20_co.Rda")
load("poi21_co.Rda")
# poi_barplot <- read_csv2("por_barplot.csv")
# # por_barplot <- read_csv2("por_barplot.csv")

# #run dataclean file first
# #US tract
# us_tract <- tracts(state = NULL, cb = TRUE)
# #convert coordinates to lat and lon
# us_tract$geometry <- st_transform(us_tract$geometry, crs = 4326)
# us_tract$census_centroids <- st_transform(us_tract$geometry, crs = 4326) %>% 
#   st_centroid()
# sf <- st_as_sf(us_tract$census_centroids, coords = c("lon","lat")) %>% 
#   st_set_crs(4326)
# 
# coords <- unlist(st_geometry(sf)) %>% 
#   matrix(ncol=2,byrow=TRUE) %>% 
#   as_tibble() %>% 
#   setNames(c("lon","lat"))
# 
# lonlat <- as.data.frame(coords)
# us_tract <- data.frame(us_tract, lonlat)
# 
# #check<- read_csv("G:/Shared drives/Safegraph/Safegraph_Houston_Final/poi2019_edited.csv")
# #Data cleaning part 19
# sg19 <-  read_csv("G:/Shared drives/Safegraph/Safegraph_Houston_Final/poi2019_edited.csv")
# #split 'date_range_start', 'date_range_end' column using ' ' as the separator
# #split variable "date_range_start" into two variables, "start_date" and "start_time".
# sg19[c('start_date', 'start_time')] <- str_split_fixed(sg19$date_range_start, ' ', 2)
# #split variable "date_range_end" into two variables, "end_date" and "end_time".
# sg19[c('end_date', 'end_time')] <- str_split_fixed(sg19$date_range_end, ' ', 2)
# #remove useless variables.
# sg19 <- sg19[,-which(names(sg19) %in% c("date_range_start", "date_range_end", "start_time", "end_time","bucketed_dwell_times", "visitor_home_cbgs", "popularity_by_day", "visitor_daytime_cbgs", "popularity_by_hour", "visitor_country_of_origin"))]
# #First we checked the maximum number of columns we need if separate the "visitor_home_aggregation" by ","
# max(sapply(strsplit(as.character(sg19$visitor_home_aggregation),','),length))
# #The result showed we need 5127 column, which means the maximum number of census block a place has is 5127
# sg19<-separate(sg19, visitor_home_aggregation, paste0("vha",1:5127), sep=",")
# sg19 <- gather(sg19, vha, visitor_home_census, vha1:vha5127, factor_key=TRUE)
# sg19 <- sg19[,-which(names(sg19) %in% c("vha"))]
# sg19<-sg19 %>% drop_na(visitor_home_census)
# sg19[c('visitor_home_census', 'vhc_number')] <- str_split_fixed(sg19$visitor_home_census, ':', 2)
# sg19[sg19==""]<-NA
# sg19<-sg19 %>% drop_na(vhc_number)
# sg19$visitor_home_census<-gsub("[^[:alnum:]]", " ", sg19$visitor_home_census)
# sg19$visitor_home_census<-gsub("[[:space:]]", "", sg19$visitor_home_census)
# sg19 <- sg19 %>%
#   rename(ZIP = postal_code)
# sg19 <- sg19 %>%
#   rename(GEOID = visitor_home_census)
# sg19$ZIP <- as.character(sg19$ZIP)
# sg19$vhc_number <- as.numeric(sg19$vhc_number)
# sg19[1108168, 25] = 4
# 
# #census tracts 
# sg19 <- sg19 %>%
#   left_join(us_tract, by="GEOID")
# 
# sg19 <- sg19[, c('ZIP','placekey','location_name','top_category','sub_category', 'GEOID', 'vhc_number','longitude','latitude','raw_visitor_counts', 'start_date','end_date','geometry', 'lon', 'lat')]
# 
# sg19 <- sg19 %>% 
#   rename(
#     lat.end = latitude,
#     lon.end = longitude,
#     lat.start = lat,
#     lon.start = lon
#   )
# 
# #Data cleaning part 20
# sg20 <-  read_csv("G:/Shared drives/Safegraph/Safegraph_Houston_Final/poi2020.csv")
# #split 'date_range_start', 'date_range_end' column using ' ' as the separator
# #split variable "date_range_start" into two variables, "start_date" and "start_time".
# sg20[c('start_date', 'start_time')] <- str_split_fixed(sg20$date_range_start, ' ', 2)
# #split variable "date_range_end" into two variables, "end_date" and "end_time".
# sg20[c('end_date', 'end_time')] <- str_split_fixed(sg20$date_range_end, ' ', 2)
# #remove useless variables.
# sg20 <- sg20[,-which(names(sg20) %in% c("date_range_start", "date_range_end", "start_time", "end_time","bucketed_dwell_times", "visitor_home_cbgs", "popularity_by_day", "visitor_daytime_cbgs", "popularity_by_hour", "visitor_country_of_origin"))]
# #First we checked the maximum number of columns we need if separate the "visitor_home_aggregation" by ","
# max(sapply(strsplit(as.character(sg20$visitor_home_aggregation),','),length))
# #The result showed we need 3836 column, which means the maximum number of census block a place has is 5127
# sg20<-separate(sg20, visitor_home_aggregation, paste0("vha",1:3836), sep=",")
# sg20 <- gather(sg20, vha, visitor_home_census, vha1:vha3836, factor_key=TRUE)
# sg20 <- sg20[,-which(names(sg20) %in% c("vha"))]
# sg20<-sg20 %>% drop_na(visitor_home_census)
# sg20[c('visitor_home_census', 'vhc_number')] <- str_split_fixed(sg20$visitor_home_census, ':', 2)
# sg20[sg20==""]<-NA
# sg20<-sg20 %>% drop_na(vhc_number)
# sg20$visitor_home_census<-gsub("[^[:alnum:]]", " ", sg20$visitor_home_census)
# sg20$visitor_home_census<-gsub("[[:space:]]", "", sg20$visitor_home_census)
# sg20<-sg20[!(sg20$visitor_home_census=="CA"),]
# sg20 <- sg20 %>%
#   rename(ZIP = postal_code)
# sg20 <- sg20 %>%
#   rename(GEOID = visitor_home_census)
# sg20$ZIP <- as.character(sg20$ZIP)
# sg20$vhc_number <- as.numeric(sg20$vhc_number)
# sg20[786824, 26] = 4
# 
# #census tracts 
# sg20 <- sg20 %>%
#   left_join(us_tract, by="GEOID")
# 
# sg20 <- sg20[, c('ZIP','placekey','location_name','top_category','sub_category', 'GEOID', 'vhc_number','longitude','latitude','raw_visitor_counts', 'start_date','end_date','geometry', 'lon', 'lat')]
# 
# sg20 <- sg20 %>% 
#   rename(
#     lat.end = latitude,
#     lon.end = longitude,
#     lat.start = lat,
#     lon.start = lon
#   )
# 
# #Data cleaning part 21
# sg21 <-  read_csv("G:/Shared drives/Safegraph/Safegraph_Houston_Final/2021_edited.csv")
# #split 'date_range_start', 'date_range_end' column using ' ' as the separator
# #split variable "date_range_start" into two variables, "start_date" and "start_time".
# sg21[c('start_date', 'start_time')] <- str_split_fixed(sg21$date_range_start, ' ', 2)
# #split variable "date_range_end" into two variables, "end_date" and "end_time".
# sg21[c('end_date', 'end_time')] <- str_split_fixed(sg21$date_range_end, ' ', 2)
# #remove useless variables.
# sg21 <- sg21[,-which(names(sg21) %in% c("date_range_start", "date_range_end", "start_time", "end_time","bucketed_dwell_times", "visitor_home_cbgs", "popularity_by_day", "visitor_daytime_cbgs", "popularity_by_hour", "visitor_country_of_origin"))]
# #First we checked the maximum number of columns we need if separate the "visitor_home_aggregation" by ","
# max(sapply(strsplit(as.character(sg21$visitor_home_aggregation),','),length))
# #The result showed we need 2356 column, which means the maximum number of census block a place has is 5127
# sg21<-separate(sg21, visitor_home_aggregation, paste0("vha",1:2356), sep=",")
# sg21 <- gather(sg21, vha, visitor_home_census, vha1:vha2356, factor_key=TRUE)
# sg21 <- sg21[,-which(names(sg21) %in% c("vha"))]
# sg21<-sg21 %>% drop_na(visitor_home_census)
# sg21[c('visitor_home_census', 'vhc_number')] <- str_split_fixed(sg21$visitor_home_census, ':', 2)
# sg21[sg21==""]<-NA
# sg21<-sg21 %>% drop_na(vhc_number)
# sg21$visitor_home_census<-gsub("[^[:alnum:]]", " ", sg21$visitor_home_census)
# sg21$visitor_home_census<-gsub("[[:space:]]", "", sg21$visitor_home_census)
# sg21<-sg21[!(sg21$visitor_home_census=="CA"),]
# sg21 <- sg21 %>%
#   rename(ZIP = postal_code)
# sg21 <- sg21 %>%
#   rename(GEOID = visitor_home_census)
# sg21$ZIP <- as.character(sg21$ZIP)
# sg21$vhc_number <- as.numeric(sg21$vhc_number)
# #census tracts 
# sg21 <- sg21 %>%
#   left_join(us_tract, by="GEOID")
# 
# sg21 <- sg21[, c('ZIP','placekey','location_name','top_category','sub_category', 'GEOID', 'vhc_number','longitude','latitude','raw_visitor_counts', 'start_date','end_date','geometry', 'lon', 'lat')]
# 
# sg21 <- sg21 %>% 
#   rename(
#     lat.end = latitude,
#     lon.end = longitude,
#     lat.start = lat,
#     lon.start = lon
#   )
# 
# #harris zip covid
# covid <- read_csv("G:/Shared drives/Safegraph/Harris county covid cases/Download_COVID_Cases_By_Zip_Codes.csv")
# #Only keep data for Houston
# covid <- covid[which(covid$POSTAL=="HOUSTON"),]
# covid$ZIP <- as.character(covid$ZIP)
# harris_zip <- zctas(cb = FALSE, starts_with = c("77001","77002","77003","77004","77005","77005","77006","77007","77008","77009","77010","77011","77012","77013","77014","77015","77016","77017","77018","77019","77020","77021","77022","77023","77024","77025","77026","77027","77028","77029","77029","77030","77030","77031","77032","77033","77034","77035","77036","77037","77038","77039","77040","77040","77041","77041","77042","77043","77044","77045","77046","77046","77047","77048","77049","77050","77051","77052","77054","77055","77056","77057","77058","77059","77060","77061","77062","77063","77064","77065","77065","77066","77067","77068","77069","77070","77071","77072","77073","77074","77075","77076","77077","77078","77079","77080","77081","77082","77083","77084","77085","77086","77087","77088","77089","77090","77091","77092","77093","77094","77095","77096","77097","77098","77099","77201","77202","77203","77205","77206","77207","77208","77210","77213","77215","77217","77218","77219","77220","77221","77222","77223","77224","77225","77226","77227","77228","77229","77230","77231","77233","77234","77235","77236","77237","77238","77240","77241","77242","77243","77244","77245","77248","77249","77251","77252","77253","77254","77255","77256","77257","77258","77259","77261","77262","77263","77265","77266","77267","77268","77269","77270","77271","77272","77273","77274","77275","77277","77279","77280","77281","77282","77284","77287","77288","77289","77290","77291","77292","77293","77315","77325","77325","77336","77337","77337","77338","77339","77339","77345","77345","77346","77346","77346","77347","77373","77375","77377","77379","77379","77383","77388","77389","77389","77391","77391","77396","77401","77402","77410","77411","77413","77429","77433","77447","77449","77449","77450","77450","77491","77492","77493","77493","77501","77502","77503","77504","77505","77506","77507","77508","77520","77521","77522","77530","77532","77532","77536","77547","77562","77571","77571","77572","77586","77586","77586","77587","77598"), state="48", year=2010)
# harris_zip <- harris_zip %>%
#   rename(ZIP = ZCTA5CE10)
# covid <- harris_zip %>%
#   left_join(covid, by="ZIP")
# covid$geometry <- st_transform(covid$geometry, crs = 4326)
# covid <- covid[, -which(names(covid) %in% c("POSTAL","code", "OBJECTID", "ActiveCases_str", "Death_str", "STATE", "DATE_MOD", "ZIP_TYPE", "GDB_GEOMATTR_DATA", "EditDate", "Today", "Shape_STArea_1", "Shape_STLength_1", "Shape__Area", "Shape__Length"))]
# covid <- covid[ -c(1,3:11) ]
# #POI data all
# poi19 <- sg19[,c('location_name','top_category','sub_category','lon.start','lat.start','lon.end','lat.end','vhc_number', 'ZIP', 'raw_visitor_counts', 'start_date','end_date')]
# poi20 <- sg20[,c('location_name','top_category','sub_category','lon.start','lat.start','lon.end','lat.end','vhc_number', 'ZIP', 'raw_visitor_counts', 'start_date','end_date')]
# poi21 <- sg21[,c('location_name','top_category','sub_category','lon.start','lat.start','lon.end','lat.end','vhc_number', 'ZIP', 'raw_visitor_counts', 'start_date','end_date')]
# 
# #POI and COVID Data join
# poi19_co <- poi19 %>%
#   left_join(covid, by="ZIP")
# poi19_co <-na.omit(poi19_co)
# poi20_co <- poi20 %>%
#   left_join(covid, by="ZIP")
# poi20_co <-na.omit(poi20_co)
# poi21_co <- poi21 %>%
#   left_join(covid, by="ZIP")
# poi21_co <-na.omit(poi21_co)
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





{ui <- dashboardPage(
  dashboardHeader(title = "Safegraph visitation data exploration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Changes in Visitations", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Indoor vs. Outdoor", tabName = "dashboard1", icon = icon("tachometer-alt")),
      menuItem("Mobility of People", tabName = "dashboard2", icon = icon("tachometer-alt")),
      menuItem("About", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      {tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Controls", width = 3, #solidHeader = TRUE,
                  selectInput(inputId = "year",
                              label = "Year",
                              choices = c("All","before covid", "after covid", 
                                          "difference - only for map", "2019","2020","2021")
                  ),
                  radioButtons(inputId ="type",label = "type",choices = c("All",
                                                                          unique(as.character(por_barplot$type)))
                  )
                )
                ,
                box(  title = "Visitation Map", width = 9, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, leafletOutput(outputId = "map_zipcode")),
                
                
              ),
              fluidRow(
                box(  title = "Visitation stats", width = 12, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("VisitationBarPlot", height = 350)),
                
                
              )
      )
      }
      ,
      {tabItem(tabName = "dashboard1",
              fluidRow(
                box(
                  title = "Controls", width = 3, #solidHeader = TRUE,
                  selectInput(inputId = "year1",
                              label = "Year",
                              choices = c("All","before covid", "after covid", 
                                          "difference - only for map", "2019","2020","2021")
                  ),
                  radioButtons(inputId ="type1",label = "type",choices = c("All",
                                                                           unique(as.character(por_barplot$type)))
                  )
                )
                ,
                box(  title = "Visitation line plot", width = 9, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("VisitationlinePlot")),
                
                
              ),
              fluidRow(
                box(  title = "Visitation violin plot", width = 12, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("VisitationviolinPlot", height = 350)),
                
              )
      )
      },
      {tabItem(tabName = "dashboard2",
              fluidRow(
                # fluidPage(
                # sidebarPanel(
                #   h2("COVID Cases vs Mobility"),
                #   selectInput("year", "Year", yearvars),
                #   selectInput("covid", "COVID Cases Type", covidvars),
                #   plotOutput("scatterplot", height = 250)),
                # mainPanel(
                #   leafletOutput("basemap",width = "100%",height = "800") )
                box(
                  title = "Controls", width = 3, #solidHeader = TRUE,
                  selectInput(inputId = "year2", label = "Year",choices = yearvars),
                  selectInput(inputId = "covid", label = "COVID Cases Type",choices = covidvars)
                )
                ,
                box(  title = "COVID Cases vs Mobility", width = 6, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("scatterplot", height = 200)),
                
                
              ),
              fluidRow(
                box(title = "Mobility Map", width = 12, status = "primary", #solidHeader = TRUE,
                    collapsible = TRUE, leafletOutput(outputId = "basemap",height = 400)
                )
              )
              
              
      )
      }
      ,
      {tabItem(tabName = "widgets",
              h2("About the study "),
              h4("Last update"),
              "03 May 2022",
              h4("Introduction"),
              "The COVID-19 pandemic has been the most critical global public health crisis during the past two years. However, the pandemic cannot yet be declared that it is over, and due to the prolonged period of the pandemic, people's visits to public places may be subject to change as a result of policy changes or as a result of acclimating to the pandemic. This study aims to understand changes to Arts, Entertainment, and Recreation-related public spaces visitation before and after the pandemic by using the commercial dataset SafeGraph.",
              "The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through lo-cation-enabled applications. A POI is a point that represents the geographical location of a place that someone may find interesting. POIs were classified based on the North American Industry Classification System and the one categorized as 71- Arts, Entertainment, and Recreation were extracted. We look at the visitation to public places during the pandemic, starting from the beginning of 2019 to the end of 2021, and compare the visitation with those prior to the pandemic. Additionally, we examine which census block each visitor originates from. The second dataset we use is the COVID-19 daily confirmed cases and daily death cases.",
              h4("Code"),
              "Code and input data used to generate this Shiny mapping tool are available on Github:https://github.com/dolcejw324/Changes-in-Visitations-during-the-COVID-19",
              h4("Sources"),
              "Visitation data: Commercial dataset from SafeGraph. The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through location-enabled applications",
              "Total confirmed COVID-19 cases: Johns Hopkins Center for Systems Science and Engineering",
              
              h4("Authors"),
              "Ruben Lopez, Texas A&M University /",
              " Yizhen Ding, Texas A&M University /",
              " Jiwoon Jeong, Texas A&M University"
      )
      }
    )
  )
)
  
}

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
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
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
    else if (input$year2 == "2019" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi19map
      flow=flows19
      palflow=palflow19
    }
    else if (input$year2 == "2019"& input$covid=="death"){
      basedata=death
      pal=paldeath
      poim=poi19map
      flow=flows19
      palflow=palflow19
    }
    else if(input$year2 == "2020" & input$covid=="confirmed"){
      basedata=confirmed
      pal=palconfirmed
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    else if (input$year2 == "2020" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    else if (input$year2 == "2020"& input$covid=="death"){
      basedata=death
      pal=paldeath
      poim=poi20map
      flow=flows20
      palflow=palflow20
    }
    else if(input$year2 == "2021" & input$covid=="confirmed"){
      basedata=confirmed
      pal=palconfirmed
      poim=poi21map
      flow=flows21
      palflow=palflow21
    }
    else if (input$year2 == "2021" & input$covid=="active"){
      basedata=active
      pal=palactive
      poim=poi21map
      flow=flows21
      palflow=palflow21
    }
    else if (input$year2 == "2021"& input$covid=="death"){
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
                                '<br>ub Category:', sub_category,
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
    else if (input$year2 == "2019" & input$covid=="active") {
      plot(poi19_co$ActiveCases/poi19_co$TotalPop, poi19_co$vhc_number/poi19_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    else if (input$year2 == "2019"& input$covid=="death") {
      plot(poi19_co$Death/poi19_co$TotalPop, poi19_co$vhc_number/poi19_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    else if (input$year2 == "2020" & input$covid=="confirmed") {
      plot(poi20_co$TotalConfirmedCases/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    } 
    else if (input$year2 == "2020"& input$covid=="active") {
      plot(poi20_co$ActiveCases/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    else if (input$year2 == "2020"& input$covid=="death") {
      plot(poi20_co$Death/poi20_co$TotalPop, poi20_co$vhc_number/poi20_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    else if (input$year2 == "2021" & input$covid=="confirmed") {
      plot(poi21_co$TotalConfirmedCases/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    } 
    else if (input$year2 == "2021"& input$covid=="active") {
      plot(poi21_co$ActiveCases/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
    else if (input$year2 == "2021"& input$covid=="death") {
      plot(poi21_co$Death/poi21_co$TotalPop, poi21_co$vhc_number/poi21_co$TotalPop, xlab="Confirmed Cases per population unit of ZIP", 
           ylab="Mobility Frequency of ZIP")
    }
  })
}

shinyApp(ui = ui, server = server)


rsconnect::deployApp()
