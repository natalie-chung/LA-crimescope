library(shiny)
library(ggplot2)
library(shinyTime)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(jsonlite)
library(shinydashboard)
library(htmltools)
library(dplyr)
library(stringr)
library(xml2)
library(treemap)
library(XML)
library(rvest)
library(tidyr)
library(curl)
library(magrittr)
library(stringr)
library(dslabs)
library(ggmap)
library(rgdal)
library(raster)
library(ggrepel)
library(viridis)
library(treemap)
library(devtools)
library(treemapify)
library(htmlwidgets)
devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)
library(randomcoloR)
library(shinythemes)
library(plotly)
library(lubridate)
library(formattable)
library(RColorBrewer)
library(shinyWidgets)

# Load static data 
crimecodes <- read.csv("crimecodes.csv")
stations <- read.csv("Sheriff_and_Police_Stations.csv")

#LA kml map
## Additional functions (not reactive)
kml_points <- function(x, layer = "d1", verbose = TRUE) {
  require(dplyr)
  require(stringr)
  require(xml2)
  
  #' Extract Placemark fields.
  #' 
  #' @param x A nodeset of Placemarks.
  #' @param field The name of the field to extract, e.g. \code{"name"}.
  #' @param layer The name of the layer to extract from; defaults to \code{"d1"}.
  #' @return A character vector. Missing values, i.e. empty fields, will be
  #' returned as \code{NA} values.
  get_field <- function(x, field, layer = "d1") {
    
    # vectorization required to get missing values when field is xml_missing
    lapply(x, xml_find_first, str_c(layer, ":", field)) %>%
      sapply(xml_text)
  }
  
  x <- read_xml(x) %>%
    xml_find_all(str_c("//", layer, ":Point/.."))

  x <- data_frame(
    name = get_field(x, "name", layer),
    description = get_field(x, "description", layer),
    styleUrl = get_field(x, "styleUrl", layer),
    coordinates = get_field(x, str_c("Point/", layer, ":coordinates"), layer)
  )
  
  x$longitude <- kml_coordinate(x$coordinates, 1, verbose)
  x$latitude  <- kml_coordinate(x$coordinates, 2, verbose)
  x$altitude  <- kml_coordinate(x$coordinates, 3, verbose)
  
  return(dplyr::select(x, -coordinates))
}

kml_coordinate <- function(x, coord, verbose = TRUE) {
  require(stringr) # includes `%>%`
  x <- str_replace(x, "(.*),(.*),(.*)", str_c("\\", coord)) %>%
    as.numeric
  if (verbose && coord == 1 && any(abs(x) > 180))
    message("Some longitudes are not contained within [-180, 180].")
  if (verbose && coord == 2 && any(abs(x) > 90))
    message("Some latitudes are not contained within [-90, 90].")
  if (verbose && coord == 3 && any(x < 0))
    message("Some altitudes are below sea level.")
  return(x)
}

attractions <- kml_points("Los Angeles map.kml")

# Define UI for app

ui <- dashboardPage(
  
  skin="red",
  
  dashboardHeader(title = span(tagList(icon("crosshairs", lib="font-awesome"), "Crimescope: LA"))),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Map View", tabName = "map", icon = icon("map")),
    menuItem("Crime Type", tabName = "type", icon = icon("bars")),
    menuItem("Victim Analysis", tabName = "victim", icon = icon("user")), 
    menuItem("Fun Fact", tabName = "FunFact", icon = icon("bomb")),
    menuItem("About", tabName = "About", icon = icon("info"))
  ), hr(), 
  out = h6("Disclaimer: Reliance on past information may not accurately reflect the current situation")),
  
  dashboardBody(
    
    tabItems(
      # First tab content 
      tabItem(tabName = "dashboard",
              h5(
                "Last Updated:  ",
                textOutput("latestdate", container = span)
              ),
              fluidRow(
                valueBoxOutput("vbox"),
                valueBoxOutput("vbox2"),
                valueBoxOutput("vbox3")
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel(
                    status = "primary",
                    title = "Trend of no. of crimes",
                    withSpinner(plotlyOutput("trend", height = "300px"))
                  ),
                  tabPanel(
                    status = "success",
                    title = "Trend of no. of crimes in week",
                    withSpinner(plotlyOutput("trend2", height = "300px"))
                  )
                )
              ),
              box(
                title = "Spread of Crimes This Month (Area)",
                background = "yellow",
                collapsible = TRUE,
                plotOutput("area", height = 250)
              ),
              box(
                title = "Consititution of Crime Type This Month",
                background = "yellow",
                collapsible = TRUE,
                plotOutput("area2", height = 250)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "map",
              h2("Map View"),
              fluidRow(
                # left side - map 
                column(width=9, 
                       box(width=NULL, height="600px", 
                           withSpinner(leafletOutput("PlotCrime", height=570)))
                ),
                # right side - boxes to change map 
                column(width=3, 
                       box(width=NULL, 
                           dateInput(inputId = "date1", 
                                     label="Date: ", 
                                     value="2021-01-01")), 
                       box(width=NULL, 
                           radioButtons(inputId = "format", 
                                        label="View Format", 
                                        choices = c("Point","Heatmap"))), 
                       box(width=NULL,
                           checkboxGroupInput(inputId="crimetype", 
                                              label="Type of Crime:", 
                                              choices=sort(unique(crimecodes$Type)), 
                                              selected=sort(unique(crimecodes$Type))) 
                       )
                )
              )
      ),
      
      # Third Tab Content
      tabItem(tabName = "type",
              h2("Crime Type"),
              helpText("Please wait a while for the data to load..."), 
              fluidRow(
                tabBox(title="Crime over time", side="right", id="tabset1", width=12,
                       tabPanel("Hour", "Crime by Hour",
                                plotOutput("crimehour", width="100%") %>% withSpinner()),
                       tabPanel("Day", "Crime by Day", 
                                plotOutput("crimeday", width="100%") %>% withSpinner()), 
                       tabPanel("Hour & Day", "Crime by Hour & Day", 
                                plotOutput("crimehourday", width="100%") %>% withSpinner()), 
                       tabPanel("Hour & Day Treemap", "Treemap of Crime by Hour & Day", 
                                plotOutput("crimehourdayheatmap", width="100%") %>% withSpinner()), 
                       tabPanel("Month", "Crime by Month", 
                                plotOutput("crimemonth", width="100%") %>% withSpinner()), 
                       tabPanel("Year", "Crime by Year", 
                                plotOutput("crimeyear", width="100%") %>% withSpinner()))
                ),
              fluidRow(
                tabBox(title="Top 20 Crimes", side="right", id="tabset2", width=12, 
                       tabPanel("Bar Chart", "Top 20 Crimes", 
                                plotOutput("top20crimes", width="100%") %>% withSpinner()),
                       tabPanel("Treemap", "Treemap of Top 20 Crimes", 
                                plotOutput("top20crimestreemap", width="100%") %>% withSpinner()))
                ), 
              fluidRow(
                tabBox(title="Proportion of Crime", side="right", id="tabset3", width=12,
                       tabPanel("Year", "Proportion of Crime by Year", 
                                plotOutput("propcrimeyear", width="100%") %>% withSpinner()),
                       tabPanel("Area", "Proportion of Crime by Area",
                                d3treeOutput("crimesbyareatreemap", width="100%") %>% withSpinner())
                      )
                )
      ), 
      
      # Fourth Victim Tab
      tabItem(tabName = "victim",
              h2("Victim Analysis"),
              helpText("Filter crime types based on victim attributes"),
              fluidRow(plotOutput("vict_analy") %>% withSpinner()),
              box(
                title = "Choose your attributes", 
                selectInput("Race", "Choose a race:",
                            list("Black", "Hispanic/Latin/Mexican",        
                                 "White", "Unknown", "Other Asian", "Other", 
                                 "Chinese", "Filipino", "Korean", 
                                 "American Indian/Alaskan Native", "Vietnamese",
                                 "Asian Indian", "Japanese", "Pacific Islander",
                                 "Guamanian", "Hawaiian", "Cambodian", 
                                 "Samoan", "Laotian" )
                ),
                selectInput("Sex", "Choose a sex:",
                            list("F", "M", "Unknown")
                ),
                selectInput("Age", "Choose an age group:",
                            list("0-17", "18-29", "30-59", "60-79", ">80")
                ),
                textOutput("result")
              )
      ),
      
      # Fifth Fun Fact Tab
      tabItem(tabName = "FunFact",
              h2("Weapons Used by Crime Type"),
              fluidRow(plotlyOutput("weapon") %>% withSpinner()),
              box(
                title = "Controls",
                dateInput(inputId = "date2", "Date:", value="2021-01-01"))
      ),
      
      tabItem(tabName = "About",
              h2("About this project"),
              br(),
                 h5("The Project is Built by Team 7:", align = "center"),
                br(),
                 h5("Leung Le Yi ", align = "center"),
                 h5("Lobeck Antonio Friedrich ", align = "center"),
                 h5("Natalie Chung Hui En", align = "center"),
                 h5("Shermin Ong", align = "center"),
                 h5("Sun Jiaze", align = "center"),
              br(),
              h3("Future updates of this project may include: "),
              br(),
              p("1. Add more functionalities, such as incorporating the users’ current locations to give suggestions to better protect the users’ safety."), 

              p("2. Include datasets containing other parts of the United States or beyond the country, enabling users to refer to the app when travelling outside of LA."),

              p("3. Further adapt the app to accommodate more complex and detailed visualisation to suit different users’ needs, such as the police force or government institutions.") 
                 )
    )
  )
)

# Define server logic  
server <- function(input, output) {
  #The function to get data for given date
  #date must in format "yyyy-MM-dd"
  
  #dashboard functions 
  getFullCrimeData2 <- reactive({
    url = "https://data.lacity.org/resource/2nrs-mtv8.json?$limit=50000"
    data <- fromJSON(url)
    data <- as.data.frame(data)
    
    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)
    data$lat[data$lat==0] <- NA
    data$lon[data$lon==0] <- NA
    
    data <- data %>% filter(!is.na(lon) & !is.na(lat))
    data$date_occ2 <- with(data, substring(as.character(date_occ),1,10))
    data$date_rptd2 <- with(data, substring(as.character(date_rptd),1,10))
    #data$year <- with(data, substring(as.character(date_occ),1,4))
    data2 <- data[rev(order(as.POSIXct(data$date_rptd, tz = "GMT"))),]
    data2$yr <- year(data2$date_rptd)
    data2$month <- month(as.POSIXlt(data2$date_rptd))
    data2$wday <- wday(data2$date_rptd, label=TRUE)
    #View(head(data2))
    data2
  })
  
  output$latestdate <- renderText({
    
    as.character(head(getFullCrimeData2(),1)$date_rptd)
  })
  
  getThisMonth <- reactive({
    this.month <- getFullCrimeData2() %>% 
      filter(yr == head(getFullCrimeData2(),1)$yr) %>% 
      filter(month == head(getFullCrimeData2(),1)$month)
    this.month
  })
  
  getTypeThisMonth <- reactive({
    type <- getThisMonth () %>% 
      group_by(crm_cd_desc) %>%
      summarize(c = n())
    type <- type %>% arrange(desc(c))
    type
  })
  
  getTotalThisMonth <- reactive({
    #View(head(getFullCrimeData2()))
    this.month <- getFullCrimeData2() %>% 
      filter(yr == head(getFullCrimeData2(),1)$yr) %>% 
      filter(month == head(getFullCrimeData2(),1)$month)
    n <- nrow(this.month)
    n
  })
  
  getTotalLastMonth <- reactive({
    last.month <- getFullCrimeData2() %>% 
      filter(yr == head(getFullCrimeData2(),1)$yr) %>% 
      filter(month == head(getFullCrimeData2(),1)$month - 1)
    #View(last.month)
    n <- nrow(last.month)
    n
  })
  
  output$trend <- renderPlotly({
    #ggplot(getAllData(), aes(x=year, y=n, group=name, color=name)) +
    d <- getFullCrimeData2() %>% 
      filter(yr %in% c(2021,2022)) %>%
      group_by(month = lubridate::floor_date(as.Date(date_rptd2), "month")) %>%
      summarize(monthlyCount = n())
    
    ggplot(d, aes(x = month, y = monthlyCount)) + 
      geom_line(group = 1) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ylab("Number of crimes") +
      xlab("Month") +
      scale_x_date(date_breaks = "1 month")
  })
  
  output$trend2 <- renderPlotly({
    d <- getFullCrimeData2() %>% 
      filter(yr %in% c(2021,2022)) %>%
      group_by(wday) %>%
      summarize(Count = n())
    
    ggplot(d, aes(x = wday, y = Count)) + 
      geom_line(group = 1) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ylab("Number of crimes") +
      xlab("Day of the Week") 
  })
  
  output$area <- renderPlot({
    a <- getFullCrimeData2() %>% 
      filter(yr %in% c(2022)) %>%
      filter(month == head(getFullCrimeData2(),1)$month - 1) %>%
      group_by(area_name) %>%
      summarize(Count = n())
    # Render a barplot
    ggplot(a, aes(x = reorder(area_name,-Count), y = Count)) + 
      geom_bar(stat="identity",fill = "#CC6666") + 
      scale_fill_hue(c=45, l=80) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      xlab("Area Name")
  })
  
  output$area2 <- renderPlot({
    # Render a barplot
    data <- head(getTypeThisMonth(),4)
    
    data$fraction <- data$c / sum(data$c)
    data$ymax <- cumsum(data$fraction)
    data$ymin <- c(0, head(data$ymax, n=-1))
    data$labelPosition <- (data$ymax + data$ymin) / 2
    #data$label <- paste0(data$crm_cd_desc, "\n ", percent(data$fraction))
    data$label <- percent(data$fraction)
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=crm_cd_desc)) +
      geom_rect() +
      geom_text( x=2, aes(y=labelPosition, label=label, color=crm_cd_desc), size=4) + # x here controls label position (inner / outer)
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void()
    #labs(fill='Consititution of Crime Type This Month') 
    #guides(fill=guide_legend(title="Consititution of Crime Type This Month"))
    #theme(legend.position = "none")
  })
  
  output$vbox <- renderValueBox({
    valueBox(
      getTotalThisMonth(),
      "Total Crime This Month", 
      icon = icon("globe"),
      color = "yellow")
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      percent((getTotalThisMonth() - getTotalLastMonth()) / getTotalLastMonth()),
      "Increase/Decrease from Last Month", 
      icon = icon("thumbtack"),
      color = "orange")
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      #head(getTypeThisMonth()[order(-c),],1),
      head(getTypeThisMonth()$crm_cd_desc,1),
      "Most Common Crime Type This Month", 
      icon = icon("hand"),
      color = "red")
  })
  
  
  # Extract data for map tab ---------------------------------------------------
  getCrimeData <- reactive( {
    date <- as.character(input$date1)
    
    url = "https://data.lacity.org/resource/2nrs-mtv8.json?date_occ="
    url <- paste0(url,date,"T00:00:00.000")
    data <- fromJSON(url)
    data <- as.data.frame(data[c("lat", "lon", "crm_cd", "crm_cd_desc")])
    
    # Clean data
    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)
    data$lat[data$lat==0] <- NA
    data$lon[data$lon==0] <- NA
    
    # Merge with crime categories 
    data <- merge(data, crimecodes, by.x="crm_cd", by.y="Crime.Code", all.x=T)
    data <- data[c("lat", "lon", "crm_cd", "crm_cd_desc", "Type")]
    
    data <- data %>% filter(!is.na(lon) & !is.na(lat))
    
    data
  })
  
  # Palette for points on map 
  pal <- colorFactor("Paired", unique(crimecodes$Type))
  
  # Map tab content - plot leaflet --------------------------------------
  output$PlotCrime <-renderLeaflet({
    
    if(input$format=='Point') {
      points <- getCrimeData()
      if(length(points)==0)
        return(NULL)
      
      # Show only selected crime types
      points <- filter(points, Type %in% as.character(input$crimetype))
      
      plot <- leaflet(points) %>%
        # Base layer 
        addProviderTiles("CartoDB.Positron") %>%
        setView(lat=34, lng=-118.3, zoom=10) %>%
        
        # Overlay groups
        addMarkers(data=attractions, lng=~longitude, lat=~latitude, 
                   icon=makeIcon(iconUrl="https://cdn-icons-png.flaticon.com/512/5836/5836608.png", 
                                 iconWidth=17, iconHeight=17), 
                   popup=~name, 
                   group="Attractions") %>%
        addMarkers(data=stations, lng=~longitude, lat=~latitude, 
                   icon=makeIcon(iconUrl="https://cdn-icons-png.flaticon.com/512/2862/2862389.png", 
                                 iconWidth=17, iconHeight=17), 
                   popup=~Name, 
                   group="Police Stations") %>%
        
        # Crime points 
        addCircleMarkers(lng=~lon, lat=~lat, popup=~htmlEscape(crm_cd_desc), 
                         color=~pal(Type), radius=5, fillOpacity=1,
                         stroke = FALSE, group="Crime") %>%
        addLegend("bottomright", pal=pal, values=~Type) %>% 
        
        # Layer Control
        addLayersControl(
          overlayGroups = c("Attractions", "Police Stations"),
          options = layersControlOptions(collapsed = F)
        ) %>% 
        hideGroup("Attractions") %>%
        hideGroup("Police Stations")
        
    }
    else if (input$format == 'Heatmap'){
      plot <- leaflet(getCrimeData()) %>%
        addTiles() %>%
        addWebGLHeatmap(lng=~lon, lat=~lat, size=3000)
    }
    else {print("Wrong format name")}
    
  })
  
  ### Get all crime data for crime type tab ------------------------------------------
  getAllData <- reactive({
    print("Loading data...")  
    url = "https://data.lacity.org/resource/2nrs-mtv8.json?$limit=6000000"
    data <- fromJSON(url)
    crimedata <- as.data.frame(data)
    
    print("Cleaning data...")
    # Clean data
    crimedata$date_occ <- substr(crimedata$date_occ,1,nchar(crimedata$date_occ)-13)
    crimedata$date_occ<-strptime(crimedata$date_occ,format="%Y-%m-%d")
    crimedata$date_occ<-as.Date(crimedata$date_occ,format="%Y-%m-%d")
    crimedata$month <- months(crimedata$date_occ)
    crimedata$year <- as.numeric(format(crimedata$date_occ,'%Y'))
    crimedata$weekday <- weekdays(crimedata$date_occ)
    crimedata$hour <- substr(crimedata$time_occ, 1, nchar(crimedata$time_occ)-2)
    crimedata$lat <- as.numeric(crimedata$lat)
    crimedata$lon <- as.numeric(crimedata$lon)
    
    crimedata
  })
  
  ### Crime Type tab functions -------------------------------------------------
  output$crimeday <- renderPlot({
    crimedata <- getAllData()
    weekdaycounts <- as.data.frame(table(crimedata$weekday))
    weekdaycounts$Var1 <- factor(weekdaycounts$Var1, ordered = T, 
                                 levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    
    ggplot(weekdaycounts, aes(x = Var1, y = Freq)) + 
      geom_line((aes(group = 1))) + 
      theme_minimal() + 
      xlab("Day of the week") + 
      ylab("Total Crime")
  })
  
  output$crimehour <- renderPlot({
    crimedata <- getAllData()
    hourcounts <- as.data.frame(table(crimedata$hour))
    
    ggplot(hourcounts, aes(x = Var1, y = Freq)) + 
      geom_line((aes(group = 1))) + xlab("Hour of the Day") + ylab("Total Crime")
  })
  
  output$crimehourday <- renderPlot({
    crimedata <- getAllData()
    DayHourCount <- as.data.frame(table(crimedata$weekday, crimedata$hour))
    DayHourCount$Hour <- as.numeric(as.character(DayHourCount$Var2))
    DayHourCount$Var1 <- factor(DayHourCount$Var1, ordered = T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday"))
    
    ggplot(DayHourCount, aes(x = Hour, y = Freq)) + 
      geom_line(aes(group = Var1, color = Var1), size = 2) + 
      theme_minimal() + 
      labs(colour = "Weekday")
  })
  
  output$crimehourdayheatmap <- renderPlot({
    crimedata <- getAllData()
    DayHourCount <- as.data.frame(table(crimedata$weekday, crimedata$hour))
    DayHourCount$Hour <- as.numeric(as.character(DayHourCount$Var2))
    DayHourCount$Var1 <- factor(DayHourCount$Var1, ordered = T,
                                levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday"))
    
    ggplot(DayHourCount, aes(x = Hour, y = Var1)) +
      geom_tile(aes(fill = Freq)) +
      scale_fill_gradient(name = "Total Crime", low = "white", high = "blue") +
      theme_minimal() + 
      theme(axis.title.y = element_blank())
  })
  
  output$crimemonth <- renderPlot({
    crimedata <- getAllData()
    monthcounts <- as.data.frame(table(crimedata$month))
    monthcounts$Var1 <- factor(monthcounts$Var1, levels=month.name)

    ggplot(monthcounts, aes(x = Var1, y = Freq)) + 
      geom_line((aes(group = 1))) + 
      theme_minimal() + 
      xlab("Month") + 
      ylab("Total Crime") 
  })
  
  output$crimeyear <- renderPlot({
    crimedata <- getAllData()
    yearcounts <- as.data.frame(table(crimedata$year))
    
    ggplot(yearcounts, aes(x = Var1, y = Freq)) + 
      geom_line((aes(group = 1))) + 
      theme_minimal() + 
      xlab("Year") + 
      ylab("Total Crime")
  })
  
  output$top20crimes <- renderPlot({
    crimedata <- getAllData()
    
    crimetop20 <- crimedata %>% 
      group_by(crm_cd_desc) %>%
      summarise(total = n()) %>%
      distinct() %>%
      top_n(20)
    
    crimetop20 %>%
      ggplot(aes(reorder(crm_cd_desc, total), y = total)) +
      geom_col(fill = "blue") +
      geom_label_repel(aes(label = total), size = 2, force = 1) +
      coord_flip() +
      theme_minimal() + 
      labs(title = "Top 20 Crime Commited", 
           x = "Crime Description", 
           y = "Total")
  })
  
  output$top20crimestreemap <- renderPlot({
    crimedata <- getAllData()
    
    crimetop20 <- crimedata %>% 
      group_by(crm_cd_desc) %>%
      summarise(total = n()) %>%
      distinct() %>%
      top_n(20)
    
    n <- 20
    palette <- distinctColorPalette(n)
    
    crimetreemap2 <- ggplot(data = crimetop20, 
                            aes(fill = crm_cd_desc, area = total, label = paste0(crm_cd_desc,"\n", prettyNum(total,big.mark=","))))
    
    crimetreemap2 + geom_treemap() +
      geom_treemap_text(grow = F, reflow = T, colour = "black", place = "centre", size = 5) + 
      labs(title ="Treemap of crime in LA") + 
      labs(subtitle ="Top 20 types of crimes") + 
      theme_minimal() + 
      theme(legend.position = "none") + 
      scale_fill_manual(values = palette)
  })
  
  output$crimesbyareatreemap <- renderD3tree({
    crimedata <- getAllData()
    
    crimedatatree <- crimedata %>% 
      group_by(area_name,crm_cd_desc) %>%
      summarise(total = n())
    
    tm <- treemap(crimedatatree,
                  index=c("area_name", "crm_cd_desc"),
                  vSize="total",
                  vColor="crm_cd_desc",
                  title="Crime Proportion")
    
    d3tree2(tm, rootname = "Los Angeles")
  })
  
  output$propcrimeyear <- renderPlot({
    crimedata <- getAllData()
    
    crimedataproportion <- crimedata %>% 
      group_by(crm_cd_desc) %>% 
      filter(n() > 10000)
    
    ggplot(crimedataproportion) + 
      geom_bar(aes(x= year, fill = crm_cd_desc), colour = "black", position = "fill") + 
      theme_minimal() + 
      ggtitle("Proportion of Crimes") +
      ylab("Number of Crimes")
  })
  
  ### Get full crime data ---------------------------------------------------
  getFullCrimeData <- reactive({
    print("Loading data...")  
    url = "https://data.lacity.org/resource/2nrs-mtv8.json?$limit=600000"
    data <- fromJSON(url)
    data <- as.data.frame(data)
    
    print("Cleaning data...")
    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)
    data$lat[data$lat==0] <- NA
    data$lon[data$lon==0] <- NA
    
    data <- merge(data, crimecodes, by.x="crm_cd", by.y="Crime.Code", all.x=T)
    
    data$vict_age <- as.integer(data$vict_age)
    data <- data %>% filter(!is.na(lon) & !is.na(lat))
    
    # Cleaning vict_descent column
    # Replace Descent Code with description 
    data <- data %>%
      mutate(vict_descent = recode(vict_descent, 
                                   "A" = "Other Asian", 
                                   "B" = "Black",
                                   "C" = "Chinese",
                                   "D" = "Cambodian",
                                   "F" = "Filipino",
                                   "G" = "Guamanian",
                                   "H" = "Hispanic/Latin/Mexican",
                                   "I" = "American Indian/Alaskan Native",
                                   "J" = "Japanese",
                                   "K" = "Korean",
                                   "L" = "Laotian",
                                   "O" = "Other",
                                   "P" = "Pacific Islander",
                                   "S" = "Samoan",
                                   "U" = "Hawaiian",
                                   "V" = "Vietnamese",
                                   "W" = "White",
                                   "X" = "Unknown",
                                   "Z" = "Asian Indian"
      )) %>%
      replace(is.na(.), "Unknown")
    
    # Clean vict_sex
    data[is.na(data$vict_sex) | data$vict_sex == "X", "vict_sex"] <- "Unknown"
    
    # Clean vict_age
    data[data$vict_age == 0 & 
           data$vict_descent == "Unknown" & 
           data$vict_sex == "Unknown", "vict_age"] <- "Unknown"
    
    data <- data %>% filter(vict_age != "Unknown") %>%
      mutate(age_group = cut(as.integer(vict_age), 
                             breaks=c(0, 17, 29, 59, 79, Inf), 
                             labels=c("0-17", "18-29", "30-59", "60-79", ">80"), right=T))
    full.data <- data
    full.data
  })
  
  ### Victim tab functions -----------------------------------------------------
  output$vict_analy <- renderPlot({
    ggplot(getFullCrimeData()[getFullCrimeData()$vict_descent == input$Race &
                                getFullCrimeData()$vict_sex == input$Sex &
                                getFullCrimeData()$age_group == input$Age, ], 
           aes(x=Type, fill=Type)) + 
      geom_bar(stat="count") + 
      theme_minimal() + 
      theme(axis.text.x = element_text())
  })
  
  ### Get weapon data ---------------------------------------------------------
  getWeaponData <- reactive({
    date <- as.character(input$date2)
    
    url = "https://data.lacity.org/resource/2nrs-mtv8.json?date_occ="
    url <- paste0(url, date, "T00:00:00.000")
    data <- fromJSON(url)
    data <- as.data.frame(data)
    
    data[is.na(data$weapon_desc), "weapon_desc"] <- "UNKNOWN WEAPON/OTHER WEAPON" 
    data[data$weapon_desc %in% c("HAND GUN", "UNKNOWN FIREARM", "OTHER FIREARM",
                                 "HECKLER & KOCH 93 SEMIAUTOMATIC ASSAULT RIFLE",
                                 "AIR PISTOL/REVOLVER/RIFLE/BB GUN",
                                 "SEMI-AUTOMATIC PISTOL", "SHOTGUN", "REVOLVER"), "weapon_desc"] <- "FIREARM" 
    data[data$weapon_desc %in% c("KITCHEN KNIFE", "OTHER KNIFE", "FOLDING KNIFE",
                                 "KNIFE WITH BLADE OVER 6 INCHES IN LENGTH",
                                 "KNIFE WITH BLADE 6INCHES OR LESS"), "weapon_desc"] <- "KNIFE" 
    data <- merge(data, crimecodes, by.x="crm_cd", by.y="Crime.Code", all.x=T)
    
    data[!is.na(data$Type),]
  })
  
  ### Fun fact tab functions ---------------------------------------------------
  output$weapon <- renderPlotly({
    gplt <- ggplot(getWeaponData()) +
      geom_bar(aes(x=Type, fill=weapon_desc), position='fill') + 
      coord_flip() +
      theme_minimal() + 
      xlab("") +
      ylab("Proportion") +
      ggtitle("Weapons Used by Crime Type")
    ggplotly(gplt)
    
  })
}

shinyApp(ui=ui, server=server)