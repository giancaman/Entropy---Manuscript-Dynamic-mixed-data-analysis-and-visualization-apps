#Installing packages if not already installed:
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("data.table")) install.packages("data.table")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("sf")) install.packages("sf")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

#Loading libraries
library(shiny)
library(leaflet)
library (data.table)
library (geojsonio)
library(sf)
library(htmlwidgets)

# Choosing the folder where the dataset "dati1.csv" is stored:
wd <- easycsv::choose_dir()
setwd(wd)
# "Coordinates.csv" contains the latitudes and longitudes of countries' capitals
df <- read.csv("Coordinates.csv", sep = ";", header = T)
countries = df$Country
longitudes = df$Longitude
latitudes = df$Latitude

downAndSaveDataPC <- function(pcmRegLink){
  #Number of weeks:
  len <- 59
  # Using fread function from data.table package which is enormously faster than read_csv and read.csv:
  pcmRegData <- fread(paste(pcmRegLink,"dati1.csv", sep =""), showProgress = FALSE, nrows = 25)
  # Prepare an empty 59-dimensional list:
  period_list <- vector(mode = "list", length = len)
  # This counter is used for partitioning the dataset (see the for loop)
  j=0
  #Create a matrix from dataframe:
  pcmRegDataMatrix <- as.matrix(pcmRegData)
  #Loop for populating the list. Notice the use of the j variable
  for (i in 1:len){
    period_list[[i]] = pcmRegDataMatrix[,c((j+1):(j+25))]
    j=i*25
  }
  #Return the full list to be used in the server part of the app
  return(period_list)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("COVID-19 country comparison - Part II"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          
          helpText(HTML("EU country comparison on 
               COVID-19 performance based on distances from 1/2/2021 (week 1) to 14/3/2022 (week 59).
               Complete data for Luxembourg and Cyprus are not
               available. <b><i>Distances displayed on the map are computed with respect to the country chosen (in light yellow)</i></b>.")),
          
          selectInput("StateInput", 
                         label = "Choose one country to be compared to other countries",
                         choices = countries,
                         selected = "", multiple = FALSE),
          # Slider to allow for selection of week and start and stop map animation
          sliderInput("Weeks", "Week:",
                      min = 1, max = 59, value = 1, 
                      animate = animationOptions(interval = 500, loop = FALSE)
          ),
          # This button allows for downloading the current map displayed on main panel
          downloadButton(outputId = "savemap", "Download plot in html format"),
          # This button is to close the app
          actionButton("myBtn", "Close the app")
          ),
        # Show a leaflet plot of the generated distribution according to slider value
        mainPanel(
          leafletOutput("PlotDistances")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  Period_list = downAndSaveDataPC(wd)
  len <-length(Period_list)
  df0 <- data.frame(matrix(ncol = 26, nrow = 59))
    # A renderLeaflet function has been created to allow htmlwidget to capture the map to ve downloaded:
    output$PlotDistances <- renderLeaflet({
    map()
      })
      map <- reactive({  
      select_week = req(input$Weeks)
      select_countries <- req(input$StateInput)
      ind_countries <- which(countries %in% select_countries)
      data_week = Period_list[[select_week]]
      # This is the geo-json file used for the map
      geo_europe1 = geojson_read("geo_europe.geojson", what = "sp")
      # Subsetting the 25 countries object of the analysis
      geo_europe = geo_europe1[geo_europe1$NAME %in% c("Italy", "Spain", "Austria", "Belgium", "Bulgaria",
                                                       "Czech Republic","Germany", "Denmark", "Estonia", 
                                                       "Finland", "France", "Greece","Croatia", 
                                                       "Hungary", "Ireland", "Lithuania", "Latvia", "Malta", 
                                                       "Netherlands", "Poland", "Portugal", "Romania",
                                                       "Slovakia", "Sweden","Slovenia"), ]
        # Week selected by the current value in slider
        data_week = Period_list[[select_week]]
        # Picking up the right country on the distance dataset
        country_distance = as.character(round(data_week[ind_countries,],2))
        df$Value = country_distance
        # Merging geographical dataset with dataset with distances:
        db_geo <- sp::merge(geo_europe,df,by.x ="NAME",by.y="Country")
        # Setting the leaflet:
        paletteBins <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
        pal<- colorBin(palette = "YlOrBr", na.color = "transparent", bins = paletteBins)
        labels <- sprintf(
          "<strong>%s</strong><br/> Distance: %s ",
          db_geo$NAME, db_geo$Value
        ) %>% lapply(htmltools::HTML)
        # Producing the leaflet:
        m <- leaflet(db_geo) %>% 
          setView(lng = 9.54, lat = 49.5, zoom = 4) %>%
          addTiles()  %>% 
          addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                      fillColor = ~pal(as.numeric(Value)),
                      label = labels) %>%
          addLegend(pal = pal, values = ~as.numeric(Value), opacity = 0.7, title = "Distance", position
                    = "topright")
        #saveWidget(m,file = "currentmap.html")
      #}
    })
    observe({
      if(input$myBtn > 0){
        stopApp(7)
      }
    })
    # Function to download the current displayed map:
    output$savemap <- downloadHandler(
      filename = "currentmap.html",
      content = function(file){
        saveWidget(map(), file=file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


