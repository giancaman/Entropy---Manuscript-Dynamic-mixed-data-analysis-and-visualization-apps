
library(shiny)
library(leaflet)
library(tidyverse)
library (data.table)
library (rgdal)
library (geojsonio)
library(ggplot2)
library(broom)
library(eurostat)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(plotly)
countries = c("Italy",
              "Spain",
              "Austria",
              "Belgium",
              "Bulgaria",
              "Czech Republic",
              "Germany",
              "Denmark",
              "Estonia",
              "Finland",
              "France",
              "Greece",
              "Croatia",
              "Hungary",
              "Ireland",
              "Lithuania",
              "Latvia",
              "Malta",
              "Netherlands",
              "Poland",
              "Portugal",
              "Romania",
              "Slovakia",
              "Sweden",
              "Slovenia")
# Choosing the folder where the dataset "dati1.csv" is stored:
savePath <- easycsv::choose_dir()
setwd(savePath)
#Number of weeks:
len <- 59
# Using fread function from data.table package which is enormously faster than read_csv and read.csv:
pcmRegData <- fread(paste(savePath, "dati2", ".csv", sep =""), showProgress = FALSE, nrows = 25)
pcmRegDataMatrix <- as.matrix(pcmRegData)
#Loop for populating the list. Notice the use of the j variable

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("COVID-19 country comparison - Part III"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          
          helpText(HTML("EU country comparison on 
               COVID-19 performance based on distances from 1/2/2021 (week 1) to 14/3/2022 (week 59).
               Complete data for Luxembourg and Cyprus are not
               available. <b><i>Plot of the first two MDS PCs</i></b>. 
               <p style='color:red;'>Point sizes in the scatterplot are on a scale from 1 to 5</p>")),
        
          sliderInput("Weeks", "Week:",
                      min = 1, max = 59, value = 1, 
                      animate = animationOptions(interval = 500, loop = FALSE)
          ),
          radioButtons(inputId = "SelectPoints",
                       label = "Select variable for point size",
                       choices = c("Case", "Vaccine"),
                       selected = "Case",
                       inline = TRUE),
          
          actionButton("myBtn", "Close the app")
        
          ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plotDistances")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    output$plotDistances <- renderPlotly({
      #browser()
      select_week = req(input$Weeks)
      #filter data depending on selected date
      #plotting only the first two components:
      #Correction of 13/8/2022: added columns for cumulated cases and cumulated vaccines...
      base = ((select_week-1)*17) +1
      filteredData = as.data.frame(pcmRegDataMatrix[, c(base , base + 1, base + 15, base + 16)])
      Data_for_MDS = as.data.frame(cbind(countries, filteredData))
      colnames(Data_for_MDS) = c("Country", "PC1", "PC2", "Cases", "Vaccines")
      Data_for_MDS$Cases = (Data_for_MDS$Cases - min(Data_for_MDS$Cases))/(max(Data_for_MDS$Cases)- min(Data_for_MDS$Cases))
      Data_for_MDS$Vaccines = (Data_for_MDS$Vaccines - min(Data_for_MDS$Vaccines))/(max(Data_for_MDS$Vaccines)- min(Data_for_MDS$Vaccines))
      #browser()
      #ggplot is wrapped in a plotly plot to allow for saving the plot, zooming it, etc.
      if(input$SelectPoints == "Case"){
        ggplotly(ggplot(Data_for_MDS, aes_string("PC1", "PC2", key = "Country")) +
          geom_point(color = "blue", aes(size = 5 * Cases)) +
          xlim(-0.3,0.3) +
          ylim(-0.3,0.3) +
          xlab("PC1") +
          ylab("PC2"), source = "select", tooltip = c("key"))
      }
      else if (input$SelectPoints == "Vaccine"){
        ggplotly(ggplot(Data_for_MDS, aes_string("PC1", "PC2", key = "Country")) +
                   geom_point(color = "blue", aes(size = 5 * Vaccines)) +
                   xlim(-0.3,0.3) +
                   ylim(-0.3,0.3) +
                   xlab("PC1") +
                   ylab("PC2"), source = "select", tooltip = c("key"))
        }
    })
    #observe({
      if(input$myBtn > 0){
        stopApp(7)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


