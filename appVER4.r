#Options
options(warn=-1)

#Installing packages if not already installed:
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("plotly")) install.packages("plotly")

# Load packages ----
library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

# List of countries as they are in the dataset
# The dataset is a 25 X (59*25) dimensional matrix. Each (25X25) submatrix starting from first column is a weekly distance matrix
# among 25 countries (No data for Luxembourg an Cyprus)
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
wd <- easycsv::choose_dir()
setwd(wd)

# Function to read the data
# Data are saved in a 59-member list. Each list element contains the distance matrix of one week
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

# Set up of the objects to be used in the app:
ui <- fluidPage(
  titlePanel("COVID-19 country comparison - Part I"),
  sidebarLayout(
    sidebarPanel(
      textOutput("text_out"),
      #This is to raise the no. of chosen countries error (must be more than 1):
      tags$head(tags$style("#text_out{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")),
      helpText(HTML("EU country comparison on 
               COVID-19 performance based on distances from 1/2/2021 (week 1) to 14/3/2022 (week 59).
               Complete data for Luxembourg and Cyprus are not
               available. <b><i>Distances on time series are referred to the first country chosen</i></b>.")),
      selectizeInput("StateInput", 
                  label = "Choose two countries for comparison",
                  choices = countries,
                  selected = c("Italy", "Spain"), multiple = TRUE, options = list(maxItems = 25)),
      actionButton("myBtn", "Close the app"),
      ),
    mainPanel(
      plotlyOutput("plotDistances")
      )
  )
)

# Server part
# The current version allow user to choose two countries and plot a time series of distances between this two countries.
# In a next version multiple time series comparison (choosing a reference country) will be implemented 
server <- function(input, output, session) {
  # Load data ----
  Period_list = downAndSaveDataPC(wd)
  len <-length(Period_list)
  df <- data.frame(matrix(ncol = 26, nrow = 59))
  # This part is for the selection of countries
  output$plotDistances <- renderPlotly({
    # This is the part regarding countries' choice:
    select_countries <- req(input$StateInput)
    n_countries_sel = length(select_countries)
    colnames(df) = c("Week", countries)
   
    # Selection of corresponding country indexes according to the user's choice:
    ind_countries <- which(countries %in% select_countries)
    
    # For() loop for filling the df data frame according to the chosen countries
    # Indexes follow the list of countries loaded in this app's preamble
    for(j in ind_countries[-1]){
      for (i in 1:len){
        df[i,j+1]<-Period_list[[i]][ind_countries[1],j]
        df[i,1] <- i
      }
      i = 1
    }  
    
    #Simple time series ggplot
   
    #The following code is for dropping empty columns in df:
    empty_columns <- sapply(df, function(x) all(is.na(x) | x == ""))
    my_df = df[, !empty_columns]
    #This code creates a date column with the initial day of each week:
    y2021 = as.Date(paste(2021,df$Week[5:52],1,sep="-"),"%Y-%U-%u")
    y2022 = as.Date(paste(2022,c(1:11),1,sep="-"),"%Y-%U-%u")
    my_df[1]=c(y2021,y2022)
    # This is taken from the reshape2 package to let ggplot prosude multiple time series:
    df1 =melt(my_df, 'Week')
    # melt() produce a variable called "variable". We replace it with "Country" as this will be the name of the legend:
    colnames(df1)[2] <- "Country"
    # Plotting the data frame df1 with the selected country (the first selected country is the reference country
    # against which all the other selected countries' distances are taken:
    ggplotly(ggplot(df1, aes(x=Week, y=value, group=Country,color=Country)) +
      geom_line() +
      theme_bw() +
      xlab("Week") +
      ylab("Distance") +
      ggtitle("Covid-19 distances through 59 weeks (from FEB 2021 TO MAR 2022)")+
      theme(plot.title = element_text(size = 8, face = "bold")))
  })
  output$text_out <- renderText({
    # Test on number of countries chosen in the dropdown list of countries:
    if (length(req(input$StateInput)) < 2 ){
      text_out = "Error: choose at least two countries"
    }
  })
  observe({
  if(input$myBtn > 0){
    stopApp(7)
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
