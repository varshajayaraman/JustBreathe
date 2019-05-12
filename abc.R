install.packages



setwd("E:/Varsha/Sem 2/Data Visualisation/AQIData")
aqiData <- read.table(file = "annual_aqi_by_county_2018.csv", sep = ",", header = TRUE)
aqiCook = subset(aqiData, State == "Illinois" & County == "Cook")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(leaflet)
library(ggrepel)
library(shinycssloaders)
library(bsplus)
library(htmltools)
#library(shinysky)

options(spinner.color.background="yellow")
pieChartData <- data.frame("days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"count" = c(aqiCook$Good.Days,aqiCook$Moderate.Days,aqiCook$Unhealthy.for.Sensitive.Groups.Days,aqiCook$Unhealthy.Days,aqiCook$Very.Unhealthy.Days,aqiCook$Hazardous.Days))
pieChart2Data <- data.frame("Pollutants" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"count" = c(aqiCook$Days.CO,aqiCook$Days.NO2,aqiCook$Days.Ozone,aqiCook$Days.SO2,aqiCook$Days.PM2.5,aqiCook$Days.PM10))

allData <- read.table(file="annual_aqi_by_county_1980.csv", sep = ",", header = TRUE)

for(i in 1981:2018){
  data1 <- read.table(file=paste0(paste0("annual_aqi_by_county_",i),".csv"), sep = ",", header = TRUE)
  allData <- rbind(allData, data1)
}




state <- (unique(aqiData$State))
#county <- (unique(aqiData$County))
#listNamesGood <- listNames[listNames != "Hour" & listNames != "newDate"]
year<-c(1980:2018)
year<-c(year)

mycss <- "
#Year ~ .selectize-control .selectize-input {
  max-height: 10px;
  overflow-y: auto;
}
"

ui <- dashboardPage(
    dashboardHeader(title = span("Basic ", 
                             style = "color: red; font-size: 4px"),
      tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 20px}"),
            tags$style(".main-header .logo {height: 20px;}"),
            tags$style(".sidebar-toggle {height: 20px; padding-top: 5px !important;}"),
            tags$style(".navbar {min-height:10px !important}")
      ) 
    ),
    dashboardSidebar(
      tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
    sidebarMenu(
      menuItem("COUNTY-WISE ANALYSIS",
     # tags$style(type='text/css', ".selectize-input { padding: 2px; min-height: 0; max-height: 20px; margin-botton: 5px; margin-top: 2px;} .selectize-dropdown { line-height: 9px; max-height: 20px;} .selectize-control{margin-bottom: 0px; max-height: 20px;} "),
        selectInput("Year", "Select the year to visualize", year, selected = 2016),
        selectInput("State0", "Select the state to visualize", state, selected = "Illinois"),
        sidebarMenuOutput("county")
        ),
        
      menuItem("COUNTY COMPARISON",
      sliderInput("Year1",
                  "Year 1:",
                  value = 1,
                  min = 1980,
                  max = 2018),
        selectInput("State1", "Select the state1 to visualize", state, selected = "Illinois"),
        sidebarMenuOutput("county1"),
        sliderInput("Year2",
                  "Year 2:",
                  value = 1,
                  min = 1980,
                  max = 2018),
        selectInput("State2", "Select the state2 to visualize", state, selected = "Illinois"),
        sidebarMenuOutput("county2"),
        sliderInput("Year3",
                  "Year 3:",
                  value = 1,
                  min = 1980,
                  max = 2018),
        selectInput("State3", "Select the state3 to visualize", state, selected = "Illinois"),
        sidebarMenuOutput("county3")
        )


      #h2("The colours in the plots are plotted in accordance with the official AQI site.")
        
    )  
    ),
    dashboardBody(
    tabsetPanel(
      # First tab content
        tabPanel("Single county",
          fluidRow( 
            column(4,
              #box(title = "Plots based on days", solidHeader = FALSE, status = "primary", width = 12,
                tabsetPanel(type = "tabs",
                  tabPanel("Days Bar Graph", plotOutput("daysBar", height = 350))
                  #tabPanel("Days Line Plot", plotOutput("line1", height = 305))               
                  #tabPanel("Pollutants", dataTableOutput("table2", height = 300))
                )
            ),

            column(4,
              #box(title = "Plots based on days", solidHeader = FALSE, status = "primary", width = 12,
                tabsetPanel(type = "tabs",
                  tabPanel("Days Tabulation", dataTableOutput("table", height = 350)),
                  tabPanel("Pollutants Tabulation", dataTableOutput("table2", height = 350))                
                  #tabPanel("Pollutants", dataTableOutput("table2", height = 300))
                )
            ),

            column(4,
              #box(title = "Plots based on days", solidHeader = FALSE, status = "primary", width = 12,
                tabsetPanel(type = "tabs",
                  tabPanel("Days Pie Chart", plotOutput("daysPie", height = 350))               
                  #tabPanel("Pollutants", dataTableOutput("table2", height = 300))
                )
            )
          ),

          br(),
          fluidRow(
            column(4,
                tabsetPanel(type = "tabs",
                  tabPanel("Pollutants Bar Graph", plotOutput("daysBar2", height = 305))
                  #tabPanel("Pollutants Line Plot", plotOutput("line2", height = 305))                
                  #tabPanel("Pollutants", dataTableOutput("table2", height = 300))
                )
            ),

            column(4,
              box(title = "Localisation of the selected State & County", solidHeader = FALSE, status = "primary", width = 15,
                    withSpinner(leafletOutput("map1", height = 305))
              )
            ),

            column(4,
              #box(title = "Plots based on days", solidHeader = FALSE, status = "primary", width = 12,
                tabsetPanel(type = "tabs",
                  tabPanel("Pollutants Pie Chart", plotOutput("daysPie2", height = 350))               
                  #tabPanel("Pollutants", dataTableOutput("table2", height = 300))
                )
            )
          ),

          fluidRow(
            h2("Line Graphs Indicating the trends in Days and Pollutants"),
            column(6,
              box(title = "Line graph Based on Days", solidHeader = FALSE, status = "primary", width = 15,
                plotOutput("line1", height = 400)
              )
            ),

            column(6,
              box(title = "Line graph Based on Pollutants", solidHeader = FALSE, status = "primary", width = 15,
                plotOutput("line2", height = 400)
              )
            )
          )

          
        ),

      # Second tab content
        tabPanel("Comparison",

          fluidRow( 
            column(6,
              #box(title = "Plots based on days", solidHeader = FALSE, status = "primary", width = 12,
                tabsetPanel(type = "tabs",
                  tabPanel("Days Bar Graph Comparison", plotOutput("daysBarCombined", height = 305))
                )
            ),

            column(6,
              tabsetPanel(type = "tabs",
                  tabPanel("Pollutants Bar Graph Comparison", plotOutput("daysBarCombinedPollutants", height = 305))
              )
            )
          ),

          fluidRow(
            column(4,
              tabsetPanel(type = "tabs",
                  tabPanel("Max AQI Line Graph Comparison", plotOutput("daysLineCombinedMaxAQI", height = 305))
              )
            ),

            column(4,
              tabsetPanel(type = "tabs",
                  tabPanel("X90th AQI Line Graph Comparison", plotOutput("daysLineCombinedX90thPercentileAQI", height = 305))
              )
            ),

            column(4,
              tabsetPanel(type = "tabs",
                  tabPanel("Median AQI Line Graph Comparison", plotOutput("daysLineCombinedMedianAQI", height = 305))
              )
              )
          ),

          fluidRow(
            h2("Pollutant-wise comparison"),
            column(4,
                tabsetPanel(type = "tabs",            
                  tabPanel("Pollutants-CO Line Graph", plotOutput("daysLineCombinedCO", height = 305))
                )
            ),

            column(4,
              tabsetPanel(type = "tabs",            
                  tabPanel("Pollutants-NO2 Line Graph", plotOutput("daysLineCombinedNO2", height = 305))
              )
            ),

            column(4,
                tabsetPanel(type = "tabs",            
                  tabPanel("Pollutants-Ozone Line Graph", plotOutput("daysLineCombinedOzone", height = 305))
                )
            )
          ),

          fluidRow(
            column(4,
                tabsetPanel(type = "tabs",            
                  tabPanel("Pollutants-SO2 Line Graph", plotOutput("daysLineCombinedSO2", height = 305))
                )
            ),

            column(4,
              tabsetPanel(type = "tabs",
                  tabPanel("Pollutants-PM2.5 Line Graph", plotOutput("daysLineCombinedPM2.5", height = 305))
              )
            ),

            column(4,
                tabsetPanel(type = "tabs",            
                  tabPanel("Pollutants-PM10 Line Graph", plotOutput("daysLineCombinedPM10", height = 305))
                )
            )
          )
        ),

        tabPanel("About App",
          includeHTML("aboutApp.html")
          )
    )
  )
)

server <- function(input, output, session) {


    output$daysPie <- renderPlot({
      
    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    pieChartData <- data.frame("days" = c("Good","Moderate","Unhealthy for sensitive groups","Unhealthy","Very Unhealthy","Hazardous"), #"count" = c((aqiCounty$Good.Days),(aqiCounty$Moderate.Days),(aqiCounty$Unhealthy.for.Sensitive.Groups.Days),(aqiCounty$Unhealthy.Days),(aqiCounty$Very.Unhealthy.Days),(aqiCounty$Hazardous.Days)))

      "count" = c((aqiCounty$Good.Days/aqiCounty$Days.with.AQI)*100,(aqiCounty$Moderate.Days/aqiCounty$Days.with.AQI)*100,(aqiCounty$Unhealthy.for.Sensitive.Groups.Days/aqiCounty$Days.with.AQI)*100,(aqiCounty$Unhealthy.Days/aqiCounty$Days.with.AQI)*100,(aqiCounty$Very.Unhealthy.Days/aqiCounty$Days.with.AQI)*100,(aqiCounty$Hazardous.Days/aqiCounty$Days.with.AQI)*100))
    pieChartData<- subset(pieChartData, round(count) != 0)
    bp <- ggplot(pieChartData, aes(x="Days", y=count, fill=days))+geom_bar(width = 1, stat = "identity")
    blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    pie <- bp + coord_polar("y", start=0)
    pie + scale_fill_manual(values = c("Good"="green", "Moderate"="yellow", "Unhealthy for sensitive groups"="orange", "Unhealthy"="red", "Very Unhealthy"="purple", "Hazardous"="maroon"))+blank_theme+theme(axis.text.x=element_blank()) +  geom_text(hjust=0.75, aes(x=1.7, nudge_x=0.2, label=paste0(round(count), "%")), position = position_stack(vjust=0.75)) 
    
  })


  output$plot <- renderPlot({
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())

    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
  #  plot(cars)
  })
# increase the default font size
  theme_set(theme_grey(base_size = 18)) 

  fileName <- reactive(paste0(paste0("annual_aqi_by_county_", input$Year), ".csv"))
  fileName1 <- reactive(paste0(paste0("annual_aqi_by_county_", input$Year1), ".csv"))
  fileName2 <- reactive(paste0(paste0("annual_aqi_by_county_", input$Year2), ".csv"))
  fileName3 <- reactive(paste0(paste0("annual_aqi_by_county_", input$Year3), ".csv"))
  enteredCaption <- reactive(input$Caption)

  #COMPARISON OF MAX-AQI DATA FOR 3 DIFFERENT COUNTIES
  output$daysLineCombinedMaxAQI <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1& State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=Max.AQI, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County))
  })

  output$daysLineCombinedMedianAQI <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1& State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=Median.AQI, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County))
  })

  output$daysLineCombinedX90thPercentileAQI <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1& State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=X90th.Percentile.AQI, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County))
  })

  output$daysLineCombinedCO <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.CO/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" CO Percentage")
  })

  output$daysLineCombinedNO2 <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.NO2/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" NO2 Percentage")
  })

  output$daysLineCombinedOzone <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.Ozone/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" Ozone Percentage")
  })

  output$daysLineCombinedSO2 <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.SO2/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" SO2 Percentage")
  })

  output$daysLineCombinedPM2.5 <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.PM2.5/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" PM2.5 Percentage")
  })

  output$daysLineCombinedPM10 <- renderPlot({

    lineGraph1 <- subset(allData, County==input$County1 & State == input$State1)
    lineGraph1 <- rbind(subset(allData, County == input$County2 & State == input$State2), lineGraph1)
    lineGraph1 <- rbind(subset(allData, County == input$County3 & State == input$State3), lineGraph1)
  
    ggplot(data=lineGraph1, aes(x=Year, y=(Days.PM10/Days.with.AQI)*100, group=interaction(County, State))) + geom_line(aes(linetype=County, color=County))+ geom_point(aes(shape=County, color=County)) + ylab(label=" PM10 Percentage")
  })
#  output$Test <- renderText({ 
#      paste("x")
    
#  })
  
  #DYNAMIC COUNTY DROPDOWN DEPENDING ON USER-SELECTED STATE
  output$county1 <- renderMenu({
    if(input$Year1 == "generic"){
      aqiData <- allData
    } else{
      aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    }
    aqiState <- subset(aqiData, State == input$State1)
    countyData <- unique(aqiState$County)
    selectInput("County1", "Select the county1 to visualize", countyData)
    
  })

  output$county2 <- renderMenu({
    if(input$Year1 == "generic"){
      aqiData <- allData
    } else{
      aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    }
    aqiState <- subset(aqiData, State == input$State2)
    countyData <- unique(aqiState$County)
    selectInput("County2", "Select the county2 to visualize", countyData)
    
  })

  output$county3 <- renderMenu({
    if(input$Year1 == "generic"){
      aqiData <- allData
    } else{
      aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    }
    aqiState <- subset(aqiData, State == input$State3)
    countyData <- unique(aqiState$County)
    selectInput("County3", "Select the county3 to visualize", countyData)
    
  })
  output$county <- renderMenu({
    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State0)
    countyData <- unique(aqiState$County)
    print(paste0("hg ",input$State))
    selectInput("County", "Select the county to visualize", countyData)
  })



  
    
    
    #Sys.sleep(100)
    


  #PIE-CHART VISUALISATION FOR DIFFERENT KIND OF DAYS FOR AN YEAR
  

  #PIE-CHART VISUALISATION FOR DIFFERENT KIND OF POLLUTANTS FOR AN YEAR
  output$daysPie2 <- renderPlot({

    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    #aqiCounty
    pieChart2Data <- data.frame("Pollutants" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"count" = c((aqiCook$Days.CO/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.NO2/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.Ozone/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.SO2/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.PM2.5/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.PM10/aqiCounty$Days.with.AQI)*100))

    pieChart2Data<- subset(pieChart2Data, round(count) > 0)
    bp <- ggplot(pieChart2Data, aes(x="Pollutants", y=count, fill=Pollutants))+geom_bar(width = 1, stat = "identity")
    blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    pie <- bp + coord_polar("y", start=0)
    pie + blank_theme+theme(axis.text.x=element_blank()) +  geom_text(aes(x=1.7, label=paste0(round(count), "%")), position = position_stack(vjust=0.65)) 

  })

  #BAR-CHART REPRESENTATION FOR DIFFERENT KIND OF DAYS FOR COMPARING GIVEN YEARS AND COUNTIES
  output$daysBarCombined <- renderPlot({

    aqiData1 <- read.table(file=fileName1(), sep = ",", header = TRUE)
    aqiData2 <- read.table(file=fileName2(), sep = ",", header = TRUE)
    aqiData3 <- read.table(file=fileName3(), sep = ",", header = TRUE)

    aqiState1 <- subset(aqiData1, State == input$State1)
    aqiState2 <- rbind(subset(aqiData2, State == input$State2), aqiState1)
    aqiState3 <- rbind(subset(aqiData3, State == input$State3), aqiState2)
    aqiCounty1 <- subset(aqiState3, County == input$County1)
    aqiCounty2 <- subset(aqiState3, County == input$County2)
    aqiCounty <- rbind(aqiCounty2, aqiCounty1)
    aqiCounty3 <- subset(aqiState3, County == input$County3)
    aqiCounty <- rbind(aqiCounty3, aqiCounty)
    value <- c(aqiCounty$Good.Days[1], aqiCounty$Good.Days[2], aqiCounty$Good.Days[3], aqiCounty$Moderate.Days[1], aqiCounty$Moderate.Days[2], aqiCounty$Moderate.Days[3], aqiCounty$Unhealthy.for.Sensitive.Groups.Days[1], aqiCounty$Unhealthy.for.Sensitive.Groups.Days[2], aqiCounty$Unhealthy.for.Sensitive.Groups.Days[3], aqiCounty$Unhealthy.Days[1], aqiCounty$Unhealthy.Days[2], aqiCounty$Unhealthy.Days[3], aqiCounty$Very.Unhealthy.Days[1], aqiCounty$Very.Unhealthy.Days[2], aqiCounty$Very.Unhealthy.Days[3], aqiCounty$Hazardous.Days[1], aqiCounty$Hazardous.Days[2], aqiCounty$Hazardous.Days[3])
    
    #aqiCounty
    pieChartData <- data.frame("county" = c(paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3)), "days" = c("Good", "Good", "Good", "Moderate", "Moderate", "Moderate","Unhealthy for sensitive", "Unhealthy for sensitive", "Unhealthy for sensitive", "Unhealthy", "Unhealthy", "Unhealthy", "Very Unhealthy", "Very Unhealthy", "Very Unhealthy", "Hazardous", "Hazardous", "Hazardous"), "count" = value )
    
    ggplot(pieChartData, aes(x=days, y=count, fill = county)) + geom_bar(stat="identity", position="dodge", besides=TRUE)+
      theme(axis.text.x = element_text(size = 8))
  })

  #BAR-CHART REPRESENTATION FOR DIFFERENT KIND OF POLLUTANTS FOR COMPARING GIVEN YEARS AND COUNTIES 
  output$daysBarCombinedPollutants <- renderPlot({

    aqiData1 <- read.table(file=fileName1(), sep = ",", header = TRUE)
    aqiData2 <- read.table(file=fileName2(), sep = ",", header = TRUE)
    aqiData3 <- read.table(file=fileName3(), sep = ",", header = TRUE)

    aqiState1 <- subset(aqiData1, State == input$State1)
    aqiState2 <- rbind(subset(aqiData2, State == input$State2), aqiState1)
    aqiState3 <- rbind(subset(aqiData3, State == input$State3), aqiState2)
    aqiCounty1 <- subset(aqiState3, County == input$County1)
    aqiCounty2 <- subset(aqiState3, County == input$County2)
    aqiCounty <- rbind(aqiCounty2, aqiCounty1)
    aqiCounty3 <- subset(aqiState3, County == input$County3)
    aqiCounty <- rbind(aqiCounty3, aqiCounty)
    value <- c((aqiCounty$Days.CO[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.CO[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.CO[3]/aqiCounty$Days.with.AQI[3])*100, (aqiCounty$Days.NO2[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.NO2[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.NO2[3]/aqiCounty$Days.with.AQI[3])*100, (aqiCounty$Days.Ozone[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.Ozone[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.Ozone[3]/aqiCounty$Days.with.AQI[3])*100, (aqiCounty$Days.SO2[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.SO2[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.SO2[3]/aqiCounty$Days.with.AQI[3])*100, (aqiCounty$Days.PM2.5[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.PM2.5[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.PM2.5[3]/aqiCounty$Days.with.AQI[3])*100, (aqiCounty$Days.PM10[1]/aqiCounty$Days.with.AQI[1])*100, (aqiCounty$Days.PM10[2]/aqiCounty$Days.with.AQI[2])*100, (aqiCounty$Days.PM10[3]/aqiCounty$Days.with.AQI[3])*100)
    
    pieChartData <- data.frame("county" = c(paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3), paste0(input$State1, ",",input$County1, "- ", input$Year1), paste0(input$State2, ",",input$County2, "- ", input$Year2), paste0(input$State3, ",",input$County3, "- ", input$Year3)), "type"=c("CO", "CO", "CO", "NO2", "NO2", "NO2", "Ozone", "Ozone", "Ozone", "SO2", "SO2", "SO2", "PM2.5", "PM2.5", "PM2.5", "PM10", "PM10", "PM10"), "count" = value )
    
    ggplot(pieChartData, aes(x=type, y=count, fill = county)) + geom_bar(stat="identity", position="dodge", besides=TRUE)+
      theme(axis.text.x = element_text(size = 8))
  })

  

 # observeEvent(input$goButton2, {
  output$daysBar <- renderPlot({

    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    
    pieChartData <- data.frame("days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"count" = c(aqiCounty$Good.Days,aqiCounty$Moderate.Days,aqiCounty$Unhealthy.for.Sensitive.Groups.Days,aqiCounty$Unhealthy.Days,aqiCounty$Very.Unhealthy.Days,aqiCounty$Hazardous.Days))
    
    ggplot(pieChartData, aes(x=days, y=count, fill=days))+geom_bar(width = 0.5, stat = "identity")+
      theme(axis.text.x = element_text(size = 8))  + scale_fill_manual(values = c("Good"="green", "Moderate"="yellow", "Unhealthy for sensitive groups"="orange", "Unhealthy"="red", "Very Unhealthy"="purple", "Hazardous"="maroon"))
 
  #})
  })

  output$daysBar2 <- renderPlot({

    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    
    pieChartData <- data.frame("Pollutants" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),"count" = c((aqiCook$Days.CO/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.NO2/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.Ozone/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.SO2/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.PM2.5/aqiCounty$Days.with.AQI)*100,(aqiCook$Days.PM10/aqiCounty$Days.with.AQI)*100))
    
    ggplot(pieChart2Data, aes(x=Pollutants, y=count, fill=Pollutants))+geom_bar(width = 0.5, stat = "identity")+
      theme(axis.text.x = element_text(size = 8))  
  })

  output$table <- DT::renderDataTable(DT::datatable({

    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    #aqiCounty
    pieChartData <- data.frame("days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"count" = c(aqiCounty$Good.Days,aqiCounty$Moderate.Days,aqiCounty$Unhealthy.for.Sensitive.Groups.Days,aqiCounty$Unhealthy.Days,aqiCounty$Very.Unhealthy.Days,aqiCounty$Hazardous.Days))
    
      pieChartData
    }, 
  options = list(searching = TRUE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE ))

  output$table2 <- DT::renderDataTable(DT::datatable({

    aqiData <- read.table(file=fileName(), sep = ",", header = TRUE)
    aqiState <- subset(aqiData, State == input$State)
    aqiCounty <- subset(aqiState, County == input$County)
    #aqiCounty
    pieChartData <- data.frame("days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"count" = c(aqiCounty$Good.Days,aqiCounty$Moderate.Days,aqiCounty$Unhealthy.for.Sensitive.Groups.Days,aqiCounty$Unhealthy.Days,aqiCounty$Very.Unhealthy.Days,aqiCounty$Hazardous.Days))
    
      pieChart2Data
    }, 
  options = list(searching = TRUE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE ))

  output$line1 <- renderPlot({

    lineGraph <- subset(allData, County==input$County & State == input$State)
    lineGraphData <- data.frame("type"=c("Median", "Max", "90%"), "year"=c(1980, 1980, 1980), "days"=c(lineGraph$Median.AQI[1], lineGraph$Max.AQI[1], lineGraph$X90th.Percentile.AQI[1]))

    j=2
    for(i in 1981:2018){
      
      newFrame <- data.frame("type"=c("Median", "Max", "90%"), "year"=c(i, i, i), "days"=c(lineGraph$Median.AQI[j], lineGraph$Max.AQI[j], lineGraph$X90th.Percentile.AQI[j]))
      lineGraphData <- rbind(lineGraphData, newFrame)
      j=j+1
    }
    
    ggplot(data=lineGraphData, aes(x=year, y=days, group=type)) + geom_line(aes(linetype=type, color=type))+ geom_point(aes(shape=type, color=type))
  })


  

  output$line2 <- renderPlot({

    lineGraph <- subset(allData, County==input$County & State == input$State)
    lineGraphData <- data.frame("type"=c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "year"=c(1980, 1980, 1980, 1980, 1980, 1980), "percentage"=c((lineGraph$Days.CO[1]/lineGraph$Days.with.AQI[1])*100, (lineGraph$Days.NO2[1]/lineGraph$Days.with.AQI[1])*100, (lineGraph$Days.Ozone[1]/lineGraph$Days.with.AQI[1])*100, (lineGraph$Days.SO2[1]/lineGraph$Days.with.AQI[1])*100, (lineGraph$Days.PM2.5[1]/lineGraph$Days.with.AQI[1])*100, (lineGraph$Days.PM10[1]/lineGraph$Days.with.AQI[1])*100))

    j=2
    for(i in 1981:2018){
      
      newFrame <- data.frame("type"=c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "year"=c(i, i, i, i, i, i), "percentage"=c((lineGraph$Days.CO[j]/lineGraph$Days.with.AQI[j])*100, (lineGraph$Days.NO2[j]/lineGraph$Days.with.AQI[j])*100, (lineGraph$Days.Ozone[j]/lineGraph$Days.with.AQI[j])*100, (lineGraph$Days.SO2[j]/lineGraph$Days.with.AQI[j])*100, (lineGraph$Days.PM2.5[j]/lineGraph$Days.with.AQI[j])*100, (lineGraph$Days.PM10[j]/lineGraph$Days.with.AQI[j])*100))
      lineGraphData <- rbind(lineGraphData, newFrame)
      j=j+1
    }
    
    ggplot(data=lineGraphData, aes(x=year, y=percentage, group=type)) + geom_line(aes(linetype=type, color=type))+ geom_point(aes(shape=type, color=type))
  })

  output$table3 <- DT::renderDataTable(DT::datatable({

    lineGraph <- subset(allData, County==input$County & State == input$State)
    lineGraphData <- data.frame("Year"=c(1980, 1980, 1980, 1980, 1980, 1980), "Pollutant"=c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Percentage"=c(round((lineGraph$Days.CO[1]/lineGraph$Days.with.AQI[1])*100, 2), round((lineGraph$Days.NO2[1]/lineGraph$Days.with.AQI[1])*100, 2), round((lineGraph$Days.Ozone[1]/lineGraph$Days.with.AQI[1])*100, 2), round((lineGraph$Days.SO2[1]/lineGraph$Days.with.AQI[1])*100, 2), round((lineGraph$Days.PM2.5[1]/lineGraph$Days.with.AQI[1])*100, 2), round((lineGraph$Days.PM10[1]/lineGraph$Days.with.AQI[1])*100, 2)))

    j=2
    for(i in 1981:2018){
      
      newFrame <- data.frame( "Year"=c(i, i, i, i, i, i), "Pollutant"=c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Percentage"=c(round((lineGraph$Days.CO[j]/lineGraph$Days.with.AQI[j])*100, 2), round((lineGraph$Days.NO2[j]/lineGraph$Days.with.AQI[j])*100, 2), round((lineGraph$Days.Ozone[j]/lineGraph$Days.with.AQI[j])*100, 2), round((lineGraph$Days.SO2[j]/lineGraph$Days.with.AQI[j])*100, 2), round((lineGraph$Days.PM2.5[j]/lineGraph$Days.with.AQI[j])*100, 2), round((lineGraph$Days.PM10[j]/lineGraph$Days.with.AQI[j])*100, 2)))
      lineGraphData <- rbind(lineGraphData, newFrame)
      j=j+1
    }
      lineGraphData
    }, 
  options = list(searching = TRUE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE ))

  output$map1 <- renderLeaflet({
    aqsData <- read.table(file = "aqs_sites.csv", sep = ",", header = TRUE)
    aqsSpecific <- subset(aqsData, State.Name == input$State & County.Name == input$County)
    aqsSpecific <- subset(aqsSpecific, Latitude != 0)
    aqsSpecific <- subset(aqsSpecific, Longitude != 0)
    print(paste0("map: ", aqsSpecific))
    if("Latitude" %in% colnames(aqsSpecific)){
      m <- leaflet()
      m <- addTiles(m)
      m <- setView(m, lng=aqsSpecific$Longitude[1], lat=aqsSpecific$Latitude[1], zoom=6)
      m <- addMarkers(m, lng=aqsSpecific$Longitude[1], lat=aqsSpecific$Latitude[1], popup=paste0(input$State, ", ", input$County))
      m
    }
    else{
      print(paste0("map", "hu"))
      m <- leaflet()
      m <- addTiles(m)
      m <- setView(m, lng=aqsSpecific$Longitude[1], lat=aqsSpecific$Latitude[1], zoom=6)
      m <- addMarkers(m, lng=0, lat=0, popup=paste0(input$State, ", ", input$County))
      m
    }
  })
  
}

shinyApp(ui = ui, server = server)

