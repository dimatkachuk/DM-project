#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

yearChoices <- list("2001"="2001", "2002"="2002", "2003"="2003", "2004"="2004", "2005"="2005", "2006"="2006",
                    "2007"="2007", "2008"="2008", "2009"="2009", "2010"="2010", "2011"="2011", "2012"="2012",
                    "2013"="2013", "2014"="2014", "2015"="2015", "2016"="2016", "2017"="2017")
print('Start reading!')
crimes <- read.csv("Chicago.csv")
#cleaning
chicago_crimes <- subset(crimes, select = -c(Case.Number, IUCR, Updated.On, Beat, FBI.Code, Description, Location.Description))
#Add column hour of the day
chicago_crimes$Hour <- format(as.POSIXlt(chicago_crimes$Date, format = "%m/%d/%Y %I:%M:%S %p"),"%H")
#cleaning
chicago_crimes$Primary.Type[chicago_crimes$Primary.Type == "NON - CRIMINAL"] <- "NON-CRIMINAL"
chicago_crimes$Primary.Type[chicago_crimes$Primary.Type == "NON-CRIMINAL (SUBJECT SPECIFIED)"] <- "NON-CRIMINAL"


#Other narcotic violation added to narcotic violation
chicago_crimes$Primary.Type[chicago_crimes$Primary.Type == "OTHER NARCOTIC VIOLATION"] <- "NARCOTICS"
# Get number of crimes by type 
crimes_by_type <- group_by(chicago_crimes, chicago_crimes$Primary.Type)
crimes_number_by_type <- summarize(crimes_by_type, number = n())
names(crimes_number_by_type)[1] <- "crime"
crimes_number_by_type <- arrange(crimes_number_by_type, desc(number))
#Any offense with less than 10000 cases per all years added to common category 'Other Offense'
crimes_under_other_offense <- filter(crimes_number_by_type, number < 10000)
for (crime in crimes_under_other_offense$crime) {
  chicago_crimes$Primary.Type[chicago_crimes$Primary.Type == crime] <- "OTHER OFFENSE"
}
# Now after preprocessing we need to get number of crimes by type again
crimes_by_type <- group_by(chicago_crimes, chicago_crimes$Primary.Type)
crimes_number_by_type <- summarize(crimes_by_type, number = n())
names(crimes_number_by_type)[1] <- "crime"
#Now we will add percent of succesful arrests
crimes_type_info <- subset(chicago_crimes, select = c(Primary.Type, Arrest))
crimes_succ_arrest <- filter(crimes_type_info, Arrest == 'true')
crimes_by_type_succ <- group_by(crimes_succ_arrest, crimes_succ_arrest$Primary.Type)
crimes_number_succ_arrest <- summarize(crimes_by_type_succ, number = n())
names(crimes_number_succ_arrest)[1] <- "crime"
crimes_number_by_type$percent <- crimes_number_succ_arrest$number / crimes_number_by_type$number * 100
crimes_number_by_type <- arrange(crimes_number_by_type, percent)
crimes_number_by_type$crime <- factor(crimes_number_by_type$crime, levels=c("BURGLARY","CRIMINAL DAMAGE","MOTOR VEHICLE THEFT","ROBBERY","THEFT","ARSON","CRIM SEXUAL ASSAULT","DECEPTIVE PRACTICE","OTHER OFFENSE","OFFENSE INVOLVING CHILDREN","BATTERY","ASSAULT","SEX OFFENSE","PUBLIC PEACE VIOLATION","CRIMINAL TRESPASS","WEAPONS VIOLATION","INTERFERENCE WITH PUBLIC OFFICER","LIQUOR LAW VIOLATION","GAMBLING","NARCOTICS","PROSTITUTION"))

print(head(crimes))
print('Done reading!')

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  
  # monthChoices <- list("January" = "01/01", "February" = "02/02", "March" = "03/03",
  #                      "April" = "04/04", "May" = "05/05", "June" = "06/06",
  #                      "July" = "07/07", "August" = "08/08", "September" = "09/09",
  #                      "October" = "10/10", "November" = "11/11", "December" = "12/12"),
  #
  
  
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  plotOutput("plot", width ="100%", height = "100%"),
  plotOutput("plot1", width ="100%", height = "100%"),
  plotOutput("plot2", width ="100%", height = "100%"),
  plotOutput("plot3", width ="100%", height = "100%"),
  plotOutput("plot4", width ="100%", height = "100%"),
  plotOutput("plot5", width ="100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                
                selectInput("year", "Select a year:",
                            choices = yearChoices,
                            selected = yearChoices[1]
                ),
                
                dateRangeInput('dateRange2',
                               label = paste('Enter date range:'),
                               start = Sys.Date() - 3, end = Sys.Date() + 3, 
                               separator = " / ", format = "dd/mm/yy",
                               startview = 'year', language = 'en', weekstart = 1
                ),
                
                tags$h3("Crime types:"),
                checkboxInput("arson", "ARSON", FALSE),
                checkboxInput("assault", "ASSAULT" , FALSE),
                checkboxInput("battery","BATTERY" , FALSE),
                checkboxInput("burglary", "BURGLARY" , FALSE),
                checkboxInput("cancealed", "CONCEALED CARRY LICENSE VIOLATION" , FALSE),
                checkboxInput("criminal_damage", "CRIMINAL DAMAGE"  , FALSE),
                checkboxInput("criminal_trespass", "CRIMINAL TRESPASS", FALSE),
                checkboxInput("criminal_assaut", "CRIM SEXUAL ASSAULT" , FALSE),
                checkboxInput("deceptive_practice", "DECEPTIVE PRACTICE" , FALSE),
                checkboxInput("domestic_violence", "DOMESTIC VIOLENCE" , FALSE),
                checkboxInput("gambling","GAMBLING" , FALSE),
                checkboxInput("homicide", "HOMICIDE" , FALSE),
                checkboxInput("human_trafficking", "HUMAN TRAFFICKING" , FALSE),
                checkboxInput("officer", "INTERFERENCE WITH PUBLIC OFFICER" , FALSE),
                checkboxInput("intimidation", "INTIMIDATION"  , FALSE),
                checkboxInput("kidnapping", "KIDNAPPING" , FALSE),
                checkboxInput("liquor", "LIQUOR LAW VIOLATION"  , FALSE),
                checkboxInput("vehicle_theft", "MOTOR VEHICLE THEFT"  , FALSE),
                checkboxInput("narcotics", "NARCOTICS", FALSE),
                checkboxInput("other_offense", "OTHER OFFENSE", FALSE),
                checkboxInput("robbery", "ROBBERY" , FALSE),
                checkboxInput("liquor", "LIQUOR LAW VIOLATION"  , FALSE),
                checkboxInput("sex_offense", "SEX OFFENSE" , FALSE),
                checkboxInput("prostitution", "PROSTITUTION", FALSE),
                checkboxInput("theft", "THEFT", FALSE),
                checkboxInput("weapon_violation", "WEAPONS VIOLATION" , FALSE),
                checkboxInput("offense_child", "OFFENSE INVOLVING CHILDREN" , FALSE)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  output$dateRangeText2 <- renderText({
    paste("input$dateRange2 is", 
          paste(as.character(input$dateRange2), collapse = " to ")
    )
  })

  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  getData <- reactive({
    crimeTypesActual <- levels(crimes$Primary.Type)
    crimeTypesActive <- c(input$arson, input$assault, input$battery, input$burglary, input$cancealed, input$criminal_damage, input$criminal_trespass, input$criminal_assaut, input$deceptive_practice,
                          input$domestic_violence, input$gambling, input$homicide, input$human_trafficking, input$officer, input$intimidation,  input$kidnapping, input$liquor, input$vehicle_theft, 
                          input$narcotics, FALSE, FALSE, FALSE, FALSE, input$offense_child, FALSE, input$other_offense, input$prostitution, FALSE, FALSE, FALSE, input$robbery, input$sex_offense,
                          FALSE,  input$theft,  input$weapon_violation)
    print(crimeTypesActual)
    print(crimeTypesActive)
    crimeTypes <- crimeTypesActual[crimeTypesActive]
    print(crimeTypes)
    plotData <- crimes[which(crimes$Primary.Type %in% crimeTypes), ]
    return(plotData)
  })
  
  
  output$map <- renderLeaflet({
    print("Rerendering")
    mapData <- getData()
    mapData <- mapData[which(mapData$Year ==  input$year), ]

    leaflet(data =mapData) %>% addTiles() %>%
      addMarkers(~Longitude, ~Latitude, clusterOptions = markerClusterOptions(), popup = ~as.character(Description))
  })
  
  output$plot <-  renderPlot({ ggplot(chicago_crimes,aes(Year)) +
    geom_bar(aes(fill = Arrest), position="dodge", width=0.8) +
    coord_fixed(ratio=0.00003) +
    labs(x="Crimes per year with arrest factor in Chicago", y="number of crimes")  })
  
  output$plot1 <- renderPlot({ ggplot(la_crimes,aes(Year)) +
    geom_bar(aes(fill = Status.Description),position="dodge") +
    coord_fixed(ratio=0.00003) +
    labs(x="Crimes per year with arrest factor in LA", y="number of crimes") })
  
  output$plot2 <- renderPlot({ggplot(chicago_crimes,aes(Primary.Type)) +
      geom_bar(aes(fill = Arrest), width=0.8) +
      coord_fixed(ratio=0.00001) +
      labs(x="Crimes per type with arrest factor")  + theme(axis.text.x = element_text(angle=90)) })
  
  output$plot3 <- renderPlot({ ggplot(crimes_number_by_type,aes(crime,percent)) +
      geom_col() +
      labs(x="Crime types", y="percent of successful arrests")  + theme(axis.text.x = element_text(angle=90)) })
  
  output$plot4 <- renderPlot({ ggplot(chicago_crimes,aes(Hour)) +
      geom_bar(aes(fill = Arrest), width=0.8) +
      coord_fixed(ratio=0.00005) +
      labs(x="Crimes per hour of day with arrest factor in Chicago", y="number of crimes")  })
  
  output$plot5 <- renderPlot({ ggplot(la_crimes,aes(Hour)) +
      geom_bar(aes(fill = Status.Description), width=0.8) +
      coord_fixed(ratio=0.00015) +
      labs(x="Crimes per hour of day with arrest factor in LA",y="number of crimes")  })
  
  output$plot6 <- renderPlot({  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

