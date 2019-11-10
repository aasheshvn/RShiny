###Path Setting and Installing packages
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, shiny, ggplot2,plyr ,leaflet,install = TRUE)

###Reading the dataset
df <- fread("Chicago_Crimes_2018.csv")

###Converting the "Date" column to class Date
df$Date <- as.POSIXct(df$Date,format="%m/%d/%Y %H:%M")
df$month <- months(df$Date)    ### Month extraction from date
df$month <- as.factor(df$month)
df$hour <- hour(df$Date)       ### Hour extraction from date
df$hour <- as.factor(df$hour)
df <- na.omit(df)             ### Omitting the Null Values

### Location Preprocessing
### Replacing location values to "Other" where frequency of crime in the location is less than 100
tmp <- data.frame(table(df$`Location Description`))

### Retrieving locations with frequency of crime less than 100
lessthan_100 <- tmp[tmp$Freq <= 100,]

### Replacing the values in the dataframe
df[df$`Location Description` %in% lessthan_100$Var1]$`Location Description` <- "OTHER"

ui <-  fluidPage(
  tabsetPanel(
    tabPanel("Frequency of crime by month and crime type",fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "crime_type", label = 'Select a crime',
                             choices = unique(df$`Primary Type`)
                 )),
               mainPanel(
                 plotOutput(outputId = "crimeMonthType")
               )
             )
    ),
    tabPanel("Frequency of crime by location",fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "crime_location", label = 'Select locations with crimes',
                             choices = c("greater than 5000","between 1000 and 5000",
                                         "between 500 and 1000","between 100 and 500")
                 )),
               
               mainPanel(
                 plotOutput(outputId = "crimeLocation")
               )
             )
    ),
    tabPanel("Crime by hour and type",fluid = TRUE,
              sidebarLayout(
                sidebarPanel(
                  radioButtons(inputId = "crime_hour", label = 'Select an option for crimes',
                               choices = c("Most Frequently occuring","Frequently occuring",
                                           "Rarely occuring")
                  )),
                mainPanel(
                  plotOutput(outputId = "crimeHour")
                )
              )
    ),
    tabPanel("Crimes by Date on Maps",fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
              dateInput("date", "Select a Date:",value="2018-01-01",min = "2018-01-01", max = "2018-12-31",format = "yyyy-mm-dd")),
    mainPanel(
      leafletOutput(outputId = "mymap")
    )
    
    ))))

server <- function(input, output) {
  output$crimeMonthType<-renderPlot({
    ###Filtering the dataset for crime type
    df_crime_type <- df[df$`Primary Type` == input$crime_type,]
    ###Plotting the crime type for every month
    ggplot(data = df_crime_type, aes(month)) + geom_bar(fill = "darkblue",width = 0.8) +scale_x_discrete(limits=month.name)+scale_y_continuous()+labs(title = "Crime by every month")
  })
  output$crimeLocation<-renderPlot({ ### Filtering the dataset based on location & Crimes frequency
    if (input$crime_location == 'greater than 5000'){
      location <- tmp[tmp$Freq >= 5000,]
    } else if (input$crime_location == 'between 1000 and 5000'){
      location <- tmp[(tmp$Freq >= 1000) & (tmp$Freq < 5000),]
    } else if (input$crime_location == 'between 500 and 1000'){
      location <- tmp[(tmp$Freq > 500) & (tmp$Freq < 1000),]
    } else if (input$crime_location == 'between 100 and 500'){
      location <- tmp[(tmp$Freq >= 100) & (tmp$Freq < 500),]
    }
    ggplot(data =location, aes(x=Var1,y=Freq)) + geom_bar(stat = "identity")+coord_flip() +ylab("Frequency of Crimes") + xlab("Location")+labs(title = "Crimes based on Locations")
    
  })
  output$crimeHour<-renderPlot({ ### Filtering the dataset based on crime type and its frequency
    df_heatmap <- count(df, c("hour","`Primary Type`"))
    crime_type<-data.frame(table(df$`Primary Type`))
    
    if (input$crime_hour == 'Most Frequently occuring'){
      crimes <- crime_type[crime_type$Freq >= 10000,]
      crime_hour_heatmap<-df_heatmap[df_heatmap$Primary.Type %in% crimes$Var1,]
      
    } else if (input$crime_hour == 'Frequently occuring'){
      crimes <- crime_type[(crime_type$Freq >= 1000) & (crime_type$Freq < 10000),]
      crime_hour_heatmap<-df_heatmap[df_heatmap$Primary.Type %in% crimes$Var1,]
      
    } else if (input$crime_hour == 'Rarely occuring'){
      crimes <- crime_type[crime_type$Freq < 1000,]
      crime_hour_heatmap<-df_heatmap[df_heatmap$Primary.Type %in% crimes$Var1,]
    } 
    ggplot(data = crime_hour_heatmap, aes(x=hour,y=`Primary.Type`,fill=freq)) + geom_tile() +labs(title = "Heatmap by every hour and crime type")
    
  })
  
  output$mymap <- renderLeaflet({
    df$Date <- as.Date(df$Date)
    date_df <- df[df$Date == input$date]
    leaflet(date_df) %>% setView(lng = -87.6, lat = 41.8, zoom = 10)  %>% #Default Map Setting
      addTiles() %>% addControl("Click on a location to view the crime details", position = "topright") %>%
      addCircleMarkers(data = date_df, lat = ~ Latitude, lng = ~ Longitude,radius = 4, weight = 1, fillOpacity = 0.5,popup = paste("Case Number: ",date_df$`Case Number`,"<br>","Primary Type: ",date_df$`Primary Type`,"<br>","Description: ",date_df$Description))
                                                                                              
  })

  }

shinyApp(ui = ui, server = server)

