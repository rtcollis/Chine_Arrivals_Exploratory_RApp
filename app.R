#Data Mining in R 
#Assignment 6 - Shiny App 

#Done by:
#Jose Beltran Vilardy
# Borja Ureta
# Nicole Lerman
# Timucin Engin
# Robin Collis
# Sandra Sorza



# Vector with the required packages 
packages <- c("shiny","dplyr", "lubridate", "stats", "tidyr", "readxl", "readr", "ggplot2",
              "shinythemes", "shinydashboard", "here", "tibble", "scales") 
# Checking for package installations on the system and installing if not found.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
# Packages to use
for(package in packages){
  library(package, character.only = TRUE)
}

setwd(here::here())
### Exploratory Analysis
#Import dataset
#Import table with 15 to 19 data
pdset15_19 <- as.tibble(read_xlsx('forwardkeys-Arrivals-History-Paris(FR)_PAR.xlsx', col_names = FALSE, na = c("", "NA"), sheet = 'China-CN',
                                  skip = 121, n_max = 1703))
#Import 2014 from second table
pdset14 <- as.tibble(read_xlsx('forwardkeys-Arrivals-History-Paris(FR)_PAR.xlsx', col_names = FALSE, na = c("", "NA"), sheet = 'China-CN',
                               skip = 1828, n_max = 364))
#Join both tables by row
pdset <- rbind(pdset14, pdset15_19)


#Change column names
colnames(pdset) <- c('Date', 'Total_trav', 'Total_trav_China/CN', 'Returnhome', 'ShortTransfer', 'DwellingTransfer', 'LongTransfer', 'Stopover', 'DayTrip', 'LoS_1night', 'LoS_2nights', 'LoS_3nights', 'LoS_4to5nights', 'LoS_6to8nights', 'LoS_9to13nights', 'LoS_14to21nights', 'LoS_22nightsormore', 'Endoftrip', 'TrDur_0night', 'TrDur_1night', 'TrDur_2nights', 'TrDur_3nights', 'TrDur_4to5nights', 'TrDur_6to8nights', 'TrDur_9to13nights', 'TrDur_14to21nights', 'TrDur_22nightsormore', 'ppb_1pax', 'ppb_2pax', 'ppb_3pax', 'ppb_4pax', 'ppb_5pax', 'ppb_6to9pax', 'ppb_10pl_pax', 'LeadT_0to4days', 'LeadT_5to14days', 'LeadT_15to29days', 'LeadT_30to44days', 'LeadT_45to59days', 'LeadT_60to89days', 'LeadT_90to119days', 'LeadT_120to364days', 'Cabin_Economy', 'Cabin_EconomyPremium', 'Cabin_Business', 'Cabin_First', 'DistCh_OnlineTA', 'DistCh_CorporateTA', 'DistCh_RetailTA', 'DistCh_OtherTA', 'TypeSt_Weekendstay', 'TypeSt_Workweekstay', 'TypeSt_Combinedstay', 'TypeSt_Nostay', 'OneRet_Oneway', 'OneRet_Return', 'OneRet_MultiCity', 'TrDay_Sunday', 'TrDay_Monday', 'TrDay_Tuesday', 'TrDay_Wednesday', 'TrDay_Thursday', 'TrDay_Friday', 'TrDay_Saturday', 'Profile_Business', 'Profile_Leisure', 'Profile_Group', 'Profile_VFRndExpats', 'Sign_InitialBookings', 'Sign_Partialadditions_modif', 'Sign_Partialcancellations', 'Sign_Fulltripcancellations', 'Avg.LOS', 'Avg.LOT', 'Avg.LT', 'Avg.PPB')

#Get rid of columns that have no data and are not useful for our analysis
pdset <- dplyr::select(pdset, -c('Total_trav_China/CN', 'Returnhome', 'ShortTransfer', 'DwellingTransfer', 'LongTransfer', 'Stopover', 'DayTrip', 'Endoftrip', 'TrDur_0night', 'TypeSt_Nostay', 'TrDay_Sunday', 'TrDay_Monday', 'TrDay_Tuesday', 'TrDay_Wednesday', 'TrDay_Thursday', 'TrDay_Friday', 'TrDay_Saturday', 'Profile_VFRndExpats', 'OneRet_Oneway'))

#Add day of the week, week, month and year.
pdset <- dplyr::mutate(pdset, 
                       WeekDay=lubridate::wday(pdset$Date, label = TRUE), 
                       Month=lubridate::month(pdset$Date, label = TRUE), 
                       Week=lubridate::week(pdset$Date),
                       Year=lubridate::year(pdset$Date))

pdset_14_18 <- subset(pdset, Year <= 2018)

barfill = "gold1"
barlines = "goldenrod2"


### Predictive Analysis

table1 <- as.data.frame(read_csv("apptable.csv"))%>%dplyr::select(-X1)
table2 <- gather(table1, model, value, `XGBoost`:`Prophet_Regressor`)%>%dplyr::filter(Date >= "2019-08-01")

table <- table1%>%dplyr::filter(Date >= "2019-08-01")

list <- unique(table2$model)


rmse.xgbt <- round(sqrt(mean((table$Actuals - table$XGBoost)^2)),2)
nmse.xgbt <- round(sum((table$Actuals - table$XGBoost)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

rmse.rft <- round(sqrt(mean((table$Actuals - table$RandomForest)^2)),2)
nmse.rft <- round(sum((table$Actuals - table$RandomForest)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

ensmb.rmse <- round(sqrt(mean((table$Actuals - table$Prediction_Average)^2)),2)
ensmb.nmse <- round(sum((table$Actuals - table$Prediction_Average)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

rmseDHR <- round(sqrt(mean((table$Actuals - table$DHR)^2)),2)
nmseDHR <- round(sum((table$Actuals - table$DHR)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

rmsetbats <- round(sqrt(mean((table$Actuals - table$TBATS)^2)),2)
nmsetbats <- round(sum((table$Actuals - table$TBATS)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

rmseprophet <- round(sqrt(mean((table$Actuals - table$Prophet)^2)),2)
nmseprophet <- round(sum((table$Actuals - table$Prophet)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

rmserprophetavglos <- round(sqrt(mean((table$Actuals - table$Prophet_Regressor)^2)),2)
nmseprophetr <- round(sum((table$Actuals - table$Prophet_Regressor)^2) / sum((table$Actuals-mean(table$Actuals))^2)*100,2)

tablem <- data.frame("Model" = c('XGBoost Refined','Random Forest Refined', "Ensamble", "DHR", "TBATS", "Prophet", "Prophet with Regressor"), 
                            "RMSE" = c(rmse.xgbt,rmse.rft, ensmb.rmse, rmseDHR, rmsetbats, rmseprophet, rmserprophetavglos ), 
                            "NMSE" = c(nmse.xgbt,nmse.rft, ensmb.nmse, nmseDHR, nmsetbats, nmseprophet, nmseprophetr))%>%arrange(RMSE)




#========SHINY INFRASTRUCTURE========

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Machine Learning Models to Predict Chinese tourists into Paris", titleWidth = 650),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
                        menuItem("Exploratory Analysis", tabName = "EA", icon = icon("chart-bar")),
                        menuItem("Trend and Seasonality", tabName = "TS", icon = icon("signal")),
                        menuItem("Models Evaluation", tabName = "ML", icon = icon("signal"))
                        
                      ),
                        selectInput("Models", "Models:",
                                    choices = list)
                      ),
        
                    
                    dashboardBody(
                      shinydashboard::tabItems(
                        tabItem("Overview",
                                h1("Project Overview"),
                                h3("We partnered with DFS Group to forecast travel and purchase intent of Chinese consumers to Paris. DFS Group is the world's leading 
                                   luxury travel retailer. Network consists of duty free stores located in 11 major global airports and 20 downtown T Galleria locations."),
                                
                                h3("DFS has re-oriented its strategy towards the Chinese market and being as close as possible to where Chinese travellers are. 
                                   As part of this strategy, DFS will be opening a new store in Paris, France. This store will have a deep focus on Chinese tourists."),
                                
                                h3("Our initial goal is to predict flows of Chinese tourists into Paris. For this, we will leverage certain information 
                                   owned by DFS on actual and forward looking reservation data from a proprietary database."),
                                
                                h1("The Dataset"), 
                                h3("DFS provided us with a data pull from FK. This dataset contains the actual information on Chinese travelers to Paris between 
                                   January 1 2015 and August 31, 2019. It also provides additional valuable information such as length of stay in Paris, total trip duration (overall), number of people per booking, lead times between booking and flight, cabin, distribution channels, type of stay, one-way/return, travel day of the week and traveler profile (i.e. business, leisure and group)"), 
                                
                                h1("Goal of this analysis"),
                                h3("-Visualization and insights: analyze the data and capture relevant trends, insights and an initial indication of potential predictors"),
                                h3("-Predictive modeling: develop a predictive model that predicts the number of travelers to Paris on a particular date.")
                        
                                
                                ),
                        
                        tabItem("EA",
                                h2("Exploratory Analysis"),
                                fluidRow(
                                
                                  box(width = 6, solidHeader = TRUE, status = "primary",
                                      plotOutput("stockPlot2")
                                  ),
                                
                                box(width = 6, solidHeader = TRUE, status = "primary",
                                    plotOutput("stockPlot3")
                                )),
                                
                                fluidRow(
                                  
                                  
                                  box(width = 6, solidHeader = TRUE, status = "primary",
                                      plotOutput("stockPlot4")
                                  ),
                                  
                                  box(width = 6, solidHeader = TRUE, status = "primary",
                                      plotOutput("stockPlot5")
                                  ))
                                
                                
                                ),
                              
                        tabItem("ML",
                                h1("Machine Learning and Time Series Models Performance on Test Data"),
                                h1(textOutput("textm")),
                               
                                fluidRow(
                                  valueBoxOutput("last"),
                                  valueBoxOutput("rmse"),
                                  valueBoxOutput("nmse")
                                ),
                                
                                
                                 fluidRow(
                                  box(width = 4, solidHeader = TRUE, status = "primary",
                                      title = "Accuracy Metrics", height = 550,
                                      DT::dataTableOutput("table")),
                                  
                                  box(width = 8, solidHeader = TRUE, status = "primary",
                                      title = textOutput("text"), 
                                      plotOutput("stockPlot", width = "100%", height = 500)
                                  )
                                  
                                  
                                )),
                        
                        tabItem("TS",
                                h1("Trend and Seasonality"),
                                fluidRow(
                                  
                                  box(width = 12, solidHeader = TRUE, status = "primary",
                                      title = "Chinese Travelers per year",
                                      plotOutput("stockPlot6", width = "100%", height = 600)
                                  )
                                  
                                  
                                ),
                                
                                h3("there is a strong pattern that is repeated each year with a peak during the summer months, with a slight 
                                decline in August that picks up in September. Weekend are the strongest days of the week. 
                                   In terms of years 2015 showed the largest flows of Chinese tourists, with 2016 to 2019 being quite stable.")
                                
                                )
                        

                        )
                    )
                    
)



# Define server logic to plot  ----
server <- function(input, output) {
  output$stockPlot <- renderPlot({
    table3 <- table2%>%dplyr::filter(model == input$Models)
    
    p <- ggplot(table3, aes(x = Date)) + geom_line(aes(y = Actuals, color='Actuals'), linetype=5, size=1.5) +
    geom_line(aes(y = value, color='Forecast')) +
    labs(color='Legend') + theme_classic()
    p
    
  })
  
  output$rmse <-  renderValueBox({
    table4 <- table2%>%dplyr::filter(model == input$Models, Date >= "2019-08-01")
    c  <- round(sqrt(mean((table4$Actuals - table4$value)^2)),2)
    valueBox(
      value = c,
      subtitle = "RMSE", color = "yellow",
      icon = icon("area-chart")
    )
  })
  
  output$nmse <-  renderValueBox({
    table4 <- table2%>%dplyr::filter(model == input$Models, Date >= "2019-08-01")
    d <- round(sum((table4$Actuals - table4$value)^2) / sum((table4$Actuals-mean(table4$Actuals))^2)*100,2)
    valueBox(
      value = d,
      subtitle = "NMSE",
      icon = icon("chart-bar")
    )
  })
  
  output$last <-  renderValueBox({
    tableo <- table2%>%dplyr::filter(model == input$Models, Date == "2019-08-01")
    d <- round(tableo$value,0)
    valueBox(
      value = d,
      subtitle = "Last Day Prediction Value",
      icon = icon("chart-bar")
    )
  })
  
  output$textm <-  renderText({

    paste("Model Selected is:",input$Models)
  
  })
  
  output$stockPlot2 <- renderPlot({
  p_hist_tt_y <- ggplot(data = pdset_14_18, aes(x = Year, y = Total_trav)) +
    geom_bar(stat="identity", fill = "#33DAFF") + 
    theme_light() +
    labs(x = "Year", y = "Total Inbound Passengers") + 
    scale_y_continuous(labels = comma) +
    ggtitle("Total Chinese Passengers Arrivals Direct to Paris by Year 2014 - 2018")
  p_hist_tt_y 
  })
  
  output$stockPlot3 <- renderPlot({
  p_hist_tt_m <- ggplot(data = pdset_14_18, aes(x = Month, y = Total_trav)) +
    geom_bar(stat="identity", fill = "#33DAFF") + 
    theme_light() +
    labs(x = "Month", y = "Total Inbound Passengers") + 
    scale_y_continuous(labels = comma) +
    ggtitle("Total Chinese Passengers Arrivals Direct to Paris by Month 2014 - 2018")
  p_hist_tt_m
  })
  
  output$stockPlot4 <- renderPlot({

  p_hist_tt_d <- ggplot(data = pdset_14_18, aes(x = WeekDay, y = Total_trav)) +
    geom_bar(stat="identity", fill = "#33DAFF") + 
    theme_light() +
    labs(x = "Week Day", y = "Total Inbound Passengers") + 
    scale_y_continuous(labels = comma) +
    ggtitle("Total Chinese Passengers Arrivals Direct to Paris by Weekday 2014 - 2018")
  p_hist_tt_d
  })
  
  output$stockPlot5 <- renderPlot({

  p_hist_tt_wk <- ggplot(data = pdset_14_18, aes(x = Week, y = Total_trav)) +
    geom_bar(stat="identity", fill = "#33DAFF") + 
    theme_light() +
    labs(x = "Week", y = "Total Inbound Passengers") + 
    scale_y_continuous(labels = comma) +
    ggtitle("Total Chinese Passengers Arrivals Direct to Paris by Week 2014 - 2018")
  p_hist_tt_wk 
  
  })
  
  output$stockPlot6 <- renderPlot({
    
  ggplot(data = pdset, aes(x = Date, y = Total_trav)) + geom_line(color = 'DarkBlue', size = 0.25)
    
  })
  
  output$table <- DT::renderDataTable({
    tablem })
  
  output$text <- renderText({
     a <- input$Models
    paste("Chinise Passenger Forecast using", a , "model")
  })
  
  

  
}

# Create Shiny app ----
shinyApp(ui, server)