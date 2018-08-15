#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(rsconnect)
library(DT)
library(dplyr)
library(plotly)
library(knitr)

# Data for "explore the data" section
explore <- read.csv("./outputData/explore.csv")
# NEED TO FIX CLASSES OF VARIABLES (e.g. Open should be factor)
explore$year <- as.factor(explore$year)
explore$district <- as.factor(explore$district)
explore$open <- as.factor(explore$open)
explore$challenged <- as.factor(explore$challenged)
explore$midterm <- as.factor(explore$midterm)
explore$presINCsameParty <- as.factor(explore$presINCsameParty)
explore$incWin <- as.factor(explore$incWin)
explore$thirdParty <- as.factor(explore$thirdParty)
explore$midtermEffect <- as.factor(explore$midtermEffect)
explore$dWin <- as.factor(explore$dWin)
explore$crossPressure <- as.factor(explore$crossPressure)

## Fix factor orders for coloring
explore$INC <- factor(explore$INC, levels = c("R", "D"))
explore$winner <- factor(explore$winner, levels = c("R", "D"))
explore$presParty <- factor(explore$presParty, levels = c("R", "D"))
explore$OpenInc <- factor(explore$OpenInc, levels = c("R", "Open", "D"))
explore$midPres <- factor(explore$midPres, levels = c("R", "On-cycle", "D"))
explore$dWin <- factor(explore$dWin, levels = c("0", "NA", "1"))
explore$incWin <- factor(explore$incWin, levels = c("0", "NA", "1"))

numVars <- colnames(select_if(explore, is.numeric))
factorVars <- colnames(select_if(explore, is.factor))


# Define UI for application -- will have tabs for different sections
ui <- navbarPage(title = "2018 Midterms", tabPanel("Forecast", fluidPage(
  # Sidebar layout with dems chances over time and individual district table in the sidebar
  sidebarLayout(
    sidebarPanel(
      htmlOutput("demProb"),
      tags$head(tags$style(HTML("
                                #demProb {
                                font-size: 20px;
                                }
                                "))),
      textOutput("asOfDate"),
      br(),
      plotOutput("oddsPlot"),
      h4("Individual District Forecasts"),
      dataTableOutput("distTable")
      ),
    
    # Show a plot of the histogram of seats expected to win and the dotplot of each district
    mainPanel(
      plotOutput("distPlot", width = "800px"),
      br(),
      br(),
      br(),
      br(),
      plotlyOutput("dotPlot", height = "600px", width = "800px")
    )
      )
      )), 
  # Methodology tab, grabs the .Rmd which describes the methodology
  tabPanel("Methodology", 
           tags$iframe(src = 'forecastMethodology.html', # put testdoc.html to /www
                       width = '100%', height = '3800px', 
                       frameborder = 0, scrolling = 'auto')), 
  #Explore the data tab
  tabPanel("Explore the data", fluidPage(
    # Sidebar with drop down menus for variables and color, plus an explanation of what type of chart will be created based on the variable type
    sidebarLayout(
      sidebarPanel(
        h4("Select variables"),
        selectInput(inputId = "x", label = "x-axis", choices = c("none", sort(colnames(explore))), selected = "weightedPVI"),
        selectInput(inputId = "y", label = "y-axis", choices = c("none", sort(colnames(explore))), selected = "dPct2"),
        selectInput(inputId = "col", label = "Color", choices = c("none", sort(colnames(explore))), selected = "INC"),
        checkboxGroupInput(inputId = "years", label = "Election years", choices = levels(explore$year), selected = levels(explore$year)),
        checkboxInput(inputId = "exclude", label = "Exclude unchallenged elections?", value = FALSE),
        downloadButton('downloadData', 'Download data'),
        br(),
        br(),
        img(src = "plotGuide.png", width = "80%")
      ),
      
      # Show the plot and data dictionary below it
      mainPanel(
        plotOutput("explorePlot", height = "600px"),
        br(),
        br(),
        h4("Data Dictionary"),
        dataTableOutput("dataDict")
      )
    )
  )), 
  #Contact me tab
  tabPanel("Contact", helpText(a("Check out my website", href="http://www.parkermquinn.com", target="_blank")),
           br(),
           actionLink("email", h5(a("Email me at pquinn91@gmail.com", href="mailto:pquinn91@gmail.com"))),
           br(),
           helpText(a("Find me on Twitter! @parkermquinn", href="https://twitter.com/parkermquinn", target="_blank"))
           ))

# Define server logic required
server <- function(input, output) {
  # All of the data we will need
  dfCount <- read.csv("./outputData/dfCount.csv")
  dWin <- read.csv("./outputData/dWin.csv")
  demUnchallenged <- dfCount$demUnchallenged[1]
  dProb <- table(dfCount$dWin + demUnchallenged >= 218)[2]/20000
  rfTestCheck <- read.csv("./outputData/rfTestCheck.csv")
  rfTestCheck$Pred[rfTestCheck$Pred > 19980] <- 19980
  rfTestCheck$Pred[rfTestCheck$Pred < 20] <- 20
  rfTestCheck$DemPct <- round(rfTestCheck$Pred/20000,3)*100
  rfTestCheck$district2 <- paste0(rfTestCheck$state, "", rfTestCheck$district)
  rfTestCheck$dPctPred <- round(rfTestCheck$dPctPred,3)*100
  distTable <- subset(rfTestCheck, select = c("district2", "INC", "dPctPred", "DemPct"))
  distTable <- arrange(distTable, district2)
  colnames(distTable) <- c("", "INC", "Vote Share (D)", "Probability (D)")
  
  # Histogram of seats won by democrats in each simulation
  output$distPlot <- renderPlot({
    ggplot(dfCount, aes(dWin + demUnchallenged)) + 
      geom_histogram(binwidth = 1, aes(fill = dWin + demUnchallenged >= 218, y = ..count../20000)) + 
      xlab("") + ylab("Probability") + 
      ggtitle("How many seats can Democrats expect to win?") + 
      geom_vline(aes(xintercept = 217.5), linetype = 2) + 
      #geom_vline(aes(xintercept = median(dWin + demUnchallenged)), linetype = 1) + 
      annotate("text", x = 218, y = .0075, label = "\n218 seats needed for majority", angle = 90, colour = "white", size = 4) + 
      scale_y_continuous(labels = scales::percent, limits = c(0,.0259)) + 
      scale_fill_manual(values=c("#D6604D", "#4393C3")) + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 20, hjust = .33, face = "bold"), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.ticks.y = element_blank()) + 
      theme(legend.position = "none")
  })
  
  # Text at the top of the app will explain Dems chances of taking control and the "as of" date
  output$demProb <- renderUI({
    p("Democrats have a ", span(strong(paste0(round(dProb*100,1), "%")), style = "color:blue"), "chance of a House majority")
  })
  
  output$asOfDate <- renderText({
    paste0("Last updated ", file.info("./outputData/dWin.csv")$mtime)
  })
  
  # Show the Dems chances over time with two lines
  output$oddsPlot <- renderPlot({
    ggplot(dWin, aes(as.Date(Date, format = "%Y-%m-%d"), group = 1)) + 
      geom_line(aes(y = dProb, colour = "Dem"), size = 1, alpha = .6) + 
      geom_line(aes(y = (1-dProb), colour = "Rep"), size = 1, alpha = .6) + 
      scale_colour_manual(values = c("blue", "red")) + 
      xlim(as.Date(c('6/1/2018', '11/15/2018'), format("%m/%d/%Y"))) + 
      geom_vline(aes(xintercept = as.numeric(as.Date("2018-11-06"))), linetype = 2) + 
      annotate("text", x = as.Date("2018-11-06"), y = .5, label = "\nNovember 6th, 2018", angle = 90, colour = "darkblue", size = 5) + 
      theme_minimal() + 
      theme(legend.position = "none") + 
      ggtitle("Probability over time") + xlab("") + ylab("") +
      theme(plot.title = element_text(size = 14, hjust = .5, face = "bold"), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + 
      scale_y_continuous(labels = scales::percent, limits = c(0,1))
  }, height = 250)
  
  # Individual district forecasts table
  output$distTable <- renderDataTable({
    datatable(distTable, rownames = FALSE)
  }, options = list(pageLength = 40, order = list(list(1, 'asc'))))
  
  ### This will create the dot plot -- since this dotplot is stacked by category, and I wanted several columns of dots for each category (instead of one lone column),
  ### I had to bin the data based on the Democrat's chances, and use the bins as an x-axis variable based on how many columns I wanted for each category. Then I also
  ### had to create a corresponding y-axis variable for the dot height. 
  rfTestCheck$bin <- .bincode(rfTestCheck$DemPct, c(0,1,10,25,40,60,75,90,99,100))
  rfTestCheck$bin <- as.factor(rfTestCheck$bin)
  rfTestCheck$DemPct2 <- rfTestCheck$DemPct
  rfTestCheck$y <- 0
  
  rfTestCheck <- arrange(rfTestCheck, bin, district2)
  j <- 1
  k <- 1
  l <- 44
  for(i in 1:nrow(rfTestCheck)){
    if(i <= sum(table(rfTestCheck$bin)[1:j]) & k <= ceiling(table(rfTestCheck$bin)/5)[j]){
      rfTestCheck$DemPct2[i] <- l
      rfTestCheck$y[i] <- k
      k <- k + 1
    } else if(i <= sum(table(rfTestCheck$bin)[1:j]) & k > ceiling(table(rfTestCheck$bin)/5)[j]){
      k <- 2
      l <- l - 1
      rfTestCheck$DemPct2[i] <- l
      rfTestCheck$y[i] <- 1
    } else{
      j <- j + 1
      k <- 2
      l <- l - 1
      rfTestCheck$DemPct2[i] <- l
      rfTestCheck$y[i] <- 1
    }
  }
  
  #Select colors for each category, and the category names
  myColors <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "darkgrey", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  levels(rfTestCheck$bin) <- c("<1%", "1-10%", "10-25%", "25-40%", "40-60%", "60-75%", "75-90%", "90-99%", ">99%")
  
  
  ## Create the interactive dotplot, which is really a geom_point with an x- and y-axis variable from the loops above
  output$dotPlot <- renderPlotly({ggplotly(ggplot(rfTestCheck, aes(x = DemPct2, y = y, fill = bin, text = paste0(state, " District ", district, "<br>",DemPct, "% Dem"))) + 
                                             geom_point(size = 3, aes(color = bin)) + 
                                             scale_fill_manual(values = myColors) + 
                                             scale_colour_manual(values = myColors) + 
                                             ggtitle("Which districts are competitive?") + 
                                             xlab("Probability (Dem.)") + 
                                             theme_minimal() + 
                                             theme(plot.title = element_text(size = 14, hjust = .5, face = "bold"), 
                                                   axis.text.x = element_blank(), 
                                                   axis.text.y = element_blank(),
                                                   axis.title.x = element_text(size = 14, face = "bold"),
                                                   axis.title.y = element_blank(),
                                                   axis.ticks.y = element_blank(),
                                                   panel.grid = element_blank()) + 
                                             theme(legend.position = "none") + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == ">99%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == ">99%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == ">99%"]))/2), y = -1, label = "\n>99%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "90-99%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "90-99%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "90-99%"]))/2), y = -1, label = "\n90-99%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "75-90%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "75-90%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "75-90%"]))/2), y = -1, label = "\n75-90%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "60-75%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "60-75%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "60-75%"]))/2), y = -1, label = "\n60-75%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "40-60%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "40-60%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "40-60%"]))/2), y = -1, label = "\n40-60%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "25-40%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "25-40%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "25-40%"]))/2), y = -1, label = "\n25-40%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "10-25%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "10-25%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "10-25%"]))/2), y = -1, label = "\n10-25%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "1-10%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "1-10%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "1-10%"]))/2), y = -1, label = "\n1-10%", colour = "black", size = 4) + 
                                             annotate("text", x = min(rfTestCheck$DemPct2[rfTestCheck$bin == "<1%"])+((max(rfTestCheck$DemPct2[rfTestCheck$bin == "<1%"]) - min(rfTestCheck$DemPct2[rfTestCheck$bin == "<1%"]))/2), y = -1, label = "\n<1%", colour = "black", size = 4)
                                           , tooltip = "text")})
  

  ### This is the explore the data chart -- it is a lot of if statements which check the types of variables that the user selected and makes a chart that works with those variable types\
  explorePlotData <- explore

  #One variable (factor) = bar chart (if only y-axis, coord_flip())
  output$explorePlot <- renderPlot({
    if(input$exclude){
      explorePlotData <- subset(explore, challenged == 1)
    }else{explorePlotData <- explore}
    
    explorePlotData <- subset(explorePlotData, year %in% input$years)
    
    if(input$x %in% factorVars & input$y == "none" & (input$col == "none" | input$col %in% numVars)){
    ggplot(explorePlotData, aes_string(x = input$x, fill = input$x)) + geom_bar() + theme_minimal()
  }else if(input$x %in% factorVars & input$y == "none" & input$col %in% factorVars){
    ggplot(explorePlotData, aes_string(x = input$x, fill = input$col)) + geom_bar() + theme_minimal()
  }else if(input$y %in% factorVars & input$x == "none" & (input$col == "none" | input$col %in% numVars)){
    ggplot(explorePlotData, aes_string(x = input$y, fill = input$y)) + geom_bar() + coord_flip() + theme_minimal()
  }else if(input$y %in% factorVars & input$x == "none" & input$col %in% factorVars){
    ggplot(explorePlotData, aes_string(x = input$y, fill = input$col)) + geom_bar() + coord_flip() + theme_minimal()
  }else
    
    #One variable (numeric) = density (split by color and see-thru, if selected) (if only y-axis, coord_flip())
    if(input$x %in% numVars & input$y == "none" & (input$col == "none" | input$col %in% numVars)){
      ggplot(explorePlotData, aes_string(x = input$x)) + geom_density() + theme_minimal()
    }else if(input$x %in% numVars & input$y == "none" & input$col %in% factorVars){
      ggplot(explorePlotData, aes_string(x = input$x, fill = input$col)) + geom_density(aes_string(group = input$col), alpha = .5) + theme_minimal()
    }else if(input$y %in% numVars & input$x == "none" & (input$col == "none" | input$col %in% numVars)){
      ggplot(explorePlotData, aes_string(x = input$y)) + geom_density() + coord_flip() + theme_minimal()
    }else if(input$y %in% numVars & input$x == "none" & input$col %in% factorVars){
      ggplot(explorePlotData, aes_string(x = input$y, fill = input$col)) + geom_density(aes_string(group = input$col), alpha = .5) + coord_flip() + theme_minimal()
    }else
      
      #Two variables (factor, numeric) = boxplot
      #Two variables (numeric, factor) = boxplot (coord_flip())
      if(input$x %in% factorVars & input$y %in% numVars){
        ggplot(explorePlotData, aes_string(x = input$x, y = input$y)) + geom_boxplot(aes_string(color = input$x)) + theme_minimal()
      }else if(input$x %in% numVars & input$y %in% factorVars){
        ggplot(explorePlotData, aes_string(y = input$x, x = input$y)) + geom_boxplot(aes_string(color = input$y)) + coord_flip() + theme_minimal()
      }else
        
        #Two variables (numeric, numeric) = scatterplot
        if(input$x %in% numVars & input$y %in% numVars & input$col == "none"){
          ggplot(explorePlotData, aes_string(x = input$x, y = input$y)) + geom_point() + geom_smooth(method = "loess") + theme_minimal()
        } else if(input$x %in% numVars & input$y %in% numVars & input$col != "none"){
          ggplot(explorePlotData, aes_string(x = input$x, y = input$y)) + geom_point(aes_string(color = input$col)) + geom_smooth(method = "loess") + theme_minimal()
        }else
          
          #Two variables (factor, factor)(no color) = table
          #Two variables (factor, factor)(color) = jitter
          if(input$x %in% factorVars & input$y %in% factorVars & input$col == "none"){
            ggplot(as.data.frame(table(explorePlotData[,input$x], explorePlotData[,input$y])), aes(x = Var1, y = Freq, fill = Var2, label = Freq)) + geom_bar(stat = "identity") + geom_text(size = 3, position = position_stack(vjust = 0.5)) + labs(x = input$x, fill = input$y, y = "Count") + theme_minimal()
          }else if(input$x %in% factorVars & input$y %in% factorVars & input$col != "none"){
            ggplot(explorePlotData, aes_string(x = input$x, y = input$y)) + geom_jitter(aes_string(color = input$col)) + theme_minimal()
          }})
  
  ## Table for the data dictionary
  output$dataDict <- renderDataTable({cbind("Variable name" = sort(colnames(explore)), "Description" = c("Percent of district population aged 25 to 44", "Percent of district population aged 65 and over", 
                                                                  "Challenger from the opposite major party?", "Cook Political Report Partisan Voter Index", "Cross-pressured district? (Incumbent party opposite of district PVI)",
                                                                  "Change in district unemployment rate from previous election", "Democratic expenditures", "Difference between expenditures (Dem minus Rep)", "District", 
                                                                  "Democratic candidate two-party vote share", "Democratic swing house generic ballot polling average", "Democratic candidate victory?", "State + district", 
                                                                  "Incumbent ideology * Normalized Cook PVI", "Ideology effect normalized for Democratic candidate", "Incumbent party", "Incumbent's ideology (DW-NOMINATE)", 
                                                                  "Difference between expenditures (incbument minus non-incumbent)", "Incumbent party expenditures", "Incumbent party's two-party vote share", 
                                                                  "Incumbent party victory?", "Log-transformed difference between expenditures (Dem minus Rep)", "Log-transformed Democratic expenditures", 
                                                                  "Log-transformed incumbent party expenditures", "Log-transformed non-incumbent party expenditures", "Log-transformed Republican expenditures", 
                                                                  "District median household income", "Midterm and President's party indicator", "Midterm election year?", "Midterm (normalized based on incumbent and President's party)",
                                                                  "Expenditures by the non-incumbent party", "Open election?", "Open election and incumbent party indicator", 
                                                                  "Expenditures by third party candidates", "Third party vote share", "Percent of district population below poverty level", "Percent change in district median household income",
                                                                  "Democratic house generic ballot polling average margin (2018) or Democratic national vote share margin (all other years)", "President and incumbent in the same party?", "President's party",
                                                                  "Democrat's vote share in distict's second-most recent election", "Democrat's vote share margin in distict's second-most recent election",
                                                                  "Democrat's vote share in distict's previous election", "Democrat's vote share margin in distict's previous election",
                                                                  "Swing in Democratic vote share margin from previous two elections", "District's Democratic Presidential vote share from previous election", 
                                                                  "District's incumbent party Presidential vote share from previous election", "District's Republican Presidential vote share from previous election",
                                                                  "Incumbent-normalized Cook Political Report PVI", "Percent of district population Indian/Alaska Native", "Percent of district population Asian", 
                                                                  "Percent of district population Black/African American", "Percent of district population Hispanic/Latino", "Percent of district population Hawaiian/Pacific Islander",
                                                                  "Percent of district population some other race", "Percent of district population two or more races", "Percent of district population White",
                                                                  "Replican expenditures", "Republican candidate two-party vote share", "Sabato's Crystal Ball rating", "Numbered Sabato rating", "Incumbent-normalized numbered Sabato rating",
                                                                  "State", "Terms of Democratic party control in district", "Terms of Republican party control in district", "Third party candidate?", "District unemployment rate",
                                                                  "Weighted combination of district's Presidential vote shares from two previous elections", "Winning party", "Election year", "Years of incumbent party control"),
                                            "Source" = c("Census Bureau", "Census Bureau", "CLEA, Cook Political Report", "Cook Political Report", "NA", "Census Bureau", "FEC", "FEC", "NA", 
                                                              "CLEA, Cook Political Report", "Real Clear Politics, Fivethirtyeight", "CLEA, Cook Political Report", "NA", 
                                                              "Voteview, Cook Political Report", "Cook Political Report", "FEC, Politico", "Voteview", 
                                                              "FEC", "FEC", "CLEA, Cook Political Report", 
                                                              "NA", "FEC", "FEC", 
                                                              "FEC", "FEC", "FEC", 
                                                              "Census Bureau", "NA", "NA", "NA",
                                                              "FEC", "DailyKos", "DailyKos", 
                                                              "FEC", "CLEA, Cook Political Report", "Census Bureau", "Census Bureau",
                                                              "Fivethirtyeight, Real Clear Politics", "NA", "NA",
                                                              "CLEA", "CLEA",
                                                              "CLEA", "CLEA",
                                                              "CLEA", "DailyKos", 
                                                              "DailyKos", "DailyKos",
                                                              "Cook Political Report", "Census Bureau", "Census Bureau", 
                                                              "Census Bureau", "Census Bureau", "Census Bureau",
                                                              "Census Bureau", "Census Bureau", "Census Bureau",
                                                              "FEC", "CLEA, Cook Political Report", "Sabato's Crystal Ball", "Sabato's Crystal Ball", "Sabato's Crystal Ball",
                                                              "NA", "CLEA", "CLEA", "CLEA, Cook Political Report", "Census Bureau",
                                                              "DailyKos", "CLEA, Cook Political Report", "NA", "CLEA"))})

  # Download data button
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("houseElections-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(explore, file)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
