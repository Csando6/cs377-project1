library(shiny)
library(shinydashboard)
library(lubridate)
library(leaflet)
library(stringr)
library(ggplot2)
library(data.table)
library(plyr)
library(DT)

# fixing data time stamp
data <- read.csv(file="litterati challenge-65.csv",header=TRUE, stringsAsFactors = FALSE)
data$litterTimestamp <- ymd_hms(data$litterTimestamp)
data$litterTimestamp <- with_tz(data$litterTimestamp, "America/Chicago")

# Cleaning up my data
data <-  data[!( abs(data$lat - 41) > 10) & !(abs(data$lon+87) > 10), ]
data$tags[data$tags==""]<-"untagged"
data$tags <- str_split(data$tags, pattern = ",")
data$tagCount <- lengths(data$tags)

# Creates list of top 10 trash pickers by user in descending order
data3 <- aggregate(data$tagCount, by=list(Category=data$username), FUN=sum)
data3 <- data3[order(data3$x, decreasing = TRUE),]
data3 <- data3[c(1:10),]

# Creates list of trash picked by tags
tagString <- unlist(data$tags, recursive = TRUE)
data7 <- as.data.frame(table(tagString, dnn=list("Label") ), responseName = "Freq")
data7 <- data7[order(data7$Freq, decreasing = TRUE),]
data7 <- data7[c(1:10),]



ui <- dashboardPage(
  dashboardHeader(title="Project 1"),
  dashboardSidebar(disable=FALSE, collapsed=FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     selectInput("User", "Select Top 10 User", append("All",data3$Category) ,selected = "All" ),
                     selectInput("Tag", "Select Top 10 Tags", append("All",as.character(data7$Label) ), selected = "All"),
                     actionButton("show","Show")
                   )
  ),
  dashboardBody(
    fluidRow(
      column(3,
             fluidRow(
               box(title="Total count:", solidHeader = TRUE, status="primary", width=12,
                   textOutput("plot1")
               ),
               box(title="Top 10:", solidHeader = TRUE, status="primary", width=12,
                   dataTableOutput("plot2", height=600)
               )
             )
      ),
      column(5,
             fluidRow(
               box(title="Map",solidHeader = TRUE, status="primary",width=12,
                   leafletOutput("mymap",width = "100%",height = 800)
               ),
               box(title="Trash picked Up by Day of Year",solidHeader = TRUE, status="primary",width=12,
                   plotOutput("plot4", height=500)
               )
             )
      ),
      column(4,
             fluidRow(
               box(title="Trash picked Up byHour of Day", solidHeader = TRUE, status="primary", width=12,
                   plotOutput("plot5", height=400)
               ),
               box(title="Trash picked up by day of week", solidHeader = TRUE, status="primary", width=12,
                   plotOutput("plot6", height=400)
               ),
               box(title="Total Trash picked up by Tag", solidHeader = TRUE, status="primary", width=12,
                   plotOutput("plot7", height=400)
               )
             )   
      )
    )
  )
)

server <- function(input, output){
  data4React <- reactive(
    if(input$User == "All" & input$Tag == "All"){
      temp4 <- aggregate(data$tagCount, by=list(Category=date(data$litterTimestamp)), FUN=sum )
    }else if(input$Tag == "All"){
      temp4 <- data[data$username == input$User,]
      temp4 <- aggregate(temp4$tagCount, by=list(Category=date(temp4$litterTimestamp)), FUN=sum )
    }else if(input$User=="All"){
      temp4 <- data[which(input$Tag == data$tags), ]
      temp4 <- aggregate(temp4$tagCount, by=list(Category=date(temp4$litterTimestamp)), FUN=sum )
    }
    else{
      temp4 <- data[data$username == input$User,]
      temp4 <- temp4[which(input$Tag == temp4$tags), ]
      temp4 <- aggregate(temp4$tagCount, by=list(Category=date(temp4$litterTimestamp)), FUN=sum )
    })
  
  data5React <- reactive(
    if(input$User == "All" & input$Tag == "All"){
      data5 <- aggregate(data$tagCount, by=list(Category=wday(data$litterTimestamp)) , FUN=sum )
    }
    else if(input$Tag == "All"){
      temp5 <- data[data$username == input$User,]
      data5 <- aggregate(temp5$tagCount, by=list(Category=wday(temp5$litterTimestamp)) , FUN=sum )
    }
    else if(input$User=="All"){
      temp5 <- data[which(input$Tag == data$tags), ]
      data5 <- aggregate(temp5$tagCount, by=list(Category=wday(temp5$litterTimestamp)) , FUN=sum )
    }
    else{
      temp5 <- data[data$username == input$User,]
      temp5 <- temp5[which(input$Tag == temp5$tags), ]
      data5 <- aggregate(temp5$tagCount, by=list(Category=wday(temp5$litterTimestamp)) , FUN=sum )
    }
  )
  data6React <- reactive(
    if(input$User == "All" & input$Tag == "All"){
      data6 <- aggregate(data$tagCount, by=list(Category=hour(data$litterTimestamp)), FUN=sum )
    }else if(input$Tag == "All"){
      temp6 <- data[data$username == input$User,]
      data6 <- aggregate(temp6$tagCount, by=list(Category=hour(temp6$litterTimestamp)), FUN=sum )
      
    }else if(input$User=="All"){
      temp6 <- data[which(input$Tag == data$tags), ]
      data6 <- aggregate(temp6$tagCount, by=list(Category=hour(temp6$litterTimestamp)), FUN=sum )
    }else{
      temp6 <- data[data$username == input$User,]
      temp6 <- temp6[which(input$Tag == temp6$tags), ]
      data6 <- aggregate(temp6$tagCount, by=list(Category=hour(temp6$litterTimestamp)), FUN=sum )
    }
  )
  data7React <- reactive(
    if(input$User == "All" & input$Tag == "All"){
      temp7 <- data
      tagString <- unlist(temp7$tags, recursive = TRUE)
      data7 <- as.data.frame(table(tagString, dnn=list("Label") ), responseName = "Freq")
      data7 <- data7[order(data7$Freq, decreasing = TRUE),]
      data7 <- data7[c(1:10),]
    }
    else if(input$Tag == "All") {
      temp7 <- data[data$username == input$User,]
      tagString <- unlist(temp7$tags, recursive = TRUE)
      data7 <- as.data.frame(table(tagString, dnn=list("Label") ), responseName = "Freq")
      data7 <- data7[order(data7$Freq, decreasing = TRUE),]
      data7 <- data7[c(1:10),]
    }
    else if(input$User == "All"){
      temp7 <- data[which(input$Tag == data$tags), ]
      tagString <- unlist(temp7$tags, recursive = TRUE)
      data7 <- as.data.frame(table(tagString, dnn=list("Label") ), responseName = "Freq")
      data7 <- data7[order(data7$Freq, decreasing = TRUE),]
      data7 <- data7[c(1:10),]
    }
    else{
      temp7 <- data[data$username == input$User,]
      temp7 <- temp7[which(input$Tag == temp7$tags), ]
      tagString <- unlist(temp7$tags, recursive = TRUE)
      data7 <- as.data.frame(table(tagString, dnn=list("Label") ), responseName = "Freq")
      data7 <- data7[order(data7$Freq, decreasing = TRUE),]
      data7 <- data7[c(1:10),]
    }
  )
  data8Reactive <- reactive({
    if(input$User == "All" & input$Tag == "All"){
      temp8 <- data
    }
    else if(input$Tag == "All"){
      temp8 <- data[data$username == input$User,]
    }
    else if(input$User=="All"){
      temp8 <- data[which(input$Tag == data$tags), ]
      
    }
    else{
      temp8 <- data[data$username == input$User,]
      temp8 <- temp8[which(input$Tag == temp8$tags), ]
    }
  })
  totalReact <- reactive(
    if(input$User == "All" & input$Tag == "All"){
      sum(data$tagCount)
    }else if(input$Tag == "All"){
      temp <- data[data$username == input$User,]
      sum(temp$tagCount)
    }else if(input$User=="All"){
      temp <- data[which(input$Tag == data$tags), ]
      length(temp[,1])
    }
    else{
      temp <- data[data$username == input$User,]
      temp <- temp[which(input$Tag == temp$tags), ]
      length(temp[,1])
    })
  output$plot1 <- renderText({
    totalR <- totalReact()
    paste("Total trash picked up: ",totalR)
  })
  output$plot2 <- renderDT(
    data.table(data=data3)
  )
  output$plot4 <- renderPlot({
    data4R <- data4React()
    ggplot(data=data4R, aes(x=Category, y=x)) + geom_bar(stat="identity", fill="blue")+
      xlab("Day of year") + ylab("Amount of Trash")
  })
  output$plot5 <- renderPlot({
    data6R <- data6React()
    ggplot(data=data6R, aes(x=Category, y=x)) + geom_bar(stat="identity", fill="blue") +
      xlab("Hour of Day") + ylab("Amount of Trash")
  })
  output$plot6 <- renderPlot({
    data5R <- data5React()
    ggplot(data=data5R, aes(x=Category, y=x)) + geom_bar(stat="identity", fill="blue") +
      xlab("Day of week") + ylab("Amount of Trash")
  })
  output$plot7 <- renderPlot({
    data7R <- data7React()
    ggplot(data=data7R, aes(x=Label, y=Freq)) + geom_bar(stat="identity", fill="blue") +
      xlab("Trash Type") + ylab("Amount of Trash")
  })
  output$mymap <- renderLeaflet({
    data8R <- data8Reactive()
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = data8R$lon, lat = data8R$lat, clusterOptions = markerClusterOptions())
  })
  observeEvent(input$show, {
    showNotification("cs377-project1\n
    Hello, this is my cs377 project1 for cs377.\n
    For this project, we had to take data collected from litterati.org and turn it into graphs and visualizations. This project is written in R and is hosted in a web app using shinny.\n
    In order to run this project, just type in runApp(app2.R) to launch the shinny app\n
    While doing this project, there was a lot of interesting data. In the 24 hour graph, that plotted how much trash was picked up by hours of the day, there was more trash picked up during\n
    4-8pm than in the morning. And for the 7 days of the week, the trash picked up did not follow any real pattern.")
  })
}

shinyApp(ui = ui, server = server)