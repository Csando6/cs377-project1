library(shiny)
library(shinydashboard)
library(lubridate)
library(leaflet)
library(stringr)
library(ggplot2)
library(data.table)
library(plyr)
library(DT)

data <- read.csv(file="litterati challenge-65.csv",header=TRUE, stringsAsFactors = FALSE)
data$litterTimestamp <- ymd_hms(data$litterTimestamp)
data$litterTimestamp <- with_tz(data$litterTimestamp, "America/Chicago")

# Cleaning up my data
data <-  data[!( abs(data$lat - 41) > 10) & !(abs(data$lon+87) > 10), ]
data$tags[data$tags==""]<-"untagged"
data$tags <- str_split(data$tags, pattern = ",")
data$tagCount <- lengths(data$tags)

data2 <- data
#data2$tagCount <- lengths(data2[,7])
#fix, need to clean up tags, they have " " and uppercase

# Gets num of total trash picked up
total <- sum(data2$tagCount)

# Creates list of top 10 trash pickers by user in descending order
data3 <- aggregate(data2$tagCount, by=list(Category=data2$username), FUN=sum)
data3 <- data3[order(data3$x, decreasing = TRUE),]
data3 <- data3[c(1:10),]

# Creates list of trash picked by date and amount, in decreasing order
data4 <- aggregate(data2$tagCount, by=list(Category=date(data2$litterTimestamp)), FUN=sum )
data4 <- data4[order(data4$Category),]

# Creates list of trash picked by day and amount, in decreasing order
data5 <- aggregate(data2$tagCount, by=list(Category=wday(data2$litterTimestamp)) , FUN=sum )
data5 <- data5[order(data5 $Category, decreasing = FALSE),]
# sun -> 1 .. sat -> 7

# Creates list of trash picked by hour of day and amount, in decreasing order
data6 <- aggregate(data2$tagCount, by=list(Category=hour(data2$litterTimestamp)), FUN=sum )
data6 <- data6[order(data6$Category),]

# Creates list of trash picked by tags
tagString <- unlist(data2$tags, recursive = TRUE)
#tagString <- as.list(tolower(tagString))
#tagString <- gsub(" ","",tagString)
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
      selectInput("User", "Select Top 10 User", append("All",data3$Category) ,selected = "All" ),
      selectInput("Tag", "Select Top 10 Tags", append("All",as.character(data7$Label) ), selected = "All")
      
    )
  ),
  dashboardBody(
    fluidRow(
      column(3,
        fluidRow(
          box(title="first data", solidHeader = TRUE, status="primary", width=12,
            textOutput("plot1")
          ),
          box(title="first data", solidHeader = TRUE, status="primary", width=12,
              dataTableOutput("plot2", height=300)
          )
        )
      ),
      column(5,
        fluidRow(
          box(title="Map of stuff",solidHeader = TRUE, status="primary",width=12,
            leafletOutput("mymap",width = "100%",height = 300)
          ),
          box(title="Map of stuff",solidHeader = TRUE, status="primary",width=12,
              plotOutput("plot4", height=300)
          )
        )
      ),
      column(4,
        fluidRow(
          box(title="first data", solidHeader = TRUE, status="primary", width=12,
              plotOutput("plot5", height=225)
          ),
          box(title="first data", solidHeader = TRUE, status="primary", width=12,
              plotOutput("plot6", height=225)
          ),
          box(title="first data", solidHeader = TRUE, status="primary", width=12,
              plotOutput("plot7", height=225)
          )
        )   
      )
    )
  )
)

server <- function(input, output){
  #data1React <- reactive()
  #data2React <- reactive()
  #data3React <- reactive() #should not change
  data4React <- reactive(if(input$User == "All" & input$Tag == "All"){
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
      data5 <- aggregate(data2$tagCount, by=list(Category=wday(data2$litterTimestamp)) , FUN=sum )
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
      data6 <- aggregate(data2$tagCount, by=list(Category=hour(data2$litterTimestamp)), FUN=sum )
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
  #data7React <- reactive()
  totalReact <- reactive(if(input$User == "All" & input$Tag == "All"){
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
    ggplot(data=data5, aes(x=Category, y=x)) + geom_bar(stat="identity", fill="blue") +
      xlab("Day of week") + ylab("Amount of Trash")
  })
  output$plot7 <- renderPlot(
    ggplot(data=data7, aes(x=Label, y=Freq)) + geom_bar(stat="identity", fill="blue") +
      xlab("Trash Type") + ylab("Amount of Trash")
  )
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = data2$lon, lat = data2$lat, clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui = ui, server = server)