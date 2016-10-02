library(shiny)
library(datasets)
library(plyr)
library(dplyr)
library(ggplot2)

features<- read.csv("store_features.csv")
jf <- features[,4:length(features)]
products <- read.csv("product_attributes.csv")
pca<-prcomp(jf, retx=FALSE)
tbpca <-tbl_df(pca[[2]])
top2 <- tbpca[, 1:2]
pcaseach<-as.matrix(features[,4:length(features)]) %*% as.matrix(top2)


ui <- fluidPage(
  navlistPanel(
    tabPanel("Clusters", 
    tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")),
    tags$head(tags$script(src="cdp.js")),
    #includeCSS("www/style.css"),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),
    headerPanel('Store Clusters'),
    sidebarPanel(
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
      #tags$img(src = "/rplt.png")
    )
  ),
  tabPanel("Correlated Products", 
           mainPanel(
             #plotOutput('plot1')
             tags$img(src = "/rplt.png", class="imghack")
           )
  )
)
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    rnorm(input$num)
  })
  
  
  clusters <- reactive({
    kmeans(pcaseach, input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(pcaseach,
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$hist <- renderPlot({ hist(data())
  })
  
}

shinyApp(ui = ui, server = server)
