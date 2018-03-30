library(shiny)
library(ggplot2)
library(plotly)

# p16FIXhq_df is the name of the dataset
# datasetName <- load(file = "./p16FIXhq_df.RData")
load(file="./topNucFreqNoCode.RData")
load(file="./topNucFreqCode.RData")

# dataset <- p16FIXhq_df$POS

ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "Number of observations:",
                  min = 1,
                  max = 1000,
                  value = 500)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  
  # titlePanel("Chr20 Top Variants"),
  # 
  # mainPanel(
  #   plotlyOutput('plot')
  # )
)

server <- function(input, output) {
  # positionMap <- p16FIXhq_df[,c("POS","Coding")]
  # positionCount <- as.data.frame(table(positionMap))
  # posFreq <- positionCount[positionCount$Freq>0,]
  # posFreq$CodeStatus[posFreq$Coding==TRUE] <- "Coding"
  # posFreq$CodeStatus[posFreq$Coding==FALSE] <- "Non-Coding"
  # 
  # nucMapCode <- p16FIXhq_df[p16FIXhq_df$Coding==TRUE, c("ALT")]
  # nucCountCode <- as.data.frame(table(nucMapCode))
  # nucFreqCode <- nucCountCode[nucCountCode$Freq>0,]
  # orderedNucFreqCode <- nucFreqCode[order(-nucFreqCode$Freq),]
  # topNucFreqCode <- orderedNucFreqCode[1:35,]
  # 
  # nucMapNoCode <- p16FIXhq_df[p16FIXhq_df$Coding==FALSE, c("ALT")]
  # nucCountNoCode <- as.data.frame(table(nucMapNoCode))
  # nucFreqNoCode <- nucCountNoCode[nucCountNoCode$Freq>0,]
  # orderedNucFreqNoCode <- nucFreqNoCode[order(-nucFreqNoCode$Freq),]
  # topNucFreqNoCode <- orderedNucFreqNoCode[1:35,]
  
  
  # dataset <- reactive({
  #   smallPosFreq
  # })
  
  output$plot <- renderPlotly({
    
    # topNucFreqNoCode$nucMapNoCode <- factor(topNucFreqNoCode$nucMapNoCode, 
    #                                         levels = unique(topNucFreqNoCode$nucMapNoCode)
    #                                         [order(topNucFreqNoCode$Freq, decreasing = TRUE)])
    # 
    # topNucFreqCode$nucMapCode <- factor(topNucFreqCode$nucMapCode, 
    #                                         levels = unique(topNucFreqCode$nucMapCode)
    #                                         [order(topNucFreqCode$Freq, decreasing = TRUE)])
    # 
    # p1 <- plot_ly(data = topNucFreqNoCode, x = ~nucMapNoCode, y = ~Freq,
    #               width = 1000, height = 1000, name = 'Non-Coding')
    # 
    # p2 <- plot_ly(data = topNucFreqCode, x = ~nucMapCode, 
    #               y = ~Freq, width = 1000, height = 1000, name = 'Coding')
    # 
    # p <- subplot(p1, p2, nrows = 2)
    # ggplotly(p)
    
    # Expression that generates a plot of the distribution. The expression
    # is wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically 
    #     re-executed when inputs change
    #  2) Its output type is a plot 
    #
    # output$distPlot <- renderPlot({
    #   
    #   # generate an rnorm distribution and plot it
    #   dist <- rnorm(input$obs)
    #   hist(dist)
    # })
    
  })
  
}

shinyApp(ui = ui, server = server)