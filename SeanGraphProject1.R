


df <- read.csv("complete.summary.d14.csv")

library(shiny)
library(ggplot2)

df.filter <- df[df$Media == "NPSR" & df$Sample == 1,]
df.filter2 <- df.filter[,6:7]

#ggplot(df.filter2, aes(Gene, Rel.Expression)) +
 # geom_point()
  

ui <- fluidPage(
  headerPanel('Gene analysis'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(df.filter)),
    #selectInput('ycol', 'Y Variable', names(df.filter),
      #selected = names(df.filter)[[7]])
  
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {

  #selectedData <- reactive({
  #  df.filter[, c(input$xcol, input$ycol)]
  #})
  
  
  output$plot1 <- renderPlot({
    ggplot(df.filter) +
      geom_point(aes(input$xcol, Rel.Expression))
  })

}

shinyApp(ui = ui, server = server)
