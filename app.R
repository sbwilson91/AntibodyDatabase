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



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Graphs"),
   
   # Select Box for the X axis
   selectInput("selectx", label = h3("Select box"), 
               choices = list("Choice 1" = 1 , "Choice 2" = 2,
                              "Choice 3" = 3), 
               selected = 1),
   
  
   
   
   hr(),
   fluidRow(column(3, verbatimTextOutput("valuex"))),
   
   # Select Box for the Y axis
   selectInput("selecty", label = h3("Select box"), 
               choices = list("Choice 1" = 1, "Choice 2" = 2,
                              "Choice 3" = 3), 
               selected = 1),
   
   
   
   
   hr(),
   fluidRow(column(3, verbatimTextOutput("valuey"))),
  

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$valuex <- renderPrint({ input$selectx })
  output$valuey <- renderPrint({ input$selecty })
  
  output$distPlot <- renderPlot({
    plot(x=input$selectx, y=input$selecty)
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

