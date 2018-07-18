#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# need to adjust the segment identity to more/less specific regions
library(shiny)
library(dplyr)

secondaries <- readxl::read_excel("C:/Users/sbwil/Documents/R/Shiny/ShinyProject1/testdb.xlsx", sheet = 2)
primaries <- readxl::read_excel("C:/Users/sbwil/Documents/R/Shiny/ShinyProject1/testdb.xlsx", sheet = 1)
identity <- readxl::read_excel("C:/Users/sbwil/Documents/R/Shiny/ShinyProject1/testdb.xlsx", sheet = 3)
# generate vector string of segment identities
identities <- unique(identity$IDENTITY)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Antibody database"),
   
   # Sidebar with a slider input for species of primary 
   sidebarLayout(
     
      sidebarPanel(
        
         selectInput(inputId = "ident1",
                     label =  "Segment/Identity/Region #1",
                     choices = identities)
         

      ),
      
      
      # Show a table of the antibodies compatable with input
      mainPanel(
        textOutput("ident1"),
        tableOutput("ab1")
      )
   )
)
   
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # produce a table of antibodies that will mark selected segment
  identityInput <- reactive({
    identity$GENE <- toupper(identity$GENE)
    identity %>% filter(IDENTITY == input$ident1) %>% arrange(GENE)
  })
  primariesInput <- reactive({
    primaries$GENE <- toupper(primaries$GENE)
    primaries
  })
  
  # output the segment as a text header
  output$ident1 <- renderText({
    input$ident1
  })

  # Generate a table of the primaries of selected species ----
  output$ab1 <- renderTable({
    inner_join(identityInput(), primariesInput(), by = "GENE")
    
  })

  
}  
# Run the application 
shinyApp(ui = ui, server = server)

