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
         tableOutput("seg")
      )
   )
)
   
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # produce a table of antibodies that will mark selected segment
  ab1Input <- reactive({
    genes <- identity %>% filter(IDENTITY == "Cap Mesenchyme")#input$ident1)
    genes <- genes$GENE
    primaries <- na.omit(primaries)
    prim <- as.data.frame("")
    for (i in 1:nrow(primaries)){
      for (j in 1:length(genes)){
        if (as.character(primaries[i,3]) == genes[j]){
          print(genes[j])
          prim <- rbind(primaries[i,])
          print(prim)
        }
      }
    }
  })
  # database that displays that associated to the selected segment in input
  segInput <- reactive({
    seg <- identity %>% filter(IDENTITY == input$ident1)
  })

  # Generate a table of the primaries of selected species ----
  output$seg <- renderTable({
    seg <- segInput()
    
  })
  
  
}  
# Run the application 
shinyApp(ui = ui, server = server)

