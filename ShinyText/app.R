#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# first run: make a lookup table that shows corresponding secondaries
# to primary species
library(shiny)
library(dplyr)
dataset <- readxl::read_excel("C:/Users/sbwil/Documents/R/Shiny/ShinyProject1/testdb.xlsx", sheet = 2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Antibody database"),
   
   # Sidebar with a slider input for species of primary 
   sidebarLayout(
     
      sidebarPanel(
        
         selectInput(inputId = "species",
                     label = "Which primary species?",
                     choices = c("Rabbit", "Mouse", "Goat", "Sheep",
                                 "Chicken", "Biotin"))
      ),
      
      # Show a table of the antibodies compatable with input
      mainPanel(
         tableOutput("table")
      )
   )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  
  datasetInput <- reactive({
    
    dataset %>% filter(SPECIES_REACTIVITY == input$species)
  })
  
  # Generate a table of the dataset ----
  output$table <- renderTable({
    dataset <- datasetInput()
    
  })
  
  # Show the first "n" observations ----
}  
# Run the application 
shinyApp(ui = ui, server = server)

