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
primaries <- readxl::read_excel("E:/Antibody Database-MCRI.xlsx", sheet = 1) %>% filter(!is.na(MCRI_ID))

secondaries <- readxl::read_excel("E:/Antibody Database-MCRI.xlsx", sheet = 2) %>%
  transmute(MCRI_ID = MCRI_ID,
            UQ_ID = UQ_ID,
            SOURCE = SOURCE,
            SPECIES_REACTIVITY = SPECIES_REACTIVITY,
            CONJUGATE = CONJUGATE)

imb <- readxl::read_excel("E:/IMB Complete Antibody Inventory 2014.xlsx", sheet = 1) %>% filter(!is.na(UQ_ID))



combined <- right_join((primaries %>% transmute(MCRI_ID = MCRI_ID,
                                                UQ_ID = UQ_ID,
                                                Description = Description,
                                                GENE = GENE,
                                                Immunogen = Immunogen,
                                                Supplier = Supplier,
                                                CAT_NO = CAT_NO,
                                                SOURCE = SOURCE
)),
(imb %>% transmute(UQ_ID = UQ_ID,
                   Description = Description,
                   GENE = GENE,
                   Immunogen = Immunogen,
                   Supplier = Supplier,
                   CAT_NO = CAT_NO,
                   SOURCE = SOURCE)), 
by = c("UQ_ID", "GENE", "CAT_NO","SOURCE")) %>% filter(!is.na(GENE)) %>% 
  transmute(MCRI_ID = MCRI_ID,
            UQ_ID = UQ_ID,
            GENE = GENE,
            SOURCE = SOURCE)

combined$GENE <- toupper(combined$GENE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Antibody database"),
   
   # Sidebar with a slider input for species of primary 
   sidebarLayout(
     
      sidebarPanel(
        
        
         textInput(inputId = "antibody1",
                   label =  "Which antibody?",
                   placeholder = "eg. Six2"),
         
         textInput(inputId = "antibody2",
                   label =  "Which antibody?",
                   placeholder = "eg. Six2"),
         
         textInput(inputId = "antibody3",
                   label =  "Which antibody?",
                   placeholder = "eg. Six2"),
         
         textInput(inputId = "antibody4",
                   label =  "Which antibody?",
                   placeholder = "eg. Six2"),
         
         selectInput(inputId = "species",
                     label = "Which primary species?",
                     choices = c("Rabbit", "Mouse", "Goat", "Sheep",
                                 "Chicken", "Biotin"))
      ),
      
      
      # Show a table of the antibodies compatable with input
      mainPanel(
         tableOutput("prim1"),
         tableOutput("prim2"),
         tableOutput("prim3"),
         tableOutput("prim4"),
         tableOutput("sec")
      )
   )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  
  primInput1 <- reactive({
    combined %>% filter(GENE == toupper(input$antibody1))
  })
  
  primInput2 <- reactive({
    combined %>% filter(GENE == input$antibody2)
  })
  
  primInput3 <- reactive({
    combined %>% filter(GENE == input$antibody3)
  })
  
  primInput4 <- reactive({
    combined %>% filter(GENE == input$antibody4)
  })
  
  secInput <- reactive({
    secondaries %>% filter(SPECIES_REACTIVITY == input$species) %>% 
      arrange(CONJUGATE)
  })
  
  # Generate a table of the primaries of selected species ----
  output$prim1 <- renderTable({
    prim <- primInput1()
  })
  output$prim2 <- renderTable({
    prim <- primInput2()
  })
  output$prim3 <- renderTable({
    prim <- primInput3()
  })
  output$prim4 <- renderTable({
    prim <- primInput4()
  })
  # Generate a table of the secondaries reactive to selective species ----
  output$sec <- renderTable({
    sec <- secInput()
  })
}  
# Run the application 
shinyApp(ui = ui, server = server)

