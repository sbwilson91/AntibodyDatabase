#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# load the libraries
library(shiny)
library(dplyr)

# load and adjust the databases for the various antibodies
primaries <- readxl::read_excel("E:/Antibody Database-MCRI.xlsx", sheet = 1) %>% filter(!is.na(MCRI_ID)) %>% 
  transmute(MCRI_ID = MCRI_ID,
            UQ_ID = UQ_ID,
            #DESCRIPTION = Description,
            GENE = GENE,
            #SUPPLIER = Supplier,
            CAT_NO = CAT_NO,
            SOURCE = SOURCE)

secondaries <- readxl::read_excel("E:/Antibody Database-MCRI.xlsx", sheet = 2) %>%
  transmute(MCRI_ID = MCRI_ID,
            UQ_ID = UQ_ID,
            SOURCE = SOURCE,
            SPECIES_REACTIVITY = SPECIES_REACTIVITY,
            CONJUGATE = CONJUGATE)

imb <- readxl::read_excel("E:/IMB Complete Antibody Inventory 2014.xlsx", sheet = 1) %>% filter(!is.na(UQ_ID)) %>% 
  transmute(UQ_ID = UQ_ID,
            #DESCRIPTION = Description,
            GENE = GENE,
            #SUPPLIER = Supplier,
            CAT_NO = CAT_NO,
            SOURCE = SOURCE)

# merge the primary databases together (primaries and imb, noting imb contains secondaries as well)
combined <- full_join(primaries, imb, 
                        by = c("UQ_ID", "GENE", "SOURCE", "CAT_NO")) %>% filter(!is.na(GENE)) %>% 
                          transmute(MCRI_ID = MCRI_ID,
                                    UQ_ID = UQ_ID,
                                    GENE = GENE,
                                    SOURCE = SOURCE)

primOnly <- primaries %>% filter(is.na(UQ_ID))
imbOnly <- anti_join(imb, combined,
                     by = c("UQ_ID"))
setdiff(intersect(primaries$UQ_ID, imb$UQ_ID),combined$UQ_ID)

combined$GENE <- toupper(combined$GENE)
secondaries$SPECIES_REACTIVITY <- toupper(secondaries$SPECIES_REACTIVITY)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Antibody database"),
   
   # Sidebar with a slider input for species of primary 
   sidebarLayout(
     
      sidebarPanel(
        
        
         textInput(inputId = "antibody1",
                   label =  "Antibody #1?",
                   placeholder = "eg. LTL"),
         
         textInput(inputId = "antibody2",
                   label =  "Antibody #2?",
                   placeholder = "eg. gata3"),
         
         textInput(inputId = "antibody3",
                   label =  "Antibody 3?",
                   placeholder = "eg. ecad"),
         
         textInput(inputId = "antibody4",
                   label =  "Antibody #4?",
                   placeholder = "eg. nphs1"),
         
         selectInput(inputId = "species",
                     label = "Raised against which species?",
                     choices = c("Rabbit", "Mouse", "Goat", "Sheep",
                                 "Chicken", "Biotin"))
      ),
      
      
      # Show a table of the antibodies compatable with input
      mainPanel(
        splitLayout(
          verticalLayout(
         tableOutput("prim1"),
         tableOutput("prim2"),
         tableOutput("prim3"),
         tableOutput("prim4")),
         tableOutput("sec"))
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
    combined %>% filter(GENE == toupper(input$antibody2))
  })
  
  primInput3 <- reactive({
    combined %>% filter(GENE == toupper(input$antibody3))
  })
  
  primInput4 <- reactive({
    combined %>% filter(GENE == toupper(input$antibody4))
  })
  
  secInput <- reactive({
    secondaries %>% filter(SPECIES_REACTIVITY == toupper(input$species)) %>% 
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

