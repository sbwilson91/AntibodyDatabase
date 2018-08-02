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
            ANTIGEN = GENE,
            CONJUGATE = CONJUGATE,
            #SUPPLIER = Supplier,
            #CAT_NO = CAT_NO,
            SOURCE = SOURCE)

secondaries <- readxl::read_excel("E:/Antibody Database-MCRI.xlsx", sheet = 2) %>%
  transmute(MCRI_ID = MCRI_ID,
            UQ_ID = UQ_ID,
            SOURCE = SOURCE,
            ANTIGEN = SPECIES_REACTIVITY,
            CONJUGATE = CONJUGATE)
secondaries$UQ_ID <- as.character(secondaries$UQ_ID)

imb <- readxl::read_excel("E:/IMB Complete Antibody Inventory 2014.xlsx", sheet = 1) %>% filter(!is.na(UQ_ID)) %>% 
  transmute(UQ_ID = UQ_ID,
            #DESCRIPTION = Description,
            ANTIGEN = GENE,
            #SUPPLIER = Supplier,
            #CAT_NO = CAT_NO,
            SOURCE = SOURCE,
            CONJUGATE = CONJUGATE) 

# merge the primary databases together (primaries and imb, noting imb contains secondaries as well)
combined1 <- full_join(primaries, imb, 
                        by = c("UQ_ID", "ANTIGEN", "SOURCE", "CONJUGATE")) %>% filter(!is.na(ANTIGEN)) %>% 
                          transmute(MCRI_ID = MCRI_ID,
                                    UQ_ID = UQ_ID,
                                    ANTIGEN = ANTIGEN,
                                    SOURCE = SOURCE,
                                    CONJUGATE = CONJUGATE)
combined2 <- full_join(secondaries, (imb %>% filter(!is.na(CONJUGATE))),
                       by = c("UQ_ID", "ANTIGEN", "SOURCE", "CONJUGATE"))


combined1$ANTIGEN <- toupper(combined1$ANTIGEN)
combined1$SOURCE <- toupper(combined1$SOURCE)
combined2$ANTIGEN <- toupper(combined2$ANTIGEN)
combined2$SOURCE <- toupper(combined2$SOURCE)


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
                                 "Chicken", "Biotin")),
         
         checkboxInput(inputId = "mcri",
                       label = "Show MCRI antibodies only?",
                       value = F)
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
    if(input$mcri == T) {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody1)) %>% filter(!is.na(MCRI_ID))
    } else {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody1))
    }
    
  })
  
  primInput2 <- reactive({
    if(input$mcri == T) {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody2)) %>% filter(!is.na(MCRI_ID))
    } else {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody2))
    }
    
  })
  
  primInput3 <- reactive({
    if(input$mcri == T) {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody3)) %>% filter(!is.na(MCRI_ID))
    } else {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody3))
    }
    
  })
  
  primInput4 <- reactive({
    if(input$mcri == T) {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody4)) %>% filter(!is.na(MCRI_ID))
    } else {
      combined1 %>% filter(ANTIGEN == toupper(input$antibody4))
    }
    
  })
  
  secInput <- reactive({
    combined2 %>% filter(ANTIGEN == toupper(input$species)) %>% 
      arrange(CONJUGATE)
    if (input$mcri == T){
      combined2 %>% filter(ANTIGEN == toupper(input$species)) %>% 
        arrange(CONJUGATE) %>% filter(!is.na(MCRI_ID))} else {
          combined2 %>% filter(ANTIGEN == toupper(input$species)) %>% 
            arrange(CONJUGATE)
        }
    
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

