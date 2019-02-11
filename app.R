# Prognosticator
# This is the front end of the prognosticator text prediction function. This will allow the user to make predictions in the English Language. For the free Shinyapps plan, the maximum bundle size is 1 GB, so the function and associated training material may need to be truncated to meet this criterion. 


library(shiny)
source("Prognosticator.R")

# Define UI for application that opens a text string input
ui <- fluidPage(
   
   # Application title
   titlePanel("Prognosticator - A Text Prediction Application"),
   
   # Sidebar opens the String Input and provides brief introduction.  
   mainPanel(
         p("Prognosticator will predict the next word in your phrase after you press 'Predict'"), 
         textInput("phrase", "", value = "Text", width = '1000px'),
         submitButton(text = "Predict", width = NULL),
         dataTableOutput('table')
         )
     
   )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$phrase <- renderText({input$phrase})
   output$table <- renderDataTable(Prognosticate(input$phrase), 
        options = list(
            paging = FALSE,
            searching = FALSE
        )    
   )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

