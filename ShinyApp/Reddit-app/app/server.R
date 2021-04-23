library(shiny)
library(DT)
library(ggplot2)

#  reactive sources are accessible through the input object
# and reactive endpoints are accessible through the output object

# Define server logic for change navbar
textServer <- function(id, df){
   
   moduleServer(id, function(input, output, session) {
      rv <- reactiveValues()
      observeEvent(input$navBarID, {
         rv$tab <- input$navBarID
         updateTabsetPanel(session, "inNavBar",
                           selected = input$navBarID
         )
      })
      
      output$text <-
         output$text2 <-
         renderText({
            paste0("You are viewing tab \"", rv$tab, "\"")
         })
   })
}
