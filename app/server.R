library(shiny)
library(ggplot2)



# Define server logic for change navbar
myModuleServer <- function(id, input, output, sessione) {
  # define module server
  moduleServer(
    id, 
    function(input, output, session){
      # event for change tab 
      observeEvent(input$controller, {
        updateTabsetPanel(session, "inNavBar",
                          selected =  input$controller
        )
      })
    
    
    dataset <- reactive({
      ggplot2::diamonds[sample(nrow(ggplot2::diamonds), input$sampleSize),]
    })
    
    output$text <- renderPlot({
      verbatimTextOutput(input$controller)
    })
  
    output$plot <- renderPlot({
      ggplot2::ggplot(dataset(), ggplot2::aes(x = input$x, y = input$y)) +
        ggplot2::geom_point()
  })
  #return(dataset)
  
})}# server
