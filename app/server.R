library(shiny)
library(shinydashboard)




# Define server logic required to draw a histogram
server <- shinyServer( function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "inTabset",
                      selected = paste0("panel", input$controller)
    )
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$menuitem <- renderMenu(
    menuItem("Menu Item", icon = ("calendar"))
  )
  
  
}) # server
