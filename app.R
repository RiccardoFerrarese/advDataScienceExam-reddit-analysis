#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("./app/server.R")
source("./app/ui.R")

server <- shinyServer(function(input, output, session){
   callModule(myModuleServer, "main")
})

ui <- shinyUI(
   fluidPage(
      ## define theme 
      theme = bs_theme(bootswatch = 'materia'),
      ## define css of navbar
      tags$head(
         tags$style(type = 'text/css', 
                    HTML('.navbar { background-color: white;}
                            .navbar-default .navbar-brand{color: white;}
                            .tab-panel{ background-color: red; color: white}
                            .navbar-default .navbar-nav > .active > a, 
                             .navbar-default .navbar-nav > .active > a:focus, 
                             .navbar-default .navbar-nav > .active > a:hover {
                                  color: #555;
                                  background-color: rgba(0, 255, 0, 0.1);
                              }')
         )
         
      ),
      
      
      ## call page function 
      myModuleUI("main"),
      ## footer 
      tags$footer("by Riccardo Ferrarese", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:40px;   /* Height of the footer */
              color: black;
              padding: 10px; 
              background-color: rgb(234, 234, 234, 0.5);
              z-index: 1000;")
   ))


shinyApp(ui=ui, server=server)
