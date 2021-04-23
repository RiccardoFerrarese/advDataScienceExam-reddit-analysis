

library(shiny)


# in this function call ui modelu with id and namespace
# shiny_func( id, "ns"::output$ns)

pageUI <- function(id) {
   tagList(
      navbarPage(
         title = "Reddit App",
         id = "navBarID",
         tabPanel(
            title = "About",
            mainPanel(about())
         ),
         tabPanel(
            title = "DogeCoin",
            value = "doge",
            fluidRow(
               textServer(NS(id, "text"))
            )
         ),
         tabPanel(
            title = "Ethereum",
            value = "eth",
            fluidRow(
               textServer(NS(id, "text"))
            )
         )
         # tabPanel(title = "PancakeSwap",
         #          value ='4',
         #          mainPanel(
         #              ns("text"),
         #              ns("plot")
         #          ))
      )
   )
}




about <- function() {
   fillPage(
      tags$div(
         class = "header", checked = NA,
         style = "width: 100%; height: 100%;",
         tags$img(src = "S~/ShinyApp/Reddit-app/logo.jpeg", width = "100px", height = "100px"),
         tags$p("My app"),
         tags$a(href = "https://github.com/RiccardoFerrarese/advDataScienceExam-reddit-analysis", "Click Here!")
      )
   )
}

### module for about
### module for other body
## DogeCoin call general plot func with its data
## Eth call general plot func with its data
## ecc..
