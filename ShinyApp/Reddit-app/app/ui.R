

library(shiny)
library(bslib)



pageUI <- function(){
   tagList(
      navbarPage(title = "Reddit App",
                 id = "navBarID",
                 
                 tabPanel(title='About', 
                          mainPanel(about())
                 ),
                 tabPanel(title = "DogeCoin", 
                          value ='doge', 
                              textOutput("text"), 
                              uiOutput("render1")
                          ),
                 tabPanel(title = "Ethereum",
                          value ='eth',
                              textOutput("text2"), 
                              uiOutput("render2")
                         )
                 # tabPanel(title = "PancakeSwap", 
                 #          value ='4', 
                 #          mainPanel(
                 #              ns("text"), 
                 #              ns("plot")
                 #          ))
                 
      ))
   
}


about <- function(){
   fillPage(

      tags$div(class = "header", checked = NA,
               style = "width: 100%; height: 100%;",
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
