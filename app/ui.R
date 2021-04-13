
library(shiny)
library(bslib)

myModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(title = "Reddit App",
               id = "inNavBar",
             
               tabPanel(title='About', 
                        value ='1', 
                        mainPanel(
                          ns("text"), 
                          ns("plot")
                        )),
               tabPanel(title = "DogeCoin", 
                        value ='2', 
                        mainPanel(
                          ns("text"), 
                          ns("plot")
                        )),
               tabPanel(title = "ETH", 
                        value ='3', 
                        mainPanel(
                          ns("text"), 
                          ns("plot")
                        )),
               tabPanel(title = "WallStreeBeats", 
                        value ='4', 
                        mainPanel(
                          ns("text"), 
                          ns("plot")
                        ))
               )
  )
               
}




 ### module for about
 ### module for other body 
## DogeCoin call general plot func with its data
## Eth call general plot func with its data
## ecc.. 
