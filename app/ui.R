
library(shiny)
library(bslib)
library(shinydashboard)

body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItemOutput("menuitem")
))

dashboard <- dashboardPage(
  dashboardHeader(title="DataScience App"),
  
  sidebar, 
  body
)


sidebar_ <- sidebarPanel(
  box()
)

navbar <- navbarPage("Reddit App",
                     tabsetPanel(
                       tabPanel(title='About'),
                        tabPanel(title = "DogeCoin", sidebar_),
                        tabPanel(title = "ETH", body),
                        tabPanel(title = "WallStreeBeats")
                     )
)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
  theme = bs_theme(bootswatch = 'simplex'),
  navbar,
  
))
  