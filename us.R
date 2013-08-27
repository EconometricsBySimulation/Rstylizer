library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("RStylizer"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    h4("Input Stata Code"),
    tags$textarea(id="input_code",width="100%", rows=10, ""),
    
    submitButton("Submit")

  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h6(htmlOutput("formatted")),
    h4("Generated HTML"),
    h6(htmlOutput("htmlformatted"))
  )
))
