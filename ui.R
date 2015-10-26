
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("ACO Quality Measures, ACOs with patients in CT, MA, RI"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("measure",
                   label = "Choose Measure:",
                   choices = avail.measures,
                   selected = avail.measures[1]),
      tags$hr(),
      #(tags$b("Description:"), "there are two tabs, Plots and Tables. Choose a measure group and both will update automatically. This app leverages the ", a("Socrata Open Data API", href = "https://data.medicare.gov/developers"), " so it will always display the most recent data available."),
      #br(),
      p("Data Source: ", a("CMS Physician Compare", href = "https://www.medicare.gov/physiciancompare/aco/search.html")),
      #p("Code: ", a("Github Repository", href = "https://github.com/andylytics/hospital_compare")),
      p("Created by: ", a("Andy Rosa", href = "https://www.linkedin.com/pub/andrew-rosa/99/787/a64"))
      
#       selectInput("mgroup",
#                   label = "Select Measure Group:",
#                   choices = mchoices,
#                   selected = mchoices[1])
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Plots",
                 p(),
                 textOutput("txtdesc"),
#                  if (j == 1){
#                    plotOutput("p1", height = "473", width = "400")
#                  }
#                  else {
#                    plotOutput("p2", height = "300", width = "500")
#                  },
#                  
#                  
#                  
#                  if (j == 2) {
#                    plotOutput("p1", height = "300", width = "500")
#                  }
#                  
#                  else {
#                    plotOutput("p2", height = "300", width = "500")
#                  }
                 plotOutput("p1", height = 600, width = 600)
                 #plotOutput("p2", height = ht2, width = wd2)

        ),
        tabPanel("Tables",
                 #h4(tabtext),
                 tableOutput("table"))
        #tabPanel("Measure Info", includeMarkdown("include.md"))
      )
    )
  )
))
