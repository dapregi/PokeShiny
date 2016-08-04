library(shiny)
library(ggplot2)

shinyUI(
  fluidPage(
    # Some custom CSS
    tags$head(
      tags$style(HTML(".option-header {
                      font-size: large;
                      text-decoration: underline;
                      text-transform: uppercase;
                      margin-bottom: 10px;
                      }"),
                 HTML(".tab-pane{padding-top: 20px;}")
                 )
      ),
    
    titlePanel("Data"),
    br(),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          'input.dataset === "Exploration"',
          div(class = "option-header", "Fields"),
          helpText("Select the data you want to show"),
          uiOutput("field_checkbox"),
          actionButton("show_fields", "Show selected fields"),
          hr(),
          div(class = "option-header", "Filters"),
          uiOutput("filters"),
          hr(),
          div(class = "option-header", "Download"),
          helpText("Export your filtered data table"),
          radioButtons("output_format", "File format:",
                       choices = c("csv", "tsv")),
          downloadButton('downloadData', 'Download')
          ),
        conditionalPanel(
          'input.dataset === "Scatter Plot"',
          selectInput("x", "X-axis:", c("hp", "attack", "defense", "weight", "height"), "weight"),
          selectInput("y", "Y-axis:", c("hp", "attack", "defense", "weight", "height"), "height"),
          checkboxInput("type", "Plot by Type"),
          checkboxInput("regression", "Line regression")
          ),
        conditionalPanel(
          'input.dataset === "Histogram"',
          selectInput("feature", "Feature:", c("hp", "attack", "defense", "weight", "height"), "hp"),
          selectInput("breaks", "Bin width:", c(5, 10, 20, 35, 50), 10),
          selectInput("aes", "Misc:", c("None", "Density line", "Colour by Type", "Colour by Egg Group"), "None")
          )
        ),
      
      mainPanel(
        tabsetPanel(
          id='dataset',
          tabPanel("Documentation", includeMarkdown("./documentation.Rmd")),
          tabPanel("Exploration", DT::dataTableOutput("data_table")),
          tabPanel("Scatter Plot", plotOutput("scatterplot")),
          tabPanel("Histogram", plotOutput("histogram"))
          )
        )
      )
    )
  )
