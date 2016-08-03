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
          div(class = "option-header", "Filters"),
          uiOutput("hp_threshold"),
          uiOutput("attack_threshold"),
          uiOutput("defense_threshold"),
          downloadButton('downloadData', 'Download'),
          helpText("Download your filtered data table")
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
          tabPanel("Exploration", dataTableOutput("data_table")),
          tabPanel("Scatter Plot", plotOutput("scatterplot")),
          tabPanel("Histogram", plotOutput("histogram"))
          )
        )
      )
    )
  )
