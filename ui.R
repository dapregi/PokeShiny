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
          'input.dataset === "Documentation"',
          img(src="poke-ball.png", height = 50, width = 50)
          ),
        conditionalPanel(
          'input.dataset === "Exploration"',
          div(class = "option-header", "Fields"),
          helpText("Select the data you want to explore"),
          uiOutput("field_checkbox"),
          actionButton("show_fields", "Show selected fields"),
          hr(),
          div(class = "option-header", "Filters"),
          helpText("Apply different filters to select your desired data"),
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
          uiOutput("scatterplot_opts")
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
          tabPanel("Scatter Plot",
                   fluidRow(
                     column(width = 7,
                            plotOutput("scatterplot",
                                       hover =  hoverOpts(
                                         id = "scatterplot_hover",
                                         delay = 300,
                                         delayType = "throttle"))),
                     column(width = 5,
                            fluidRow(column(width = 4,
                                            imageOutput("sprite", height = "96px")),
                                     column(width = 8,
                                            uiOutput("info_name"))),
                            hr(),
                            fluidRow(column(width = 6,
                                            h4("Variable", align = "center"),
                                            verbatimTextOutput("info_variables")),
                                     column(width = 6,
                                            h4("Data", align = "center"),
                                            verbatimTextOutput("info_data"))))
                     )
                   ),
          tabPanel("Histogram", plotOutput("histogram"))
          )
        )
      )
    )
  )
