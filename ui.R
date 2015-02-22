library(shiny)
library(ggplot2)

df <- read.delim("./pkmn_info.txt", header = TRUE)

shinyUI(fluidPage(
  titlePanel("Pokemon Data"),
  br(),
  sidebarLayout(
    sidebarPanel(
    conditionalPanel(
      'input.dataset === "Data Table"',
      sliderInput("hp_threshold",
                  label = "HP:",
                  min= 0,
                  max= max(df$hp, na.rm = TRUE),
                  value= c(0, max(df$hp, na.rm = TRUE)),
                  step=1, round=0),
      sliderInput("attack_threshold",
                  label = "Attack:",
                  min = 0,
                  max = max(df$attack, na.rm = TRUE),
                  value = c(0, max(df$attack, na.rm = TRUE)),
                  step=1, round=0),
      sliderInput("defense_threshold",
                  label = "Defense:",
                  min = 0,
                  max = max(df$defense, na.rm = TRUE),
                  value = c(0, max(df$defense, na.rm = TRUE)),
                  step=1, round=0),
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
      mainPanel(tabsetPanel(
        id='dataset',
        tabPanel("Data Table", dataTableOutput("data_table")),
        tabPanel("Scatter Plot", plotOutput("scatterplot")),
        tabPanel("Histogram", plotOutput("histogram"))
      )
    )
  )
))
