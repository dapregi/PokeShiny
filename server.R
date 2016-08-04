library(shiny)
library(DT)
library(ggplot2)

df <- read.delim("./pkmn_info2.txt", header = TRUE)

shinyServer(
  function(input, output){
    
#     data_pkmn <- eventReactive(input$show_fields, {
#       if (length(input$field_checkbox) == 0) {
#         data.frame()
#       } else if (length(input$field_checkbox) == 1) {
#         df2 <- data.frame(df[, input$field_checkbox])
#         names(df2) <- input$field_checkbox
#         df2
#       } else {
#         df[, input$field_checkbox]
#       }
#     })
    
    fields <- eventReactive(input$show_fields, {input$field_checkbox})
    
    data_pkmn <- reactive({
      df2 <- df
      if (!is.null(input$hp) & "hp" %in% fields()) {
        df2 <- df2[input$hp[1] <= df2$hp & df2$hp <= input$hp[2], ]
      }
      if (!is.null(input$attack) & "attack" %in% fields()) {
        df2 <- df2[input$attack[1] <= df2$attack & df2$attack <= input$attack[2], ]
      }
      if (!is.null(input$defense) & "defense" %in% fields()) {
        df2 <- df2[input$defense[1] <= df2$defense & df2$defense <= input$defense[2], ]
      }
      df2[, fields()]
    })
    
    output$data_table <- DT::renderDataTable({
      data_pkmn()
    }, rownames = FALSE,
    options = list(lengthMenu = seq(10, 50, 10), pageLength = 10, 
                   orderClasses = TRUE))
    
    output$scatterplot <- renderPlot({
      a <- ggplot(data_pkmn(), aes_string(x = input$x, y = input$y))
      a <- a + geom_point()
      if (input$type) {
        a <- a + facet_wrap(~type)
      }
      if(input$regression) {
        a <- a + geom_smooth(method = 'lm', se = TRUE)
      }
      a
    })
    
    output$histogram <- renderPlot({
      b <- ggplot(data_pkmn(), aes_string(x = input$feature))
      if(input$aes == "Colour by Type") { 
        b <- b + aes(fill = type)
        b1 <- b + geom_histogram(binwidth = as.numeric(input$breaks))
      } else if (input$aes == "Colour by Egg Group"){
        b <- b + aes(fill = egg_group)
        b1 <- b + geom_histogram(binwidth = as.numeric(input$breaks))
      } else if (input$aes == "Density line") {
        b <- b + geom_histogram(aes(y = ..density..), binwidth = as.numeric(input$breaks))
        b1 <- b + geom_density()
      } else {
        b1 <- b + geom_histogram(binwidth = as.numeric(input$breaks))
      }
      b1
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('pokemon-filtered-', Sys.Date(), '.', input$output_format)
      },
      content = function(file) {
        switch (input$output_format,
                "csv" = {sep <- ","}, "tsv" = {sep <- "\t"})
        write.table(data_pkmn(), file, quote = FALSE, sep = sep)
      }
    )
    
    output$field_checkbox <- renderUI({
      checkboxGroupInput("field_checkbox",
                         label = NULL,
                         choices = names(df),
                         selected = names(df))
    })
    
#     output$hp <- renderUI({
#       sliderInput("hp",
#                   label = "HP:",
#                   min = 0,
#                   max = max(df$hp),
#                   value = c(0, max(df$hp)),
#                   step=1, round=0)
#     })
    
    
    output$ui <- renderUI({
      elements <- list()
      if ("attack" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("attack",
                                          label = "ATTACK:",
                                          min = 0,
                                          max = max(df$attack),
                                          value = c(0, max(df$attack)),
                                          step=1, round=0)))
      }
      if ("defense" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("defense",
                                          label = "DEFENSE:",
                                          min = 0,
                                          max = max(df$defense),
                                          value = c(0, max(df$defense)),
                                          step=1, round=0)))
      }
      elements
      })
    
  })