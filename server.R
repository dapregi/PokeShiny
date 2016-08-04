library(shiny)
library(DT)
library(ggplot2)

df <- read.delim("./pkmn_info2.txt", header = TRUE)

shinyServer(
  function(input, output){
    
    data_pkmn <- eventReactive(input$show_fields, {
      if (length(input$field_checkbox) == 0) {
        data.frame()
      } else if (length(input$field_checkbox) == 1) {
        df2 <- data.frame(df[, input$field_checkbox])
        names(df2) <- input$field_checkbox
        df2
      } else {
        df[, input$field_checkbox] 
      }
    })

    output$data_table <- DT::renderDataTable({
      data_pkmn()
    }, options = list(lengthMenu = seq(10, 50, 10), pageLength = 10, 
                      orderClasses = TRUE), rownames = FALSE, filter = "top")
    
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
    
  })