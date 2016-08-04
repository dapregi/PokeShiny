library(shiny)
library(DT)
library(ggplot2)

df <- read.delim("./pkmn_info2.txt", header = TRUE)

shinyServer(
  function(input, output){
    fields <- eventReactive(input$show_fields, {input$field_checkbox})
    
    data_pkmn <- reactive({
      df2 <- df
      if (!is.null(input$type) & "type" %in% fields()) {
        df2 <- df2[rowMeans(sapply(input$type, function(x) grepl(x, df2$type))) > 0, ]
      }
      if (!is.null(input$egg_group) & "egg_group" %in% fields()) {
        df2 <- df2[rowMeans(sapply(input$egg_group, function(x) grepl(x, df2$egg_group))) > 0, ]
      }
      if (!is.null(input$hp) & "hp" %in% fields()) {
        df2 <- df2[input$hp[1] <= df2$hp &
                     df2$hp <= input$hp[2], ]
      }
      if (!is.null(input$attack) & "attack" %in% fields()) {
        df2 <- df2[input$attack[1] <= df2$attack &
                     df2$attack <= input$attack[2], ]
      }
      if (!is.null(input$defense) & "defense" %in% fields()) {
        df2 <- df2[input$defense[1] <= df2$defense &
                     df2$defense <= input$defense[2], ]
      }
      if (!is.null(input$special.attack) & "special.attack" %in% fields()) {
        df2 <- df2[input$special.attack[1] <= df2$special.attack &
                     df2$special.attack <= input$special.attack[2], ]
      }
      if (!is.null(input$special.defense) & "special.defense" %in% fields()) {
        df2 <- df2[input$special.defense[1] <= df2$special.defense &
                     df2$special.defense <= input$special.defense[2], ]
      }
      if (!is.null(input$height) & "height" %in% fields()) {
        df2 <- df2[input$height[1] <= df2$height &
                     df2$height <= input$height[2], ]
      }
      if (!is.null(input$weight) & "weight" %in% fields()) {
        df2 <- df2[input$weight[1] <= df2$weight &
                     df2$weight <= input$weight[2], ]
      }
      if (!is.null(input$speed) & "speed" %in% fields()) {
        df2 <- df2[input$speed[1] <= df2$speed &
                     df2$speed <= input$speed[2], ]
      }
      df2[, fields()]
    })
    
    output$data_table <- DT::renderDataTable({
      data_pkmn()
    }, rownames = FALSE,
    options = list(lengthMenu = seq(10, 50, 10), pageLength = 20, 
                   orderClasses = TRUE))
    
    output$scatterplot <- renderPlot({
      a <- ggplot(data_pkmn(), aes_string(x = input$x, y = input$y))
      a <- a + geom_point()
      if (input$type_yes) {
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
    
    output$filters <- renderUI({
      elements <- list()
      if ("type" %in% fields()) {
        types <- unique(unlist(lapply(levels(df$type),
                                      function(x) strsplit(x, ","))))
        types <- types[order(types)]
        elements <- list(elements, 
                         list(selectInput("type",
                                          label = "Type:",
                                          choices = types,
                                          selected = types,
                                          multiple=TRUE,
                                          selectize=TRUE)))
      }
      if ("egg_group" %in% fields()) {
        egg_groups <- unique(unlist(lapply(levels(df$egg_group),
                                      function(x) strsplit(x, ","))))
        egg_groups <- egg_groups[order(egg_groups)]
        elements <- list(elements,
                         list(selectInput("egg_group",
                                          label = "Egg group:",
                                          choices = egg_groups,
                                          selected = egg_groups,
                                          multiple=TRUE,
                                          selectize=TRUE)))
      }
      if ("hp" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("hp",
                                          label = "HP:",
                                          min = 0,
                                          max = max(df$hp),
                                          value = c(0, max(df$hp)),
                                          step=10, round=0)))
      }
      if ("attack" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("attack",
                                          label = "Attack:",
                                          min = 0,
                                          max = max(df$attack),
                                          value = c(0, max(df$attack)),
                                          step=10, round=0)))
      }
      if ("defense" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("defense",
                                          label = "Defense:",
                                          min = 0,
                                          max = max(df$defense),
                                          value = c(0, max(df$defense)),
                                          step=10, round=0)))
      }
      if ("special.attack" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("special.attack",
                                          label = "Special attack:",
                                          min = 0,
                                          max = max(df$special.attack),
                                          value = c(0, max(df$special.attack)),
                                          step=10, round=0)))
      }
      if ("special.defense" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("special.defense",
                                          label = "Special defense:",
                                          min = 0,
                                          max = max(df$special.defense),
                                          value = c(0, max(df$special.defense)),
                                          step=10, round=0)))
      }
      if ("height" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("height",
                                          label = "Height:",
                                          min = 0,
                                          max = max(df$height),
                                          value = c(0, max(df$height)),
                                          step=10, round=0)))
      }
      if ("weight" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("weight",
                                          label = "Weight:",
                                          min = 0,
                                          max = max(df$weight),
                                          value = c(0, max(df$weight)),
                                          step=100, round=0)))
      }
      if ("speed" %in% fields()) {
        elements <- list(elements,
                         list(sliderInput("speed",
                                          label = "Speed:",
                                          min = 0,
                                          max = max(df$speed),
                                          value = c(0, max(df$speed)),
                                          step=10, round=0)))
      }
      elements
      })
  })