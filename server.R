library(shiny)
library(ggplot2)

df <- read.delim("./pkmn_info.txt", header = TRUE)

shinyServer(
  function(input, output){
    data_pkmn <- reactive({
      df[input$attack_threshold[1] <= df$attack & 
           input$attack_threshold[2] >= df$attack &
           !is.na(df$attack) &
           input$hp_threshold[1] <= df$hp & 
           input$hp_threshold[2] >= df$hp &
           !is.na(df$hp) &
           input$defense_threshold[1] <= df$defense & 
           input$defense_threshold[2] >= df$defense &
           !is.na(df$defense)
         ,]
      })  
    
    output$data_table <- renderDataTable({
      data_pkmn()
    },
    options = list(lengthMenu = c(10, 20, 30, 40, 50), pageLength = 10))
    
    output$scatterplot <- renderPlot({
      a <- ggplot(data_pkmn(), aes_string(x = input$x, y = input$y))
      a <- a + geom_point()
      if (!input$type) {
        a1 <- a
      } else {
        a1 <- a + facet_wrap(~type)
      }
      if(!input$regression) {
        a2 <- a1
      } else {
        a2 <- a1 + geom_smooth(method = 'lm', se = TRUE)
      }
      a2
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
        paste('pokemon-filtrated-', Sys.Date(), '.txt')
      },
      content = function(file) {
        write.table(data_pkmn(), file, quote = FALSE, sep = "\t")
      }
    )
  })
