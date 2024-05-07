library(shiny)

shinyServer(
  function(input, output, session) {
    selected_location <- reactive({
      req(input$location)
      street_all_schools_2 %>% filter(`School street` %in% input$location)
    })
    
    output$Pollutant_Concentration <- renderTable({
      req(input$pollutant)
      data <- selected_location()
      data <- data[, c("School street", input$pollutant), drop = FALSE]
      result <- summarise_at(data, vars(-`School street`), list(average = ~ mean(., na.rm = TRUE),
                                                                minimum = ~ min(., na.rm = TRUE),
                                                                maximum = ~ max(., na.rm = TRUE)))
      result
        })
    }
  )
                                                    