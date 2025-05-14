library(shiny)
library(shinyjs)

server <- function(input, output, session) {
  # Открываем необязательные поля по нажатию на кнопку
  observeEvent(input$add_region, {
    shinyjs::hide("add_region")
    shinyjs::show("region_of_moscow_buyer")
  })
  observeEvent(input$add_building_age, {
    shinyjs::hide("add_building_age")
    shinyjs::show("building_age_buyer")
  })
  observeEvent(input$add_number_of_floors, {
    shinyjs::hide("add_number_of_floors")
    shinyjs::show("number_of_floors_buyer")
  })
  observeEvent(input$add_ceiling_height, {
    shinyjs::hide("add_ceiling_height")
    shinyjs::show("ceiling_height_buyer")
  })
  observeEvent(input$add_floor, {
    shinyjs::hide("add_floor")
    shinyjs::show("floor_buyer")
  })
  observeEvent(input$add_min_to_metro, {
    shinyjs::hide("add_min_to_metro")
    shinyjs::show("min_to_metro_buyer")
  })
  observeEvent(input$add_is_apartments, {
    shinyjs::hide("add_is_apartments")
    shinyjs::show("is_apartments_buyer")
  })
  
  # Сделать доступной кнопку "Рассчитать" только если все поля заполнены
  observe({
    if (input$user_type == "buyer") {
      enable <- !is.null(input$number_of_rooms_buyer) && !is.null(input$living_area_buyer)
    } else {
      enable <- !is.null(input$region_of_moscow_seller) &&
        !is.na(input$number_of_rooms_seller) &&
        !is.na(input$living_area_seller) &&
        !is.na(input$building_age_seller) &&
        !is.na(input$number_of_floors_seller) &&
        !is.na(input$ceiling_height_seller) &&
        !is.na(input$floor_seller) &&
        !is.na(input$min_to_metro_seller) &&
        input$number_of_rooms_seller > 0 &&
        input$living_area_seller > 0 &&
        input$ceiling_height_seller >= 2 &&
        input$building_age_seller >= 0 &&
        input$number_of_floors_seller >= 1 &&
        input$floor_seller >= 0 &&
        input$min_to_metro_seller >= 0
    }
    
    shinyjs::toggleState("predict_btn", condition = enable)
  })
  
  # Показываем результат
  show_progress <- reactiveVal(FALSE)
  show_result <- reactiveVal(FALSE)
  
  output$show_progress <- reactive({ show_progress() })
  output$show_result <- reactive({ show_result() })
  outputOptions(output, "show_progress", suspendWhenHidden = FALSE)
  outputOptions(output, "show_result", suspendWhenHidden = FALSE)
  
  observeEvent(input$predict_btn, {
    show_progress(TRUE)
    show_result(FALSE)
    
    Sys.sleep(2)  # Притворимся, что считаем
    
    min_price <- if (input$user_type == "buyer") 10000000 else 8500000
    max_price <- if (input$user_type == "buyer") 15000000 else 12300000
    
    output$price_range <- renderText({
      paste("₽", format(min_price, big.mark = ","), "-", format(max_price, big.mark = ","))
    })
    
    show_progress(FALSE)
    show_result(TRUE)
  })
}