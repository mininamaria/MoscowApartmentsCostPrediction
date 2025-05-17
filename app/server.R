library(shiny)
library(shinyjs)
library(xgboost)
library(caret)

# Load model and preprocessor once when app starts
model <- readRDS("src/best_xgb_model.rds")
preprocessor <- readRDS("src/preprocessor.rds")

server <- function(input, output, session) {
  # --- Show hidden inputs when their buttons are pressed ---
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
  
  # --- Enable/disable the predict button depending on input completeness ---
  observe({
    enable <- FALSE
    
    if (input$user_type == "buyer") {
      enable <- !is.null(input$number_of_rooms_buyer) && !is.null(input$living_area_buyer)
    } else {
      enable <-
        !is.null(input$region_of_moscow_seller) &&
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
  
  # --- Control for showing progress and result ---
  show_progress <- reactiveVal(FALSE)
  show_result <- reactiveVal(FALSE)
  
  output$show_progress <- reactive({ show_progress() })
  output$show_result <- reactive({ show_result() })
  outputOptions(output, "show_progress", suspendWhenHidden = FALSE)
  outputOptions(output, "show_result", suspendWhenHidden = FALSE)
  
  # --- Prediction logic inside eventReactive ---
  prediction <- eventReactive(input$predict_btn, {
    # Input validation
    if (input$user_type == "buyer") {
      validate(
        need(!is.null(input$number_of_rooms_buyer), "Укажите количество комнат."),
        need(!is.null(input$living_area_buyer), "Укажите жилую площадь."),
        need(input$living_area_buyer[1] >= 1 && input$living_area_buyer[2] <= 120,
             "Жилая площадь должна быть от 1 до 120 м²."),
        if (!is.null(input$ceiling_height_buyer))
          need(input$ceiling_height_buyer[1] >= 2 && input$ceiling_height_buyer[2] <= 5,
               "Высота потолков должна быть от 2 до 5 м."),
        if (!is.null(input$floor_buyer))
          need(input$floor_buyer[1] >= 1 && input$floor_buyer[2] <= 80,
               "Этаж должен быть от 1 до 80."),
        if (!is.null(input$number_of_floors_buyer))
          need(input$number_of_floors_buyer[1] >= 1 && input$number_of_floors_buyer[2] <= 80,
               "Количество этажей должно быть от 1 до 80."),
        if (!is.null(input$building_age_buyer))
          need(input$building_age_buyer[1] >= 0 && input$building_age_buyer[2] <= 100,
               "Возраст здания должен быть от 0 до 100 лет."),
        if (!is.null(input$min_to_metro_buyer))
          need(input$min_to_metro_buyer[1] >= 0 && input$min_to_metro_buyer[2] <= 60,
               "Минут до метро должно быть от 0 до 60.")
      )
    } else {
      validate(
        need(!is.na(input$number_of_rooms_seller) && input$number_of_rooms_seller > 0,
             "Количество комнат должно быть больше 0."),
        need(!is.na(input$living_area_seller) && input$living_area_seller > 0,
             "Жилая площадь должна быть больше 0."),
        need(input$ceiling_height_seller >= 2 && input$ceiling_height_seller <= 5,
             "Высота потолков должна быть от 2 до 5 м."),
        need(input$floor_seller >= 0 && input$floor_seller <= 80,
             "Этаж должен быть от 0 до 80."),
        need(input$number_of_floors_seller >= 1 && input$number_of_floors_seller <= 80,
             "Количество этажей должно быть от 1 до 80."),
        need(input$building_age_seller >= 0 && input$building_age_seller <= 100,
             "Возраст здания должен быть от 0 до 100 лет."),
        need(input$min_to_metro_seller >= 0 && input$min_to_metro_seller <= 60,
             "Минут до метро должно быть от 0 до 60."),
        need(!is.null(input$region_of_moscow_seller), "Выберите регион Москвы.")
      )
    }
    
    # Simulate model prediction delay
    Sys.sleep(1.5)
    
    # Return prediction result
    if (input$user_type == "buyer") {
      list(min = 10000000, max = 15000000)
    } else {
      list(min = 8500000, max = 12300000)
    }
  })
  
  # --- Trigger progress bar and result display ---
  observeEvent(input$predict_btn, {
    show_progress(TRUE)
    show_result(FALSE)
  })
  
  observeEvent(prediction(), {
    show_progress(FALSE)
    show_result(TRUE)
    
    result <- prediction()
    output$price_range <- renderText({
      paste("₽", format(result$min, big.mark = ","), "-", format(result$max, big.mark = ","))
    })
  })
}