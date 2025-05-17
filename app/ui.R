library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Рассчёт стоимости жилья в Москве"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("user_type", "Режим:",
                   choices = list("Покупатель" = "buyer", "Продавец" = "seller"),
                   selected = "buyer"),
      
      # --- Форма покупателя ---
      conditionalPanel(
        condition = "input.user_type == 'buyer'",
        
        checkboxGroupInput("number_of_rooms_buyer", "Количество комнат:", choices = 1:7),
        sliderInput("living_area_buyer", "Жилая площадь (м²):", min = 1, max = 120, value = c(30, 70)),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_region", "Добавить регион Москвы")),
        hidden(selectInput("region_of_moscow_buyer", "Регион Москвы:",
                           choices = list(
                             "ЦАО" = "CAR", "САО" = "NAR", "СВАО" = "NEAR", 
                             "ВАО" = "EAR", "ЮВАО" = "SEAR", "ЮАО" = "SAR", 
                             "ЮЗАО" = "SWAR", "ЗАО" = "WAR", "СЗАО" = "NWAR"
                           ), multiple = TRUE)
        ),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_ceiling_height", "Добавить высоту потолков")),
        hidden(sliderInput("ceiling_height_buyer", "Высота потолков (м):", min = 2.0, max = 5.0, step = 0.1, value = c(2.5, 3.2))),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_floor", "Добавить этаж")),
        hidden(sliderInput("floor_buyer", "Этаж (min-max):", min = 1, max = 80, value = c(1, 10))),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_number_of_floors", "Добавить количество этажей")),
        hidden(sliderInput("number_of_floors_buyer", "Количество этажей в здании:", min = 1, max = 80, value = c(1, 10))),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_building_age", "Добавить возраст здания")),
        hidden(sliderInput("building_age_buyer", "Возраст здания (лет):", min = 0, max = 100, value = c(10, 30))),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_min_to_metro", "Добавить минут до метро")),
        hidden(sliderInput("min_to_metro_buyer", "Минут до метро:", min = 0, max = 60, value = c(5, 20))),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_is_apartments", "Добавить: квартира")),
        hidden(checkboxInput("is_apartments_buyer", "Квартира", value = FALSE)),
        
        tags$div(style = "margin-bottom: 10px;",
                 actionButton("add_is_new", "Добавить: новостройка")),
        hidden(checkboxInput("is_new_buyer", "Новостройка", value = FALSE))
      ),
      
      # --- Форма продавца ---
      conditionalPanel(
        condition = "input.user_type == 'seller'",
        
        
        numericInput("living_area_seller", "Жилая площадь (м²):", value = NULL, min = 1),
        numericInput("number_of_rooms_seller", "Количество комнат:", value = NULL, min = 1),
        selectInput("region_of_moscow_seller", "Регион Москвы:",
                    choices = list(
                      "ЦАО" = "CAR", "САО" = "NAR", "СВАО" = "NEAR", "ВАО" = "EAR",
                      "ЮВАО" = "SEAR", "ЮАО" = "SAR", "ЮЗАО" = "SWAR",
                      "ЗАО" = "WAR", "СЗАО" = "NWAR"
                    ),
                    multiple = FALSE),
        numericInput("ceiling_height_seller", "Высота потолков (м):", value = NULL, step = 0.1, min = 2),
        numericInput("floor_seller", "Этаж:", value = NULL, min = 0),
        numericInput("number_of_floors_seller", "Количество этажей в здании:", value = NULL, min = 1),
        numericInput("building_age_seller", "Возраст здания (лет):", value = NULL, min = 0),
        numericInput("min_to_metro_seller", "Минут до метро:", value = NULL, min = 1),
        checkboxInput("is_apartments_seller", "Квартира", value = FALSE),
        checkboxInput("is_new_seller", "Новостройка", value = FALSE)
      ),
      
      actionButton("predict_btn", "Рассчитать", class = "btn-primary")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.show_progress",
        tags$div(class = "progress",
                 tags$div(class = "progress-bar progress-bar-striped active",
                          role = "progressbar",
                          style = "width: 100%")
        )
      ),
      
      conditionalPanel(
        condition = "output.show_result",
        tags$h3("Диапазон стоимости:"),
        verbatimTextOutput("price_range"),
        tags$p("Тут будет пояснительный текст.")
      )
    )
  )
)
