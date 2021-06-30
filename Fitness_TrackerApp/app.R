library(shiny)
library(DT)

source("fitness_helper.R")


category_list <- c( "bmi_category","gender","m_category")

## Panel 2
## Saving Variables

input_id_nb <- c(1, 42, 117, 221, 300)
wk_selected <-  c(0, 3, 6)

## Panel 3
## Saving Variable

input_wk_nb_new <- 0:12
wk_selected_two <- c(0, 3, 12)


### Getting and modyfing data c

weight_data <- get_data("Resources/fitness_tracking.csv")
members_data <- get_data("Resources/fitness_members.csv")
fitness_data <- fitness_data(members_data, weight_data)
fitness_long <- fitness_longer (fitness_data)
fitness_long_two <- fitness_longer_two(fitness_data)


## Preparing lists

# Getting list of membership and bmi categories

mem_cat <- fitness_long %>%
  pull(m_category) %>%
  unique()

## Getiing bmi categories

bmi_cat <- fitness_long_two %>%
  pull(bmi_category) %>%
  unique()

## UI

ui <- navbarPage(
  "Fitness Tracking",
  tabPanel("Panel 1",
           fluidPage(
             titlePanel("Stats at registration"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "factor_selected",
                   choices = category_list,
                   label = "Part 1: Select Factor",
                   selected = c("bmi_category")
                 ),
                 selectInput(
                   inputId = "factor_selectedxone",
                   choices = category_list,
                   label = "Part 2: Select First Factor",
                   selected = c("bmi_category")
                 ),
                 uiOutput('factor_selectedfillone')),
               mainPanel(
                 fluidRow(align = "center", tags$h4(textOutput("graphTitle"))),
                 plotOutput(outputId = "registraton_graph"),
                 tableOutput(outputId ="fitness_table"),
                 plotOutput(outputId = "registraton_graph_two"),
                 tableOutput(outputId ="fitness_table_two")
               )))),

  tabPanel("Panel 2",
           fluidPage(
             titlePanel("Performance monitoring "),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   inputId = "selected_id",
                   choices = input_id_nb,
                   label = "Part 1: Select Customer",
                   selected = c("1", "42", "117", "221", "300")
                 ),
                 sliderInput(
                   inputId = "week_selected_one",
                   min = 0,
                   max = 26,
                   value = 20,
                   label = "Select Weeks"
                 ),
                 checkboxGroupInput(
                   inputId = "bmi_selected_two",
                   choices = c("Underweight" = "underweight",
                               "Healthy"= "healthy",
                               "Overweight" = "overweight",
                               "Obese" = "obese"),
                   label = "Part 2: Select Weight Category",
                   selected = bmi_cat
                 )
               ),
               mainPanel(
                 fluidRow(align = "center", tags$h4(textOutput("graphTitle_two"))),
                 plotOutput(outputId = "performance_graph"),
                 dataTableOutput(outputId = "fitness_table_three",width = 800),
                 plotOutput(outputId = "performance_graph_two"),
                 tableOutput(outputId = "fitness_table_four")
               )))),

  tabPanel("Panel 3",
           fluidPage(
             titlePanel("Performance monitoring "),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "week_selected_two",
                   min = 0,
                   max = 12,
                   value = 12,
                   label = "Part 1: Select Weeks"
                 ),
                 uiOutput('factor_selectedxtwo'),
                 uiOutput('factor_selectedfilltwo'),
                 uiOutput('factor_selectedfacettwo'),
                 selectInput(inputId = "factor_selectedxthree",
                             choices = category_list ,
                             label = "Part 2: Select First Factor",
                             selected = c("gender")),
                 uiOutput('factor_selectedfillthree')),
               mainPanel(
                 fluidRow(align = "center", tags$h4(textOutput("graphTitle_three"))),
                 plotOutput(outputId = "performance_graph_three"),
                 dataTableOutput(outputId = "fitness_table_five",width = 800),
                 plotOutput(outputId = "performance_graph_four"),
                 dataTableOutput(outputId = "fitness_table_six",width = 300)
               ))))
)

## Server

server <- function(input, output) {

  output$factor_selectedfillone <- renderUI({
    selectInput(inputId = "factor_selectedfillone",
                choices = setdiff(category_list, input$factor_selectedxone) ,
                label = "Part 2: Select Second Factor",
                selected = c("gender"))
  })

  output$graphTitle <- renderText({
    paste0('Descriptive stats about members at registration')
  })

  ## Panel 1/1

  output$registraton_graph <- renderPlot({

    plot_first(fitness_long,
               input$factor_selected) %>%
      print()
  })

  output$fitness_table <- renderTable({

    table_first(fitness_long,
                input$factor_selected)
  }, striped = TRUE)

  ## Panel 1/2

  output$registraton_graph_two <- renderPlot({
    filtered_fitness_two <-
      filter_fitness_two(fitness_long,
                         input$factor_selectedxone,
                         input$factor_selectedfillone)

    req(input$factor_selectedxone != input$factor_selectedfillone)

    plot_fitness_data_two(filtered_fitness_two,
                          input$factor_selectedxone,
                          input$factor_selectedfillone) %>%
      print()
  })

  output$fitness_table_two <- renderTable({
    filtered_fitness_two <-
      filter_fitness_two(fitness_long,
                         input$factor_selectedxone,
                         input$factor_selectedfillone)

    table_two(filtered_fitness_two,
              input$factor_selectedxone,
              input$factor_selectedfillone)
  }, striped = TRUE)

  ## Panel 2

  output$graphTitle_two <- renderText({
    paste0('Performance monitoring over the time with 1 factor')
  })

  ## Panel 2/3

  output$performance_graph <- renderPlot({

    filtered_fitness_week <- filter_week(fitness_long_two,
                                         input$week_selected_one)
    filtered_fitness_id <- filter_id(filtered_fitness_week,
                                     input$selected_id)
    filtered_fitness_avg <- filter_avg(filtered_fitness_week)

    plot_fitness_data_three(filtered_fitness_week,filtered_fitness_id,filtered_fitness_avg) %>%
      print()

  })

  output$fitness_table_three<- DT::renderDataTable({

    filtered_fitness_week <- filter_week(fitness_long_two,
                                         input$week_selected_one)
    filtered_fitness_id <- filter_id(filtered_fitness_week,
                                     input$selected_id)

    table_three <- filtered_fitness_id %>%
      dplyr:: select(ID ='id',
                     Membership='m_category',
                     Gender = 'gender',
                     Week = 'wk_nb',
                     BMI ='bmi',
                     'BMI difference %'='dif_bmi',
                     'BMI category' = 'bmi_category') %>%
      ungroup()
    DT::datatable(table_three,
                  options = list(searching = FALSE,info = FALSE)) %>%
      DT::formatRound("BMI difference %") %>%
      DT::formatRound("BMI",2)
  }, striped = TRUE)

  ## Panel 2/4

  grouped_fitness <- reactive({group_fitness(fitness_long_two,
                                   input$bmi_selected_two)})

  output$performance_graph_two <- renderPlot({

    plot_fitness_data_four(grouped_fitness()) %>%
      print()
  })

  output$fitness_table_four<- renderTable({

    grouped_fitness() %>%
      dplyr:: select(Week='wk_nb',
                     'BMI category'='bmi_category',
                     Count='count')
  }, striped = TRUE)

  ## Panel 3

  output$graphTitle_three <- renderText({
    paste0('Performance monitoring over the time with 2 factors')
  })

  ## Panel 3/5

  output$factor_selectedxtwo <- renderUI({
    selectInput(inputId = "factor_selectedxtwo",
                choices = category_list,
                label = "Part 1: Select Second Factor",
                selected = c("gender"))
  })

  output$factor_selectedfilltwo <- renderUI({
    selectInput(inputId = "factor_selectedfilltwo",
                choices = setdiff(category_list, input$factor_selectedxtwo) ,
                label = "Part 1: Select Second Factor",
                selected = c("bmi_category"))
  })

  output$factor_selectedfacettwo <- renderUI({
    selectInput(inputId = "factor_selectedfacettwo",
                choices = setdiff(category_list,c(input$factor_selectedfilltwo,input$factor_selectedxtwo)) ,
                label = "Part 1: Select Third Factor",
                selected = c("m_category"))
  })

  filtered_data_five  <- reactive({ filter_data_five (fitness_long_two,
                               input$week_selected_two,
                               input$factor_selectedxtwo,
                               input$factor_selectedfilltwo,
                               input$factor_selectedfacettwo)
  })

  output$performance_graph_three <- renderPlot({

    plot_fitness_data_five(filtered_data_five(),
                           input$factor_selectedxtwo,
                           input$factor_selectedfilltwo,
                           input$factor_selectedfacettwo) %>%
      print()
  })



  output$fitness_table_five<- renderDataTable({

    DT::datatable(filtered_data_five(),
                  options = list(searching = FALSE,info = FALSE),
                  colnames = c('ID',
                             'Week',
                             'Membership' ,
                             'Gender' ,
                             'BMI category' ,
                             'Count',
                             'Percentage %')) %>%
       DT::formatRound("percentage",2)
  }, striped = TRUE)

  ## Panel 3/6

  output$factor_selectedfillthree <- renderUI({
    selectInput(inputId = "factor_selectedfillthree",
                choices = setdiff(category_list, input$factor_selectedxthree) ,
                label = "Part 2: Select Second Factor",
                selected = c("bmi_category"))
  })

  filtered_data_six  <- reactive ({filter_data_six (fitness_long_two,
                                                    input$factor_selectedxthree,
                                                    input$factor_selectedfillthree)
    })

  output$performance_graph_four <- renderPlot({

    plot_fitness_data_six(filtered_data_six(),
                          input$factor_selectedxthree,
                          input$factor_selectedfillthree) %>%
      print()

  })

  output$fitness_table_six<- renderDataTable({

    DT::datatable(filtered_data_six(),
                  options = list(searching = FALSE,info = FALSE),
      colnames = c('ID',
                   'Week',
                   'Gender' ,
                   'BMI category' ,
                   'Count',
                   'Percentage %')) %>%
      DT::formatRound("percentage",2)
  }, striped = TRUE)
}

## Running an App

options(shiny.sanitize.errors = TRUE)

shinyApp(ui, server)
