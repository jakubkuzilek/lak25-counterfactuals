#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "", disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # define CSS
    tags$head(
      
      tags$style(HTML("
      #prev {
        width: 100%;
        height: 100%;
      }
      
      body > div > div > section > div:nth-child(2) > div:nth-child(1) {
        height: 720px;
      }
      
      #nxt {
        width: 100%;
        height: 100%;
      }
      
      body > div > div > section > div:nth-child(2) > div:nth-child(3) {
        height: 720px;
      }
      
      .content-wrapper {
        background-color: #fff;
      }
      
      .content {
        padding-top: 0px;
      }
      "
      )
      )
    ),
    
    # Application title
    fluidRow(tags$h2("Counterfactual explanations visualisations")),
    
    # Application layout
    fluidRow(
      column(width = 1,
             actionButton("prev", "", 
                          icon = icon("arrow-left"))
      ),
      # Main content
      column(width = 10,
             fluidRow(textOutput("id_text"), height = "20px"),
             fluidRow(valueBoxOutput("ratio_box", width = 6), valueBoxOutput("abs_box", width = 6), height = "100px"),
             fluidRow(plotOutput("plot_weeks", height = "200px")),
             fluidRow(plotOutput("plot_sign", height = "200px")),
             fluidRow(plotOutput("plot_diff", height = "200px"))
      ),
      column(width = 1,
             actionButton("nxt", "", 
                          icon = icon("arrow-right"))
      )
    ),
    
    fluidRow(
      column(width = 12,
             tags$p("This is shiny app for demonstration of using Counterfactual Explanations for student recommendations and identification of bottlenecks in study design."),
             tags$p("Accompanying dashboard for the", tags$a("LAK25 paper",href=""))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Counter for changing the data
  cnt <- reactiveVal(1)
  
  # observing if the prev and next buttons are clicked
  observeEvent(input$prev, {
    cnt(cnt() - 1)
    if(cnt() < 1){
      cnt(1)
    }
  })
  
  observeEvent(input$nxt, {
    cnt(cnt() + 1)
    if(cnt() > 294){
      cnt(294)
    }
  })
  
  # Display student ID
  output$id_text <- renderText({
    paste("Student ID: ", unique(final_data$id_student[final_data$id == cnt()]))
  })
  
  # Load data
  original_data <- read_csv("nicepr_obs.csv", show_col_types = FALSE)
  ce_data <- read_csv("nicepr_eval.csv", show_col_types = FALSE)
  
  # DDD 2014J
  tma_weeks <- c(2, 5, 8, 15, 20, 27)
  
  # data mumbojumbo
  original_data %<>% 
    mutate(id_student = id, id = row_number()) %>% 
    pivot_longer(cols = starts_with("week"), names_to = "week", values_to = "value") %>% 
    mutate(week = str_replace(week, "minus", "-"),
           week = as.numeric(str_remove(week, "week_"))
    ) %>% 
    mutate(type = "original")
  
  ce_data %<>%
    rename(id_student = id) %>% 
    pivot_longer(cols = starts_with("week"), names_to = "week", values_to = "value") %>% 
    mutate(week = str_replace(week, "minus", "-"),
           week = as.numeric(str_remove(week, "week_"))
    ) %>% 
    mutate(type = "ce") %>% 
    select(id_student, week, value, type) %>% 
    left_join(original_data %>% select(id, id_student), 
              by = "id_student",
              relationship = "many-to-many") %>% 
    select(id, id_student, week, value, type)
  
  final_data <- 
    original_data %>% 
    bind_rows(ce_data)
  
  # plot the original and ce time series
  output$plot_weeks <- renderPlot({
    final_data %>% 
      filter(id == cnt()) %>%
      mutate(type = if_else(type == "ce", "Counterfactual", "Original"),
             `VLE activity counts` = as.factor(type)) %>% 
      ggplot(aes(x = week, y = value, color = `VLE activity counts`)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "top") +
      geom_vline(xintercept = tma_weeks, linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(breaks = seq(min(final_data$week), max(final_data$week), by = 1),
                         expand = c(0,0)) +
      xlab("Week") +
      ylab("VLE activity counts")
  })
  
  # plot the difference between original and ce
  output$plot_diff <- renderPlot({
    final_data %>% 
      filter(id == cnt()) %>%
      pivot_wider(names_from = type, 
                  values_from = value,
                  values_fn = {mean},
                  values_fill = list(values = 0)) %>%
      arrange(week) %>%
      mutate(diff = original - ce) %>%
      ggplot(aes(x = week, y = diff)) +
      geom_line() +
      theme_bw() +
      geom_vline(xintercept = tma_weeks, linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(breaks = seq(min(final_data$week), max(final_data$week), by = 1),
                         expand = c(0,0)) +
      xlab("Week") +
      ylab("Difference in VLE activity counts")
  })
  
  # plot the sign of the difference between original and ce
  output$plot_sign <- renderPlot({
    final_data %>% 
      filter(id == cnt()) %>%
      pivot_wider(names_from = type, 
                  values_from = value,
                  values_fn = {mean},
                  values_fill = list(values = 0)) %>%
      arrange(week) %>%
      mutate(pos_neg = sign(original - ce),
             cls = if_else(pos_neg > 0, "positive", 
                           if_else(pos_neg < 0, "negative", "nothing")),
             cls = factor(cls, levels = c("negative","nothing","positive"))) %>%
      ggplot(aes(x = week, y = pos_neg)) +
      geom_line() +
      geom_point(aes(color = cls, size = 3)) +
      theme_bw() +
      geom_vline(xintercept = tma_weeks, linetype = "dashed", alpha = 0.5) +
      scale_color_manual(values = c("blue", "black", "green")) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(min(final_data$week), max(final_data$week), by = 1),
                         expand = c(0,0)) +
      xlab("Week") +
      ylab("Sign of difference in VLE activity counts")
  })
  
  # compute ratio and display it
  output$ratio_box <- renderValueBox({
    hlp_tbl <-
      final_data %>% 
      filter(id == cnt()) %>%
      pivot_wider(names_from = type, 
                  values_from = value,
                  values_fn = {mean},
                  values_fill = list(values = 0)) %>%
      arrange(week) %>%
      mutate(pos_neg = sign(original - ce)) %>% 
      count(pos_neg) 
    
    valueBox(paste0(hlp_tbl$n[hlp_tbl$pos_neg == 1],":",
                    hlp_tbl$n[hlp_tbl$pos_neg == -1], " (",
                    round(hlp_tbl$n[hlp_tbl$pos_neg == 1]/
                            hlp_tbl$n[hlp_tbl$pos_neg == -1],2), ")"), 
             "Original vs. Counterfactual Ratio",
             #icon = icon("list"),
             color = "blue"
    )
  })
  
  # compute absolute ratio and display it
  output$abs_box <- renderValueBox({
    hlp_tbl <-
      final_data %>% 
      filter(id == cnt()) %>%
      pivot_wider(names_from = type, 
                  values_from = value,
                  values_fn = {mean},
                  values_fill = list(values = 0)) %>%
      arrange(week) %>%
      mutate(pos_neg = sign(original - ce),
             diff = original - ce) %>% 
      group_by(pos_neg) %>% 
      summarise(n = sum(diff))
    
    valueBox(paste0(round(hlp_tbl$n[hlp_tbl$pos_neg == 1]/
                            -hlp_tbl$n[hlp_tbl$pos_neg == -1],2)), 
             "Original vs. Counterfactual Absolute Ratio",
             #icon = icon("list"),
             color = "blue"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
