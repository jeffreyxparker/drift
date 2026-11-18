library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)


plot_time_series <- function(orders, goals, date_range, metric, show_goals = TRUE, period) {
  
  if (metric == "Subscriptions") {
    
    plot_data <- orders %>%
      filter(created_at >= date_range[1],
             created_at <= date_range[2]) %>%
      group_by(created_at) %>%
      summarise(metric = n()) %>%
      ungroup() %>%
      mutate(metric = cumsum(metric)) 
    
    if (period == "Month") {
      prior_period_plot_data <- orders %>%
        filter(created_at >= (date_range[1] - months(1)),
               created_at <= (ceiling_date(date_range[1] - months(1), "month") - days(1))) %>%
        group_by(created_at) %>%
        summarise(metric = n()) %>%
        ungroup() %>%
        mutate(metric = cumsum(metric))  %>%
        mutate(created_at = created_at + months(1)) 
    }
    
    if (period == "Year") {
      prior_period_plot_data <- orders %>%
        filter(created_at >= (date_range[1] - years(1)),
               created_at <= (ceiling_date(date_range[1] - years(1), "year") - days(1))) %>%
        group_by(created_at) %>%
        summarise(metric = sum(total_price)) %>%
        ungroup() %>%
        mutate(metric = cumsum(metric))  %>%
        mutate(created_at = created_at + months(1)) 
    }
  }
  
  if (metric == "Revenue") {
    
    plot_data <- orders %>%
      filter(created_at >= date_range[1],
             created_at <= date_range[2]) %>%
      group_by(created_at) %>%
      summarise(metric = sum(total_price)) %>%
      ungroup() %>%
      mutate(metric = cumsum(metric)) 
    
    if (period == "Month") {
      prior_period_plot_data <- orders %>%
        filter(created_at >= (date_range[1] - months(1)),
               created_at <= (ceiling_date(date_range[1] - months(1), "month") - days(1))) %>%
        group_by(created_at) %>%
        summarise(metric = sum(total_price)) %>%
        ungroup() %>%
        mutate(metric = cumsum(metric))  %>%
        mutate(created_at = created_at + months(1)) 
    }
    
    if (period == "Year") {
      prior_period_plot_data <- orders %>%
        filter(created_at >= (date_range[1] - years(1)),
               created_at <= (ceiling_date(date_range[1] - years(1), "year") - days(1))) %>%
        group_by(created_at) %>%
        summarise(metric = sum(total_price)) %>%
        ungroup() %>%
        mutate(metric = cumsum(metric))  %>%
        mutate(created_at = created_at + months(1)) 
    }
    
  }
  
  yesterday <- orders %>%
    filter(created_at == today() - days(1)) %>%
    count()
  
  goal_plot_data <- goals %>%
    filter(Date >= date_range[1],
           Date <= date_range[2])
  
  
  a <- list(
    x = max(goal_plot_data$Date)-days(7),
    y = max(goal_plot_data$Goal) * .2,
    text = paste0(
      "Yesterday = ", yesterday, "\n",
      "Predicted Today = XXX\n",
      "Needed Today = XXX"),
    xref = "x",
    yref = "y",
    showarrow = FALSE,
    align = "right"
  )
  
  p1 <-  plot_ly() 
  if(show_goals == TRUE) {
    p1 <- p1 %>%
      add_trace(data = goal_plot_data,
                x = ~Date,
                y = ~Goal, 
                name = "Goal",
                type = 'scatter',
                mode = 'markers',
                symbols = "circle",
                marker = list(
                  size = 10,
                  color = "orange"
                )) %>%
      layout(annotations = a)
  }
  
  p1 <- p1 %>%
    add_trace(data = plot_data,
              x = ~created_at,
              y = ~metric,
              name = "Current Period",
              type = 'scatter',
              mode = 'lines',
              line = list(width = 4,
                          color = "blue")) %>%
    layout(title = paste("Cummulative", metric, "to Date for", period),
           xaxis = list(title = ""),
           yaxis = list (title = "")) %>%
    add_trace(data = prior_period_plot_data,
              x = ~created_at,
              y = ~metric,
              name = "Previous Period",
              type = "scatter",
              mode = "lines",
              line = list(
                dash = 'dash',
                color = "green"))
  
  p1
  
}

# UI
ui <- fluidPage(
  titlePanel("Drift Dashboard"),
  fluidRow(
    column(6, plotlyOutput("subs_to_date_month")),
    column(5, plotlyOutput("subs_to_date_year"))
  ),
  fluidRow(
    column(6, plotlyOutput("rev_to_date_month")),
    column(5, plotlyOutput("rev_to_date_year"))
  )
 
  
)

# Server
server <- function(input, output) {
  
  month_start <- floor_date(today(), "month")
  month_end <- ceiling_date(today(), "month") - days(1)
  year_start <- floor_date(today(), "year")
  year_end <- ceiling_date(today(), "year") - days(1)
  month_date_range <- c(month_start, month_end)
  year_date_range <- c(year_start, year_end)
  monthly_end_dates <- seq(year_start, length=12, by="months") +months(1) - days(1)
  
  monthly_sub_goals <- c(2000,4000,6000,NA,
                         NA,NA,NA,NA,
                         NA,NA,NA,NA)
  monthly_rev_goals <- c(235000,484000,765000,NA,
                         NA,NA,NA,NA,
                         NA,NA,NA,NA)
  sub_goals <- tibble("Date" = monthly_end_dates,
                      "Goal" = monthly_sub_goals)
  rev_goals <- tibble("Date" = monthly_end_dates,
                      "Goal" = monthly_rev_goals)
  
  orders <- readRDS("orders_v3.rda")
  
  output$subs_to_date_month <- renderPlotly({
    plot_time_series(orders, goals = sub_goals, date_range = month_date_range, period = "Month", metric = "Subscriptions")
  })
  
  output$subs_to_date_year <- renderPlotly({
    plot_time_series(orders, goals = sub_goals, date_range = year_date_range, period = "Year", metric = "Subscriptions", show_goals = FALSE)
  })
  
  output$rev_to_date_month <- renderPlotly({
    plot_time_series(orders, goals = rev_goals, date_range = year_date_range, period = "Month", metric = "Revenue")
  })
  
  output$rev_to_date_year <- renderPlotly({
    plot_time_series(orders, goals = rev_goals, date_range = year_date_range, period = "Year", metric = "Revenue", show_goals = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
