#
# D590 Spring 2022
# Final Project
# Andrew Sommers
# Team 7
# Date 4/28/2022
# 
library(tidyverse)
library(fpp3)
library(shiny)
library(shinythemes)
library(lubridate)
library(fable)
library(fable.prophet)
library(formattable)
library(ggplot2)
library(dplyr)
library(forecast)
#
# load consumer sentiment data and transform to monthly % change
readr::read_csv("MI_Consumer_Sentiment_2002_2022.csv",show_col_types = FALSE) %>%
  mutate(Month = yearmonth(mdy(date))) %>%
  select(-date) %>%
  as_tsibble(index = Month) -> sentiment
#convert sentiment score cs to %change per month and remove the first NA value (dec 2001)
sentiment_pct <- sentiment %>%
  mutate(cs_pct_change = (cs/lag(cs) - 1) * 100) %>%
  select(-cs) %>%  
  filter(!is.na(cs_pct_change))
# add the exogenous variable to the data
sentiment_pct<- sentiment_pct %>% 
  mutate(xreg = ifelse(cs_pct_change < -12,1,0))
#
# create the forecast new data
x1 <- data.frame(date = seq(as.Date("2022/2/1"), as.Date("2023/1/1"), by = "months"))
x2 <- data.frame(cs_pct_change = integer(12))
x3 <- data.frame(xreg = integer(12))
sentiment_newdata <- dplyr::bind_cols(x1, x2, x3, .id = NULL)
#
sentiment_newdata %>%
  mutate(Month = yearmonth(ymd(date))) %>%
  select(-date) %>%
  as_tsibble(index = Month) -> sentiment_newdata
#
# set the text fields for the interface
data_text = "The plotted data respresents the monthly percentage change in the U.S. consumer sentiment from 
Janumary, 2002 to January, 2022.  The data was created from the University of Michigan U.S.consumer scores dataset
which uses the first quarter end score from 1966 as a baseline score of 100.  The raw scores were converted to
monthly percentage changes for fitting a model to the data and producing a forecast"
#
product_text  ="This application fits a Prophet model to the monthly percentage change in U.S. consumer
sentiment and allows a forecast of consumer sentiment for the next 12 months.  Use the 'Forecast' tab to select the settings for fitting the Prophet model to the historical data and generating the forecast.  Note:  the default setting provide the best fit of the model to the historical data.  However, the settings allow you to input your expectations of the level of fleixbility in the forecast trend and seasonality.  Additionally, you can add one negative geopolitical or economic event in the forecast period to impact the forecasted values."
#
forecast_text = "The 12 montly change in U.S consumer sentiment can be forecast. The first slidebar sets the flexibility in the change in the trend of the data with more flexibility as the value increases.  The second slidebar sets the flexiblity in seasonality with more flexibility as the value increases.  The third input allows a forecast month to be impacted by a significant geopolitical for economic event with the value of 0 representing no negative event in the forecast period.  Only one forecast month can be selected to be impacted by a negative event. A confidence level from 75% to 95% can be selected for the forecast.  Press ' Forecast' to generate and display the forecast using the selected settings.  Press 'Reset' to reset all settings to the default values."
#
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  # application title
  titlePanel("Forecast the Monthly Percentage Change in U.S. Consumer Sentiment"),
  tabsetPanel(type = "tabs",
    tabPanel("Introduction", p(wellPanel(textOutput("text2"))),
    sidebarLayout( sidebarPanel(wellPanel(textOutput("text"))),
    mainPanel(wellPanel(plotOutput("plot", width = "100%", height = "500px")))
     )), 
  tabPanel("Forecast",p(wellPanel(textOutput("text3"))),
    sidebarLayout(
      sidebarPanel(wellPanel(actionButton("button1", "Reset"), actionButton("button2", "Forecast")),
      sliderInput("one", "Flexibility for Short-Term Trend: Less to More Flexibility", min = .10, 
         max = .30, value = .20, step = .05),
      sliderInput("two", "Flexibility for Seasonal Impact: Less to More Flexibility", min = 5,
         max = 15, value = 10),
      numericInput("three", "Forecast Month for Negative Event: Select One Month, Only", value = 0,
         min = 0, max = 12, step = 1),
      numericInput("four", "Confidence Level for Forecast", value = 85,
         min = 75, max = 95, step = 5)
          ),
       mainPanel(wellPanel(plotOutput("plotForecast", width = "100%", height = "200px")),
                                                 wellPanel(tableOutput("table"))
          )
        )
      )
    )  
  )
#
server <- function(input, output, session) {
  # 
  output$text <- renderText({data_text})
  output$text2 <- renderText({product_text})
  #  
  output$plot <- renderPlot({
    sentiment_pct %>%
      ggplot(aes(Month, cs_pct_change)) + 
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue") +
      labs(title = "Consumer Sentiment",
           subtitle = "Monthly Percentage Over 20 Years",
           y = "Percent Change", x = "Date") })
  #  
  output$text3 <- renderText({forecast_text})
  #  
  # 
  observeEvent(input$button2, {
    #
    growth_flex <- as.numeric(input$one)
    seasonal_flex <- as.numeric(input$two)
    xreg_input <- as.numeric(input$three)
    conf_level <- as.numeric(input$four)
    #
    upper_level = conf_level + 4 # cannot be higher than 99 %
    #  
    sentiment_newdata$xreg <- integer(12)
    if (xreg_input > 0) {sentiment_newdata[xreg_input,2] <- 1}
    #  
    sentiment_pct %>%
      model(prophet = prophet(cs_pct_change ~ growth("linear", changepoint_prior_scale = growth_flex) +    
          season("year", order = seasonal_flex, prior_scale = 20, type = "additive") + xreg(xreg, standardize = "auto", type = "additive"))) -> sentiment_prophet_fit
    #
    sentiment_prophet_fit %>% forecast(sentiment_newdata) %>% 
      hilo(level = c(conf_level,upper_level)) -> sentiment_prophet_forecast
    #
    months <- sentiment_prophet_forecast[,2]
    lower_range <- numeric(12)
    upper_range <- numeric(12)
    ranges <- sentiment_prophet_forecast[,6]
    for (counter in 1:12) {
      xts <- as.character(ranges[counter,])
      low_value <- as.numeric(substr(xts,13,21))
      lower_range[counter] <- low_value
      high_value <- as.numeric(substr(xts,40,48))
      upper_range[counter] <- high_value
    }
    means = sentiment_prophet_forecast[,4]
    #format and combine value - start here
    means <- formattable(means$.mean,format="f", digits=2)
    lower_range <- formattable(lower_range,format="f", digits=2)
    upper_range <- formattable(upper_range,format="f", digits=2)
    results <- data.frame(months,lower_range, means, upper_range)
    colnames(results) <- c("Months", "Lower Value", "Mean", "Upper Value")
    results <- as.data.frame(results)
    results$Months <- as.Date(results$Months)
    results$Months <- as.character(results$Months) 
    
    #       
    output$plotForecast <- renderPlot({
      input$button2
      sentiment_prophet_fit %>% forecast(sentiment_newdata) %>%
        autoplot(sentiment_pct, level = conf_level) +
        autolayer(sentiment_pct, color = 'red') +
        labs(y = "% of consumer sentiment", x = "Months",
             title = "U.S. Consumer Sentiment with 12 Month Prophet Forecast")
    })
    #
    output$table <- renderTable(results, spacing = 'xs')
    
  }) 
  #
  observeEvent(input$button1,{
    updateSliderInput(session,'one',value = .20)
    updateSliderInput(session,'two',value = 10)
    updateNumericInput(session,"three", value = 0)
    updateNumericInput(session, "four", value = 85)
    output$plotForecast <- renderPlot({NULL})
    output$table <- renderTable(NULL)
    
  })
  #
}
#
shinyApp(ui = ui, server = server)
