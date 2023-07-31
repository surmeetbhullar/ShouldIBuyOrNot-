{\rtf1\ansi\ansicpg1252\cocoartf2709
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 library(shiny)\
\
ui <- fluidPage(\
  titlePanel("Should You Buy It Or Not?!"),\
  sidebarLayout(\
    sidebarPanel(\
      numericInput("itemprice", "Price of Item:", value = 0, step = 100),\
      numericInput("monthstopay", "# of Months Needed to Pay:", value = 0, step = 100),\
      numericInput("savings", "Total Savings Amount:", value = 0, step = 100),\
      numericInput("last_check", "Total Amount in Last Check:", value = 0, step = 100),\
      numericInput("checks_per_month", "Number of Checks per Month:", value = 1, step = 1),\
      numericInput("savings_percentage", "Percentage to Savings (%):", value = 10, step = 1)\
    ),\
    mainPanel(\
      h4("Monthly Spending Amount:"),\
      verbatimTextOutput("monthly_spending")\
    )\
  )\
)\
\
server <- function(input, output) \{\
  observeEvent(\
    c(input$itemprice, input$monthstopay, input$savings, input$last_check, input$checks_per_month, input$savings_percentage),\
    \{\
      #Calculates the amount of money that the item requires per month\
      item_per_month <- (input$itemprice / input$monthstopay)\
      \
      # Calculate the amount to be saved per month\
      monthly_savings <- (input$last_check * input$savings_percentage / 100) * input$checks_per_month\
      \
      # Calculate the monthly spending amount\
      monthly_spending <- (input$last_check * input$checks_per_month) - monthly_savings\
      \
      should_you_spend <- (monthly_spending - item_per_month)\
      # Update the output\
      output$monthly_spending <- renderText(\{\
        if (should_you_spend >= 0) \{\
          paste("If you don't buy this item you would have $", format(round(monthly_spending, 2), nsmall = 2), "to spend every month.\\nIf you were to buy this item, it would leave you with $", format(round(should_you_spend, 2), nsmall = 2), " to spend every month.")\
          \}\
        else if (should_you_spend <= 0) \{\
          paste("You shouldn't purchase this item. This item would leave you with $", format(round(should_you_spend, 2), nsmall = 2), " to spend every month.")\
        \}\
      \})\
    \}\
  )\
\}\
\
shinyApp(ui, server)}