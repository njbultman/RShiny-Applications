library(quantmod)
library(shiny)
library(htmlwidgets)
library(tidyverse)
library(PerformanceAnalytics)
library(shinycssloaders)
library(quantstrat)
library(plotly)

ui <- fluidPage( 
  navbarPage(title = "Stock Analysis",
             tabPanel(title = "Individual Stock Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          textInput(inputId = "stock",
                                    label = "Stock Ticker:",
                                    value = ""
                          ),
                          actionButton(inputId = "chartstock",
                                       label = "Chart!", 
                                       icon = icon("refresh")),
                          tags$br(),
                          tags$br(),
                          plotOutput(outputId = "stockreturnshist") %>%
                            withSpinner()
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "stockgraph") %>%
                            withSpinner()
                        )
                      )
             ),
             tabPanel(title = "Portfolio Testing",
                      sidebarLayout(
                        sidebarPanel = NULL,
                        mainPanel = mainPanel(textInput(inputId = "portfstocks",
                                                        label = "Stock Tickers (Separated by Comma)",
                                                        value = NULL),
                                              dateInput(inputId = "portfstart",
                                                        label = "Start Date:",
                                                        value = Sys.Date() - 100),
                                              dateInput(inputId = "portfend",
                                                        label = "End Date:",
                                                        value = Sys.Date()),
                                              actionButton(inputId = "portftest",
                                                           label = "Test!", 
                                                           icon = icon("refresh")
                                              ),
                                              plotlyOutput(outputId = "plotportf") %>%
                                                withSpinner()
                        ))),
             tabPanel(title = "Strategy Testing",
                      sidebarLayout(
                        sidebarPanel(
                          textInput(inputId = "strategystock",
                                    label = "Stock to Test:",
                                    value = ""
                          ),
                          dateInput(inputId = "strategyfrom",
                                    label = "From:",
                                    value = "2010-01-01"),
                          dateInput(inputId = "strategyto",
                                    label = "To:",
                                    value = Sys.Date()),
                          numericInput(inputId = "strategyinitialequity",
                                       label = "Initial Equity:",
                                       value = 10000),
                          actionButton(inputId = "strategytest",
                                       label = "Test!", 
                                       icon = icon("refresh"))
                        ),
                        mainPanel(
                          splitLayout(
                            selectInput(inputId = "indicator1",
                                        label = "First Indicator:",
                                        choices = c("SMA 200",
                                                    "SMA 50"),
                                        selected = "SMA 50"),
                            selectInput(inputId = "signal1",
                                        label = "First Signal:",
                                        choices = c("Crosses" = "sigCrossover",
                                                    "Remains" = "sigComparison"),
                                        selected = "Crosses"),
                            selectInput(inputId = "signal1additional",
                                        label = "Over or Under:",
                                        choices = c("Over" = "gt",
                                                    "Under" = "lt"),
                                        selected = "Crosses Over"),
                            selectInput(inputId = "otherindicator1",
                                        label = "Other Indicator:",
                                        choices = c("SMA 50",
                                                    "SMA 200"),
                                        selected = "SMA 200"),
                            selectInput(inputId = "rule1",
                                        label = "Long or Short:",
                                        choices = c("Long" = "LONG",
                                                    "Short" = "SHORT"),
                                        selected = "Long"),
                            numericInput(inputId = "tradesize1",
                                         label = "Tradesize:",
                                         value = 100),
                            checkboxInput(inputId = "rowtwo",
                                          label = "Add Another Row?",
                                          value = FALSE)
                          ),
                          conditionalPanel(
                            condition = "input.rowtwo == 1",
                            splitLayout(
                              selectInput(inputId = "indicator2",
                                          label = "Second Indicator:",
                                          choices = c("SMA 200",
                                                      "SMA 50"),
                                          selected = "SMA 50"),
                              selectInput(inputId = "signal2",
                                          label = "Second Signal:",
                                          choices = c("Crosses" = "sigCrossover",
                                                      "Remains" = "sigComparison"),
                                          selected = "Crosses"),
                              selectInput(inputId = "signal2additional",
                                          label = "Over or Under:",
                                          choices = c("Over" = "gt",
                                                      "Under" = "lt"),
                                          selected = "Crosses Over"),
                              selectInput(inputId = "otherindicator2",
                                          label = "Other Indicator:",
                                          choices = c("SMA 50",
                                                      "SMA 200"),
                                          selected = "SMA 200"),
                              selectInput(inputId = "rule2",
                                          label = "Long or Short:",
                                          choices = c("Long" = "LONG",
                                                      "Short" = "SHORT"),
                                          selected = "Long"),
                              numericInput(inputId = "tradesize2",
                                           label = "Tradesize:",
                                           value = 100),
                              checkboxInput(inputId = "rowthree",
                                            label = "Add Another Row?",
                                            value = FALSE)
                            )
                          ),
                          plotOutput(outputId = "strategychart") %>%
                            withSpinner()
                        )
                      )
             )
  )
)


server <- function(input, output) {
  
  stockdata <- reactive ({
    getSymbols(Symbols = input$stock, 
               src = "yahoo",
               from = "2016-01-01",
               to = Sys.Date(),
               auto.assign = FALSE
    )
  })
  
  output$stockgraph <- renderPlotly({
    
    input$chartstock
    
    isolate({ 
      
      req(input$stock)
      
      stockgraphdata <- fortify(stockdata())
      
      stockgraphdata %>%
        plot_ly(x = ~ Index, 
                y = ~ stockgraphdata[, 7],
                hoverinfo = "text",
                text = ~ paste0(Index, "<br>",
                                input$stock, ":", " ", round(stockgraphdata[, 7], 2))) %>%
        add_lines(name = "Price", color = I("black")) %>%
        add_lines(y = rollapply(stockgraphdata[, 7], 
                                width = 200, 
                                FUN = mean, 
                                fill = NA, 
                                align = "right"),
                  name = "SMA 200",
                  color = I("red"),
                  hoverinfo = "text",
                  text = ~ paste0(Index, "<br>",
                                  "200 SMA", " ", input$stock, ":", round(rollapply(stockgraphdata[, 7], 
                                                                                    width = 200, 
                                                                                    FUN = mean, 
                                                                                    fill = NA, 
                                                                                    align = "right"), 2))) %>%
        add_lines(y = rollapply(stockgraphdata[, 7], 
                                width = 50, 
                                FUN = mean, 
                                fill = NA, 
                                align = "right"),
                  name = "SMA 50",
                  color = I("blue"),
                  hoverinfo = "text",
                  text = ~ paste0(Index, "<br>",
                                  "50 SMA", " ", input$stock, ":", round(rollapply(stockgraphdata[, 7], 
                                                                                   width = 50, 
                                                                                   FUN = mean, 
                                                                                   fill = NA, 
                                                                                   align = "right"), 2))) %>%
        layout(title = paste("<b> Prices of Ticker:", input$stock, "</b>"),
               xaxis = list(title = "<b> Date </b>"),
               yaxis = list(title = "<b> Price </b>"))
      
    })
  })
  output$plotportf <- renderPlotly({
    
    input$portftest
    
    isolate({
      
      req(input$portfstocks)
      
      tickers <- str_split(input$portfstocks, pattern = ",")
      num_stocks <- length(tickers[[1]])
      
      portfreturns <- xts()
      
      for(i in 1:num_stocks) {
        
        if(length(portfreturns) == 0) {
          
          portfreturns <- Return.calculate(getSymbols(tickers[[1]][i], 
                                                      auto.assign = FALSE,
                                                      from = input$portfstart,
                                                      to = input$portfend)[, 6],
                                           method = "discrete")[-1,] 
          
        } else {
          
          portfreturns <- merge(portfreturns, Return.calculate(getSymbols(tickers[[1]][i], 
                                                                          auto.assign = FALSE,
                                                                          from = input$portfstart,
                                                                          to = input$portfend)[, 6], 
                                                               method = "discrete")[-1,])
          
        }
      }
      
      overallportfreturn <- Return.portfolio(portfreturns)
      
      plotportfreturn <- fortify(overallportfreturn)
      
      plotportfreturn %>%
        plot_ly(x =  ~ portfolio.returns) %>%
        add_histogram(color = I("skyblue"))
      
    })
    
  })
  
  output$stockreturnshist <- renderPlot({
    
    input$chartstock
    
    isolate({
      
      req(input$stock)
      
      chart.Histogram(R = periodReturn(stockdata()[, 6], period = "daily"), 
                      method = c("add.density", "add.normal"),
                      main = paste("Histogram of", input$stock, "Returns"))
      
    })
    
  })
  
  output$strategychart <- renderPlot({
    
    input$strategytest
    
    isolate({
      
      req(input$strategystock)
      
      strategy.st <- portfolio.st <- account.st <- "firstsrat"
      
      rm.strat(strategy.st)
      
      stock <- input$strategystock
      initeq <- input$strategyinitialequity
      initdate <- "2000-01-01"
      from <- input$strategyfrom
      to <- input$strategyto
      Sys.setenv(TZ = "UTC")
      currency("USD")
      getSymbols(Symbols = stock, from = from, to = to, auto.assign = TRUE, env = .GlobalEnv)
      
      stock(stock, currency = "USD")
      
      initPortf(portfolio.st, symbols = stock, initDate = initdate, currency = "USD")
      
      initAcct(account.st, portfolios = portfolio.st, initDate = initdate, initEq = initeq, currency = "USD")
      
      initOrders(portfolio = portfolio.st, initDate = initdate)
      
      strategy(strategy.st, store = TRUE)
      
      #Two indicators for the first row of inputs
      
      add.indicator(strategy = strategy.st,
                    name = substr(input$indicator1, start = 1, stop = 3),
                    arguments = list(x = quote(Cl(mktdata)), n = as.integer(substr(input$indicator1, start = 5, stop = 7))),
                    label = paste0(substr(input$indicator1, start = 1, stop = 3),
                                   substr(input$indicator1, start = 5, stop = 7)))
      
      add.indicator(strategy = strategy.st,
                    name = substr(input$otherindicator1, start = 1, stop = 3),
                    arguments = list(x = quote(Cl(mktdata)), n = as.integer(substr(input$otherindicator1, start = 5, stop = 7))),
                    label = paste0(substr(input$otherindicator1, start = 1, stop = 3),
                                   substr(input$otherindicator1, start = 5, stop = 7)))
      
      #Two indicators for the second row of inputs (if box is checked)
      
      if(input$rowtwo == 1) {
        
        add.indicator(strategy = strategy.st,
                      name = substr(input$indicator2, start = 1, stop = 3),
                      arguments = list(x = quote(Cl(mktdata)), n = as.integer(substr(input$indicator2, start = 5, stop = 7))),
                      label = paste0(substr(input$indicator2, start = 1, stop = 3),
                                     substr(input$indicator2, start = 5, stop = 7)))
        
        add.indicator(strategy = strategy.st,
                      name = substr(input$otherindicator2, start = 1, stop = 3),
                      arguments = list(x = quote(Cl(mktdata)), n = as.integer(substr(input$otherindicator2, start = 5, stop = 7))),
                      label = paste0(substr(input$otherindicator2, start = 1, stop = 3),
                                     substr(input$otherindicator2, start = 5, stop = 7))) 
        
      }
      
      #First signal for the indicators corresponding to the first row
      
      add.signal(strategy.st,
                 name = input$signal1,
                 arguments = list(
                   columns = c(paste0(substr(input$indicator1, start = 1, stop = 3),
                                      substr(input$indicator1, start = 5, stop = 7)), 
                               paste0(substr(input$otherindicator1, start = 1, stop = 3),
                                      substr(input$otherindicator1, start = 5, stop = 7))),
                   relationship = input$signal1additional),
                 label = input$rule1)
      
      #Second signal for the indicators corresponding to the second row
      
      if(input$rowtwo == 1) {
      
      add.signal(strategy.st,
                 name = input$signal2,
                 arguments = list(
                   columns = c(paste0(substr(input$indicator2, start = 1, stop = 3),
                                      substr(input$indicator2, start = 5, stop = 7)), 
                               paste0(substr(input$otherindicator2, start = 1, stop = 3),
                                      substr(input$otherindicator2, start = 5, stop = 7))),
                   relationship = input$signal2additional),
                 label = input$rule2)
      }
      
      #First rule corresponding to first row
      
      add.rule(strategy = strategy.st,
               name = "ruleSignal",
               arguments = list(sigcol = input$rule1, sigval = TRUE, orderqty = if(input$rule1 == "LONG") {
                                                                                    input$tradesize1
                                                                                } else {
                                                                                    -input$tradesize1
                                                                                  },
                                ordertype = "market", orderside = NULL,
                                replace = FALSE, prefer = "Close"))
      
      #Second rule corresponding to second row
      
      if(input$rowtwo == 1) {
        
      add.rule(strategy = strategy.st,
               name = "ruleSignal",
               arguments = list(sigcol = input$rule2, sigval = TRUE, orderqty = if(input$rule2 == "LONG") {
                                                                                     input$tradesize2
                                                                                   } else {
                                                                                     -input$tradesize2
                                                                                   },
                                ordertype = "market", orderside = "long",
                                replace = FALSE, prefer = "Close"))
      }
      
      out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
      
      updatePortf(portfolio.st)
      
      daterange <- time(getPortfolio(portfolio.st)$summary[-1])
      
      updateAcct(account.st, daterange)
      updateEndEq(account.st)
      
      portf_stats <- tradeStats(Portfolios = portfolio.st)
      
      chart.Posn(Portfolio = portfolio.st,
                 Symbol = stock)
      
      rm(list = setdiff(ls(envir = .GlobalEnv), c("ui", "server")), pos = .GlobalEnv)
      
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
