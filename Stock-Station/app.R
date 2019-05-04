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
                    numericInput(inputId = "strategytradesize",
                                 label = "Trade Size:",
                                 value = 100),
                    actionButton(inputId = "strategytest",
                                 label = "Test!", 
                                 icon = icon("refresh")),
                    tags$h4("*Signals, rules, and indicators are still being integrated*")
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
                                  choices = c("Crosses Over",
                                              "Crosses Under"),
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
                      checkboxInput(inputId = "rowtwo",
                                    label = "Add Another Row?",
                                    value = FALSE)
                    ),
                    conditionalPanel(
                      condition = "input.rowtwo == 1",
                      splitLayout(
                      selectInput(inputId = "indicator1",
                                  label = "First Indicator:",
                                  choices = c("SMA 200",
                                              "SMA 50"),
                                  selected = "SMA 50"),
                      selectInput(inputId = "signal1",
                                  label = "First Signal:",
                                  choices = c("Crosses Over",
                                              "Crosses Under"),
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
                      checkboxInput(inputId = "secondindsigrule",
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
    tradesize = input$strategytradesize
    initeq = input$strategyinitialequity
    initdate = "2000-01-01"
    from = input$strategyfrom
    to = input$strategyto
    Sys.setenv(TZ = "UTC")
    currency("USD")
    getSymbols(Symbols = stock, from = from, to = to, auto.assign = TRUE, env = .GlobalEnv)
    
    stock(stock, currency = "USD")
    
    initPortf(portfolio.st, symbols = stock, initDate = initdate, currency = "USD")
    
    initAcct(account.st, portfolios = portfolio.st, initDate = initdate, initEq = initeq, currency = "USD")
    
    initOrders(portfolio = portfolio.st, initDate = initdate)
    
    strategy(strategy.st, store = TRUE)
    
    add.indicator(strategy = strategy.st,
                  name = "SMA",
                  arguments = list(x = quote(Cl(mktdata)), n = 200),
                  label = "SMA200")
    
    add.indicator(strategy = strategy.st,
                  name = "SMA",
                  arguments = list(x = quote(Cl(mktdata)), n = 50),
                  label = "SMA50")
    
    add.signal(strategy.st,
               name = "sigCrossover",
               arguments = list(
                 columns = c("SMA50", "SMA200"),
                 relationship = "gt"),
               label = "LONG")
    
    add.signal(strategy.st,
               name = "sigCrossover",
               arguments = list(
                 columns = c("SMA50", "SMA200"),
                 relationship = "lt"),
               label = "SHORT")
    
    add.rule(strategy = strategy.st,
             name = "ruleSignal",
             arguments = list(sigcol = "LONG", sigval = TRUE, orderqty = tradesize,
                              ordertype = "market", orderside = "long",
                              replace = FALSE, prefer = "Close"))
    
    
    add.rule(strategy = strategy.st,
             name = "ruleSignal",
             arguments = list(sigcol = "SHORT", sigval = TRUE, orderqty = "all",
                              ordertype = "market", orderside = "long",
                              replace = FALSE, prefer = "Close"))
    
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
    
