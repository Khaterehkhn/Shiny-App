library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)

tickers <- c("JNJ","PFE","AZN","MRNA")
benchmarks <- c("^RUT","^GSPC","^DJI")

prices <- tq_get(tickers,
                 get="stock.prices",
                 from= today()-months(12),
                 to= today(),
                 complete_cases = F) %>%
  select(symbol,date,close)

bench <- tq_get(benchmarks,
                get= "stock.prices",
                from= today()-month(12),
                to= today()) %>%
  select(symbol,date,close)


ui<- fluidPage( theme=shinytheme("darkly"),
                
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               textInput(inputId= "title",
                                         label= "Write a title for the plot",
                                         value= "Covid Vaccine Stock Market"),
                               
                               actionButton(inputId = "update",
                                            label = "Click to Update"),
                               
                               
                               pickerInput(
                                 inputId = "stocks",
                                 label = h4("Company"),
                                 choices = c(
                                   "Johnson & Johnson"= tickers[1],
                                   "Pfizer Inc."= tickers[2],
                                   "AstraZeneca PLC"= tickers[3],
                                   "Moderna, Inc."= tickers[4]),
                                 selected = tickers,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                               ),
                               
                               radioButtons(
                                 inputId = "period",
                                 label = h4("Choose a Period"),
                                 choices = list("1 month"=1, "3 months"=2, "6 months"= 3, "12 month"= 4, "YTD"=5),
                                 selected = 4
                               ),
                               
                               radioButtons(
                                 inputId = "benchmark",
                                 label= h4("Choose a Benchmark"),
                                 choices = list("Russell 2000"=1, "S&P 500"=2, "Dow Jones Industrial Average"=3),
                                 selected = 3)
                  ),
                  
                  mainPanel(plotlyOutput("plot", height = 700))
                )
)

server <- function(input,output) {
  
 data <- eventReactive(input$update, {input$title})
  observeEvent(c(input$period, input$stocks, input$benchmark),{
    
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    if (input$period == 1) {prices <- prices %>%
      filter(
        date >= today()-months(1))}
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^RUT",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^GSPC",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 3) {
      bench <- bench %>%
        filter(symbol=="^DJI",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    output$plot <- renderPlotly({
      print(
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   ungroup() %>%
                   ggplot(aes(date, value,colour = symbol)) +
                   geom_line(size = 1, alpha = .9) +
                   ggtitle(print(data())) +
                   
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "white"),
                         panel.background = element_rect(fill="white"),
                         panel.grid = element_blank(),
                         legend.text = element_text(colour="black"))
        )
      )
    })
  }
  
  )
}
shinyApp(ui = ui, server = server)
