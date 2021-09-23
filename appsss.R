## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  header <- dashboardHeader(title = "Forcasting"),
  
  ## Sidebar content
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Pie Chart", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Bawang Merah", tabName = "dashboard1", icon = icon("chart-bar")),
      menuItem("Rice Price", tabName = "dashboard2", icon = icon("chart-bar")),
      menuItem("Controller", tabName = "controller", icon = icon("th"))
    )
  ),
  ## Body content
  body <- dashboardBody(
    tabItems(
      # Tab Content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Pie Chart Produksi", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot7", height = 500)
                )
              )
      ),
      # First Tab Content
      tabItem(tabName = "dashboard1",
              fluidRow(
                box(title = "Forcasting Bawang Merah di Banda Aceh", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 200),
                    verbatimTextOutput("detail1")
                ),
                box(title = "Forcasting Bawang Merah di Medan", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2", height = 200),
                    verbatimTextOutput("detail2")
                ),
                box(title = "Forecast Bawang Merah Padang", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot3", height = 200),
                    verbatimTextOutput("detail3")
                )
              )
      ),
      # Second Tab Content
      tabItem(tabName = "dashboard2",
              fluidRow(
                box(title = "Forecast Rice Price Holt-Winter Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 200),
                    verbatimTextOutput("detail1")
                ),
                box(title = "Forecast Rice Price MLP Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2", height = 200),
                    verbatimTextOutput("detail2")
                ),
                box(title = "Forecast Rice Price Auto-Arima Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot3", height = 200),
                    verbatimTextOutput("detail3")
                )
              )
      ),
      # Third tab content
      tabItem(tabName = "controller",
              fluidRow(
                box(title = "Choose Regional Zone of Rice", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    selectInput("reg",
                                label = "Choose Regional",
                                choices = c("West Java",
                                            "Central Java",
                                            "East Java",
                                            "South Sulawesi"),
                                selected = "East Java")
                )
                
              )
      )
    )
  ),
  dashboardPage(
    header,
    sidebar,
    body
  )
)

server <- function(input, output) {
  #Rice Produce
  datatsy = read.csv("lat.csv", check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE)
  datasaya = na.omit(datatsy)
  
  library(forecast)
  library(nnfor)
  library(sarima)
  library(tseries)

  #ARIMA Produce BW aceh
  Grafik_bawang_Merah1 <- ts(datasaya$`Banda Aceh`, start = c(2018,1), end =c(2021,1), frequency = 12)
  ts.plot(Grafik_bawang_Merah1, col="red", main="bawang merah", lwd=2)
  plot(Grafik_bawang_Merah1)
  ar1 <- auto.arima(Grafik_bawang_Merah1)
  ar1 <- auto.arima(Grafik_bawang_Merah1)
  ar1
  checkresiduals(ar1)
  summary(ar1)
  tsy1 <- forecast(ar1,24)
  tsy1
  plot(tsy1)
  detail1<- accuracy(tsy1)
  detail1
  
  #ARIMA Produc BW medan
  Grafik_bawang_Merah2 <- ts(datasaya$`M e d a n` , start = c(2018,1), end =c(2021,1), frequency = 12)
  ts.plot(Grafik_bawang_Merah2, col="red", main="bawang merah", lwd=2)
  ar2 <- auto.arima(Grafik_bawang_Merah2)
  ar2 <- auto.arima(Grafik_bawang_Merah2)
  ar2
  checkresiduals(ar2)
  summary(ar2)
  tsy2 <- forecast(ar2,24)
  tsy2
  plot(tsy2)
  detail2<- accuracy(tsy2)
  detail2
  
  #ARIMA Produc BWPadang
  Grafik_bawang_Merah3 <- ts(datasaya$`P a d a n g` , start = c(2018,1), end =c(2021,1), frequency = 12)
  ts.plot(Grafik_bawang_Merah3, col="red", main="bawang merah", lwd=2)
  ar3 <- auto.arima(Grafik_bawang_Merah3)
  ar3 <- auto.arima(Grafik_bawang_Merah3)
  ar3
  checkresiduals(ar3)
  summary(ar3)
  tsy3 <- forecast(ar3,24)
  tsy3
  plot(tsy3)
  detail3<- accuracy(tsy3)
  detail3
  
  #Piechart
  dt = read.csv("sayur.csv",  check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE)
  #pieee <- pie(dt$`Bawang Merah (Ton)`, dt$Provinsi)
  

  output$detail1 <- renderText({ detail1[,"MAPE"]})
  output$detail2 <- renderText({ detail2[,"MAPE"]})
  output$detail3 <- renderText({ detail3[,"MAPE"]})
  output$plot1 <- renderPlot({plot(tsy1)})
  output$plot2 <- renderPlot({plot(tsy2)})
  output$plot3 <- renderPlot({plot(tsy3)})
  output$plot7 <- renderPlot({pie(dt$`Bawang Merah (Ton)`, dt$Provinsi)})
}



shinyApp(ui, server)
