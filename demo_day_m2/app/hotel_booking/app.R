## app.R ##

## Dash board para el data set 'mtcars'

library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      dashboardHeader(title = "Basic dashboard"),
      
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
          menuItem("Dispersión", tabName = "graph", icon = icon("area-chart")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Imágen", tabName = "img", icon = icon("file-picture-o"))
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          # Histograma
          tabItem(tabName = "Dashboard",
                  fluidRow(
                    titlePanel("Histograma de las variables del data set mtcars"), 
                    selectInput("x", "Seleccione el valor de X",
                                choices = (booking)),
                    
                    selectInput("zz", "Selecciona la variable del grid", 
                                
                                choices = c("is_canceled", "hotel", "deposit_type", "market_segment")),
                    box(plotOutput("plot1", height = 850)),
                    
                    box(
                      title = "Controls",
                      sliderInput("bins", "Number of observations:", 1, 200, 15)
                    )
                  )
          ),
          
          # Dispersión
          tabItem(tabName = "graph", 
                  fluidRow(
                    titlePanel(h3("Gráficos de dispersión")),
                    selectInput("a", "Selecciona el valor de x",
                                choices = c("lead_time", "adr")),
                    selectInput("y", "Seleccione el valor de y",
                                choices = c("is_canceled")),
                    box(plotOutput("output_plot", height = 300, width = 460) )
                    
                  )
          ),
          
          
          
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ), 
          
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imágen de calor para la correlación de las variables")),
                    img( src = "cor_mtcars.png", 
                         height = 350, width = 350)
                  )
          )
          
        )
      )
    )
  )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
  library(ggplot2)
  
  #Gráfico de Histograma
  output$plot1 <- renderPlot({
    
    x <- booking[,input$x]
    bin <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot(booking, aes(x, fill = booking[,input$zz])) + 
      geom_histogram( ) +
      labs( xlim = c(0, max(x))) + 
      theme_gray() + 
      xlab(input$x) + ylab("Frecuencia") + 
      facet_grid(input$zz)
    
    
  })
  
  # Gráficas de dispersión
  output$output_plot <- renderPlot({ 
    
    ggplot(booking, aes(x =  booking[,input$a] , y = booking[,input$y])) + 
      geom_point() +
      ylab(input$y) +
      xlab(input$a) + 
      theme_linedraw()   #selección del grid
    
  })   
  
  #Data Table
  output$data_table <- renderDataTable( {booking}, 
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  
}


shinyApp(ui, server)
