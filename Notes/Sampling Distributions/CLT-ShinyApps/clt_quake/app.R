library(shiny)


ui <- fluidPage(  # Define UI for random distribution app
  titlePanel("Sampling Distribution for the Mean (Bimodal)"),  # App title
  
  sidebarLayout(
      sidebarPanel(
      
      # Input: Slider for the sample size
      sliderInput(inputId = "sampsize",
                  label = "Sample Size:",
                  min = 1,
                  max = 100,
                  value = 4)
    ),
    
    mainPanel(
      
      # Output: Tabset w/ pop, samp dist, summary, and qqplot
      tabsetPanel(type = "tabs",
                  tabPanel("Population", plotOutput("popplot")),
                  tabPanel("Sampling Dist Plot", plotOutput("sampdistplot")),
                  tabPanel("Summary", textOutput("mean"), textOutput("se")),
                  tabPanel("Shape", plotOutput("qq"))
      )
      
    )
  )
)

server <- function(input, output) {
  
  boot.stuff <- reactive({
    boot.quake <- numeric(1000)
    for (i in 1:1000){
      quake.sample <- sample(quakes$depth, input$sampsize, replace=FALSE)
      boot.quake[i] <- mean(quake.sample)
    }
    return(boot.quake)
  })

  output$popplot <- renderPlot({
    plot(density(quakes$depth), 
         xlab = "Depth (in km)",
         main = "Depths of All Earthquakes in Fiji Since 1964",
         xaxt='n')
    axis(1, at=seq(-100, 800, 100), pos=0)
    abline(v = mean(quakes$depth), col = "red", lwd = 2, lty = 2)  })
  
  
  output$sampdistplot <- renderPlot({
  
    hist(boot.stuff(), xlim = c(0, 700), 
         xlab = "Sample Mean",
         main = paste("Sampling Distribution of Mean Depth for n = ",
                      input$sampsize,
                      sep = ""),
         xaxt='n')
    axis(1, at=seq(0, 700, 100), pos=0)
    abline(v = mean(quakes$depth), col = "red", lwd = 2, lty = 2)
  })
    
  output$mean <- renderText({
    paste("The mean of the sampling dist is ",
          round(mean(boot.stuff()), 2),
          sep = "")
  })
  output$se <- renderText({
    paste("The standard error of the sampling dist is ",
          round(sd(boot.stuff()), 4),
          sep = "")
  })
  
  output$qq <- renderPlot({
    qqnorm(boot.stuff())
    qqline(boot.stuff())
      })
}

# Create Shiny app ----
shinyApp(ui, server)