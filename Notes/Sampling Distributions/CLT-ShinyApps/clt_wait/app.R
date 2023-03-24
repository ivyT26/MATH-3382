library(shiny)


ui <- fluidPage(  # Define UI for random distribution app
  titlePanel("Sampling Distribution for the Mean (Skewed Right)"),  # App title
  
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
    boot.wait <- numeric(1000)
    for (i in 1:1000){
      wait.sample <- rexp(input$sampsize, 1/40)
      boot.wait[i] <- mean(wait.sample)
    }
    return(boot.wait)
  })

  output$popplot <- renderPlot({
    # load possible wait times.
    wait.time <- seq(0, 100, length=200)
    
    # Compute the value of f(x) of each wait time x if we assume
    # the times are exponentially distributed with mean 40 min.
    pdf.wait.time <- dexp(wait.time, 1/40)
    
    # Plot bmi on x-axis and the value of pdf, f(x) on y-axis.
    plot(wait.time, pdf.wait.time, 
         type="l", lty=1,         # type="l" draws line lty=1 is solid line
         xlab="Wait time (in minutes)",
         ylab="Density", main="Distribution of All Train Wait Times")
  })
  
  
  output$sampdistplot <- renderPlot({
  
    hist(boot.stuff(), xlim = c(0, 100), 
         xlab = "Sample Mean",
         main = paste("Sampling Distribution of Mean Wait Time for n = ",
                      input$sampsize,
                      sep = ""),
         xaxt='n')
    axis(1, at=seq(0, 100, 10), pos=0)
    abline(v = 40, col = "red", lwd = 2, lty = 2)
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