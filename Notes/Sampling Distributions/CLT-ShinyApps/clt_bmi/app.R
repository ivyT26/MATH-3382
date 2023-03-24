library(shiny)


ui <- fluidPage(  # Define UI for random distribution app
  titlePanel("Sampling Distribution for the Mean (Normal)"),  # App title
  
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
    boot.bmi <- numeric(1000)
    for (i in 1:1000){
      bmi.sample <- rnorm(input$sampsize, 26, 4)
      boot.bmi[i] <- mean(bmi.sample)
    }
    return(boot.bmi)
  })

  output$popplot <- renderPlot({
    
    bmi <- seq(26-4*4, 26+4*4, length=100)
    pdf.bmi <- dnorm(bmi, 26, 4)
    plot(bmi, pdf.bmi,
         type="l", lty=1,  # type="l" draws line lty=1 is solid line
         xlab="Body Mass Index (BMI)",
         ylab="Density", main="Distribution of BMI for Population")
  })
  
  
  output$sampdistplot <- renderPlot({
  
    hist(boot.stuff(), xlim = c(14, 38), 
         xlab = "Sample Mean",
         main = paste("Sampling Distribution of Mean BMI for n = ",
                      input$sampsize,
                      sep = ""),
         xaxt='n')
    axis(1, at=seq(14, 38, 4), pos=0)
    abline(v = 26, col = "red", lwd = 2, lty = 2)
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