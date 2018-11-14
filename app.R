### app ###

library(shiny)

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Simulating Cumulative Variance"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    sliderInput("samplesInput", "Samples", min = 1, max = 2000, value = 100),
    sliderInput("nInput", "N per Sample", min = 2, max = 2000, value = 100),
    sliderInput("meanInput", "Mean", min = 0.1, max = 6.9, value = 4.5),
    sliderInput("sdInput", "SD", min = 0.001, max = 5, value = 1.5),
    actionButton("update", "Resample")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("vardata.plot")
    )
)

server <- function(input, output) {
  library(dplyr)
  library(ggplot2)
  
  cumvar <- function(x, sd = FALSE) {
    j <- seq_along(x) #for sequential observation within a sample ...
    v <- (cumsum(x^2) - cumsum(x)^2/j) / (j-1) #calculate variance (computational formula)
    if (sd) v <- sqrt(v) #and convert to SD
    v
  }
  
  vardata <- eventReactive(input$update, {
    Samples <- input$samplesInput #Number of samples
    n <- input$nInput #Number of participants per sample
    Simulation.Mean <- input$meanInput #True mean
    Simulation.SD <- input$sdInput #True SD
    mydata <- matrix(nrow = n, ncol = Samples)
    
    for(i in 1:Samples){
      samp <- round(rnorm(n = n, mean = Simulation.Mean, sd = Simulation.SD)) #draw a sample
      mydata[,i]<-samp #load each sample observation by columns in the mydata matrix
      mydata[mydata<1] = 1 #Round values below 1
      mydata[mydata>7] = 7 #Round values above 7
    }
    
    vardata.wide <- as.data.frame(apply(mydata,2,cumvar,sd = TRUE)) #Find SD w/ each additional obs down cols 
    vardata.wide$ID <- rep(1:n)
    reshape(vardata.wide, varying = c(1:Samples), direction = "long", sep = "", timevar = "Sample")
  })

    output$vardata.plot <- renderPlot(
        #Call the Plot
        ggplot(vardata(), aes(x=ID, y=V, group = factor(Sample))) + 
        
        #Plot Lines -- Higher alpha makes lines less transparent
        geom_line(alpha = .1, color = "dodgerblue4") + 
        
        #Add Reference Lines
        geom_hline(yintercept = Simulation.SD) + ylab("SD") + 
        theme_minimal() + #plot it
        
        #Set plot limits - Try chaning this to 100, 200, or 500 for closer look
        xlim(0, input$nInput)
    )
}

shinyApp(ui, server)

#runApp("~/shinyapp")