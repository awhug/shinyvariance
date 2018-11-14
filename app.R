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
    sliderInput("sdInput", "SD", min = 0.001, max = 5, value = 1.5)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("vardata.plot"),
    br(), br(),
    tableOutput("vardata.table")
  )
)

server <- function(input, output) {
  library(dplyr)
  library(ggplot2)
  Samples <- reactive(input$samplesInput) #Number of samples
  n <- reactive(input$nInput) #Number of participants per sample
  Simulation.Mean <- reactive(input$meanInput) #True mean
  Simulation.SD <- reactive(input$sdInput) #True SD
  
  cumvar <- function(x, sd = FALSE) {
    j <- seq_along(x) #for sequential observation within a sample ...
    v <- (cumsum(x^2) - cumsum(x)^2/j) / (j-1) #calculate variance (computational formula)
    if (sd) v <- sqrt(v) #and convert to SD
    v
  }
  
  mydata <- reactive(matrix(nrow = n, ncol = Samples))
  
  reactive(for(i in 1:Samples){
    samp <- round(rnorm(n = n, mean = Simulation.Mean, sd = Simulation.SD)) #draw a sample
    mydata[,i]<-samp #load each sample observation by columns in the mydata matrix
    mydata[mydata<1] = 1 #Round values below 1
    mydata[mydata>7] = 7 #Round values above 7
    })
  
  vardata <- reactive({
    vardata.wide <- as.data.frame(apply(mydata,2,cumvar,sd = TRUE)) #Find SD w/ each additional obs down cols 
    vardata.wide$ID <- rep(1:n)
    reshape(vardata, varying = c(1:Samples), direction = "long", sep = "", timevar = "Sample")
  })

    output$vardata.plot <- renderPlot(
        #Call the Plot
        ggplot(vardata(), aes(x=as.factor(ID), y=V, group = factor(SampleNo))) + 
        
        #Plot Lines -- Higher alpha makes lines less transparent
        geom_line(alpha = .1, color = "dodgerblue4") + 
        
        #Add Reference Lines
        geom_hline(yintercept = Simulation.SD) + ylab("SD") + 
        theme_minimal() + #plot it
        
        #Set plot limits - Try chaning this to 100, 200, or 500 for closer look
        xlim(0, n)
    )
    
    output$vardata.table <- renderTable(
      head(vardata)
    )
}

shinyApp(ui, server)

#runApp("~/shinyapp")