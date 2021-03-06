### app ###

library(shiny)

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Simulating Cumulative Mean and Variance for a Likert Scale"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("typeInput", "Statistic", choices = c("Mean", "SD"), selected = "SD"),
    helpText("Descriptive statistic (Mean or SD) is calculated cumaltively with each additional observation."),
    h4("Sample Details"),
    sliderInput("samplesInput", "Samples", min = 1, max = 100, value = 1),
    sliderInput("nInput", "N per Sample", min = 2, max = 1000, value = 100),
    h4("True Population Parameters"),
    sliderInput("meanInput", "Mean", min = 1, max = 7, value = 4.5),
    sliderInput("sdInput", "SD", min = 0.001, max = 5, value = 1.5),
    #checkboxInput("data", "Add Data Table", value = FALSE),
    actionButton("update", "Resample")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("vardata.plot"),
    conditionalPanel("data = TRUE",
                     tableOutput("data"))
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
  
  cumavg <- function(x){
    v <- cumsum(x) / seq_along(x)
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
    
    sdvar <- ifelse(input$typeInput == "SD", TRUE, FALSE)
    fn <- switch(input$typeInput, 
                   "Mean" = "cumavg",
                   "SD" = "cumvar",
                   "Variance" = "cumvar")
    ifelse(fn == "cumavg", 
           fnselect <- list(X = mydata, MARGIN = 2, FUN = fn),
           fnselect <- list(X = mydata, MARGIN = 2, FUN = fn, sd = sdvar))
    
    vardata.wide <- as.data.frame(do.call(apply,fnselect)) #Find SD w/ each additional obs down cols 
    reshape(vardata.wide, varying = c(1:Samples), direction = "long", sep = "", timevar = "Sample")
  })
  
  opacity <- ifelse((isolate(input$nInput)+isolate(input$samplesInput)) > 500, ((1010-isolate(input$nInput))/(1010)), 0.5)

    output$vardata.plot <- renderPlot(
        #Call the Plot
        ggplot(vardata(), aes(x=id, y=V, group = factor(Sample))) + 
        
        #Plot Lines -- Higher alpha makes lines less transparent
        geom_line(alpha = opacity/1.5, color = "dodgerblue4") + 
        
        #Add Labels
        ylab(ifelse(isolate(input$typeInput) == "Mean", "Mean", "Standard Deviation")) + 
          xlab("Sample Size") + theme_minimal() + 
        
        #Set plot limits - Try chaning this to 100, 200, or 500 for closer look
        xlim(0, isolate(input$nInput)) +
          ylim(0, ifelse(isolate(input$typeInput) == "Mean",
                         isolate(input$meanInput)*2,
                         isolate(input$sdInput)*2))
    )
    
    #output$data <- eventReactive(input$update {
    #  var.table <- reshape(vardata, idvar = "id", timevar = "V", direction = "wide")
    #  renderTable(var.table)
    #})
    
}

shinyApp(ui, server)

#runApp("~/shinyapp")