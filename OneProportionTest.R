library(shiny)
library(ggplot2)
library(dplyr)
library(plyr)
library(BHH2)
library(gridExtra)
library(rhandsontable)


ui <- fluidPage(titlePanel("One Proportion Test"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("numsamp", "Shuffle how many times?", value = 100, min = 1),
                    hr(),
                    tags$div(class="header", checked = NA,
                             tags$p("Explanatory Text Here"),
                             tags$p("And Some More Stuff Here")
                    ),
                    tags$p("Your Sample"),
                    textInput("sample.x", "Enter Number of Successes: ", 10),
                    textInput("sample.n", "Enter Sample Size: ", 25),
                    tags$p("Claim"),
                    textInput("claim.p", "Claimed Proportion: ", 0.3),
                    actionButton("Replicate", "Replicate!"),
                    actionButton("Reset", "Reset"),
                    checkboxInput("Show.Observed", "Show observed proportion", FALSE),
                    textOutput("Observed"),
                    textOutput("count.samples"),
                    selectInput("inequality", NULL, c("greater than", "less than")),
                    textInput("cutoff", NULL),
                    textOutput("counts")
                  ),
                  
                  mainPanel(              
                    plotOutput("RandomPlot1")
                  )
                )
)



server <- function(input, output) {
  values <- reactiveValues()
  values$props <- vector()
  
  
  #create dotplot locations from data x
  dotplot_locs <- function(x){
    counts <- table(x)
    x.locs <- as.numeric(names(counts))
    point_dist <- min(diff(as.numeric(names(counts))))/6
    
    x.coord <- sapply(x.locs, function(x) x + ((1:4)-2.5)*point_dist)
    
    x.coords <- vector()
    y.coords <- vector()
    to.red <- vector()
    for (i in 1:length(counts)){
      x.coords <- c(x.coords, rep(x.coord[, i], counts[i]/4), x.coord[0:(counts[i] %% 4), i])
      if (counts[i] > 4){
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/4), 4)),
                      rep(ceiling(counts[i]/4), counts[i] %% 4))
      } else {
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/4), counts[i])))
      }
      if (input$inequality == "greater than"){
        if (names(counts)[i] >= input$cutoff){
          to.red <- c(to.red, rep("red", counts[i]))
        } else {
          to.red <- c(to.red, rep("black", counts[i]))
        }
      } else {
        if (names(counts)[i] <= input$cutoff){
          to.red <- c(to.red, rep("red", counts[i]))
        } else {
          to.red <- c(to.red, rep("black", counts[i]))
        }
      }
    }
    return(data.frame("x" = x.coords, "y" = y.coords*4, "red" = to.red))
  }
  
  #set up starting values for the app
  mylist <- reactiveValues(n = 1, p = 1, numsamp = 1)
  
  #these will update each time the user clicks the Replicate button
  observeEvent(input$Replicate, {
    mylist$n <- as.numeric(input$sample.n)
    mylist$p <- as.numeric(input$claim.p)
    mylist$numsamp = as.numeric(input$numsamp)
    mylist$observed = as.numeric(input$sample.x)/as.numeric(input$sample.n)
})
  
  observeEvent(input$Reset, {
    values$props <- vector()
  })
  
  update_vals <- eventReactive(input$Replicate, {
    values$props <- c(values$props, rbinom(mylist$numsamp, mylist$n, mylist$p)/mylist$n)
  })
  
  update_counts <- eventReactive(c(input$cutoff, input$Replicate, input$Reset, input$inequality), {
    if (!is.na(as.numeric(input$cutoff))){
      if (input$inequality == "greater than"){
        values$prob <- sum(values$props >= input$cutoff)/length(values$props)
        values$count <- sum(values$props >= input$cutoff)
      } else {
        values$prob <- sum(values$props <= input$cutoff)/length(values$props)
        values$count <- sum(values$props <= input$cutoff)
      }
    }
  })
  
  output$RandomPlot1 <- renderPlot({
    update_vals()
    if (length(values$props) != 0){ # after reset, values$props is empty
      df <- dotplot_locs(values$props)
      myplot <- ggplot(df)  +
        geom_point(aes(x ,y, colour = red), size=50/length(values$props)^0.5) +
        scale_colour_manual(name = "red",values = c("black", "red")) + 
        theme(legend.position="none")
      if (!is.na(as.numeric(input$cutoff))){
        myplot <- myplot + geom_vline(xintercept = as.numeric(input$cutoff), color = "red")
      }
      myplot
    }
  })
  
  
  output$Observed <- renderText({
    if (input$Show.Observed){
      if (length(values$props) != 0){
        paste("Observed Difference:", round(mylist$observed, 3))
      }
    }
  })
  
  output$count.samples <- renderText({
    "Count Samples"
  })
  
  output$counts <- renderText({
    update_counts()
    if (!is.null(values$prob)){
      paste(values$count, "/", length(values$props), " (", round(values$prob, 3), ")", sep = "")
    }
  })
  
  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]])){
        DF = data.frame("X1" = c(21, 14, 35), "X2" = c(3, 10, 13))
      }
      else{
        DF = values[["DF"]]
      }
    }
    DF[3, ] <- apply(DF[-3, ], 2, sum)
    DF[, 3] <- apply(DF[, -3], 1, sum)
    values[["DF"]] = DF
    DF
  })
  
  
  
  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      rhandsontable(DF, colHeaders = c(unlist(strsplit(input$colnames, ",")), "Total"),
                    rowHeaders = c(unlist(strsplit(input$rownames, ",")), "Total"))
  })
  
  
  
}


shinyApp(ui = ui, server = server, options = list(height = 1080))

