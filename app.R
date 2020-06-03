library(shiny)
library(DT)
library(readr)
library(shinythemes)
library(slickR)

######################### ui #########################

ui <- fluidPage(
  
  # web theme
  theme = shinythemes::shinytheme("cerulean"),
  
  # headline
  h1("Data Presentation", align = "center"),
  br(),
  
  # pump data presentation
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "data", label = "choose data to display", choices = c(
        "pump 1", "pump 2", "pump 3", "pump 4", "pump 5", "pump 6", "pump 7"
      )),
      # contents: Data summary
      h3(textOutput(outputId = "caption")),
      verbatimTextOutput(outputId = "summary"),
      downloadButton(outputId = "downloadData", label = "Download Pump1 to Preview"),
      width = 3
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "DToutput", height = 0.5)
    )
  ),
  
  # middle line
  hr(),
  h1("roc-auc curve AND silhouette plot", align = "center"),
  br(),
  
  #roc-auc image presentation
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chooseImg", label = "choose a pump to estimate others",
                  choices = c("pump1 vs all", "pump2 vs all", "pump3 vs all", "pump4 vs all", "pump5 vs all", "pump6 vs all", "pump7 vs all"))
                ),
    mainPanel(
      slickROutput(outputId = "image", width = "80%", height = "340px")
    )
  ),
)
######################### server #########################

server <- function(input, output) {
  ######################### read data #########################
  pump1 <- read_csv("data/1.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  pump2 <- read_csv("data/2.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  pump3 <- read_csv("data/3.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  pump4 <- read_csv("data/4.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  pump5 <- read_csv("data/5.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  pump6 <- read_csv("data/6.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  # pump7 does not have instantFlow feature
  pump7 <- read_csv("data/7.csv", col_names=TRUE, col_types = list(col_datetime(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  
  colnames(pump1) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump2) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump3) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump4) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump5) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump6) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "instantFlow", "openDegree", "vacuumDegree")
  colnames(pump7) <- c("Date", "backT", "frontT", "moduleT", "I", "V", "openDegree", "vacuumDegree")
  
  ############################# 1st part #############################
  # Now, data is a reactive expression,
  # when the select box changes, data changes to the spefic pump data accordingly
  data <- reactive({
    switch(input$data, 
           "pump 1" = pump1,
           "pump 2" = pump2,
           "pump 3" = pump3, 
           "pump 4" = pump4,
           "pump 5" = pump5, 
           "pump 6" = pump6, 
           "pump 7" = pump7)
    })
  
  output$caption <- renderText("Data Summary")
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$downloadData <- downloadHandler(filename = "pump1.csv",
                                         content = function(file)
                                           {
                                              write_csv(pump1, file)
                                           }
                                         )
  
  output$DToutput <- DT::renderDataTable(data(), options = list(scrollX = T))
  
  ############################# 2nd part #############################
  # toDisplay <- switch(
  #   input$chooseImg,
  #   "pump1 vs all" = list(src = "images/roc_1vsall.png"),
  #   "pump2 vs all" = list(src = "images/roc_2vsall.png"),
  #   "pump3 vs all" = list(src = "images/roc_3vsall.png"),
  #   "pump4 vs all" = list(src = "images/roc_4vsall.png"),
  #   "pump5 vs all" = list(src = "images/roc_5vsall.png"),
  #   "pump6 vs all" = list(src = "images/roc_6vsall.png"),
  #   "pump7 vs all" = list(src = "images/roc_7vsall.png")
  # )
  li_1 <- list.files("www/1/", full.names = TRUE)
  li_2 <- list.files("www/2/", full.names = TRUE)
  li_3 <- list.files("www/3/", full.names = TRUE)
  li_4 <- list.files("www/4/", full.names = TRUE)
  li_5 <- list.files("www/5/", full.names = TRUE)
  li_6 <- list.files("www/6/", full.names = TRUE)
  li_7 <- list.files("www/7/", full.names = TRUE)
  output$image <- renderSlickR({
    
    req(input$chooseImg)
    
    if (input$chooseImg == "pump1 vs all")
      return(slickR(li_1))
    
    else if (input$chooseImg == "pump2 vs all")
      return(slickR(li_2))
    
    else if (input$chooseImg == "pump3 vs all")
      return(slickR(li_3))
    
    else if (input$chooseImg == "pump4 vs all")
      return(slickR(li_4))
    
    else if (input$chooseImg == "pump5 vs all")
      return(slickR(li_5))
    
    else if (input$chooseImg == "pump6 vs all")
      return(slickR(li_6))
    
    else if (input$chooseImg == "pump7 vs all")
      return(slickR(li_7))
  })
  
}

shinyApp(ui, server)