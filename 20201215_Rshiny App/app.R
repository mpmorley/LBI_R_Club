library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  # Application title
  headerPanel("Test App"),
  
  # Sidebar with input options and controls
  sidebarPanel(
    radioButtons("filetype", label = h4("Select file input type"),choices = list( "Select from list" = 'list',"Upload data" = 'upload'),selected = 'list'),
    conditionalPanel(
      condition = "input.filetype == 'list'",
      selectInput("projects","Select a project","Testdata")
    ),
    conditionalPanel(
      condition = "input.filetype == 'upload'",
      fileInput('fileupload', 'Upload File')
    ),
    
    selectInput("plottype","Select Plot type",c("ScatterPlot","BarPlot","Histogram")),
    sliderInput("nrow", label = h4("Number of samples to plot"), min = 2,max = 50, value = 10),
    uiOutput("xaxis"),
    uiOutput("yaxis"),
    
    downloadButton('dwldplot', 'Downloadplot'),
    downloadButton('dwld', 'Download table')  
    ),
                  
    mainPanel(
      plotOutput("dispPlot"),
      dataTableOutput("datasetTable"),
      
                )
)

# Define server function
server <- function(input, output) {
  
  #Read input data
  #Load Rdata
  fileload <- reactive({
    if(input$filetype == 'list'){
      data<- read.csv("data/Testdata.csv")
      }else{
      file=input$fileupload
      data<- read.csv(file$datapath)
    }
    return(data)
  })
  
  #display project list in Dashboard
  
  output$datasetTable = renderDataTable({
      fileload()
      })
  
  #Define x and Y axis
  output$xaxis = renderUI({
    data = fileload()
    selectInput("xaxislab","Select x-axis variable",as.list(sort(as.character(colnames(data)))))
  })
  
  output$yaxis = renderUI({
    data = fileload()
    selectInput("yaxislab","Select y-axis variable",as.list(sort(as.character(colnames(data)))))
  })
  
  # generate plots
  output$dispPlot <- renderPlot({
    data = fileload()
    if(input$plottype == 'ScatterPlot'){
      gg=ggplot(data[1:input$nrow,], aes_string(x=input$xaxislab, y=input$yaxislab)) + geom_point(stat = "identity")
    }else if(input$plottype == 'BarPlot'){
      gg=ggplot(data[1:input$nrow,], aes_string(x=input$xaxislab, y=input$yaxislab)) + geom_bar(stat = "identity")
    }else{
      gg=ggplot(data[1:input$nrow,], aes_string(x=input$xaxislab)) + geom_histogram()
    }
    return(gg)
  })
  
  #Download plot
  output$dwldplot <- downloadHandler(
    filename = function() {
      paste0("plot.png")
    },
    content = function(file){
      png(file,width=4,height = 4,units= "in",useDingbats=FALSE)
      dispPlot()
      dev.off()
    })
  
  #Download table
  output$dwld <- downloadHandler(
    filename = function() { 'table.csv' },
    content = function(file) {
      write.csv(fileload(), file)
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
