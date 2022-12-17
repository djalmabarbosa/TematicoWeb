library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Temático Web"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Escolha um banco de dados:",
                  choices = c("Preliminar", "Primeira Onda")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)


## get a list of datasets ----------------------------------------------------------------

if (file.exists("./datasetlist.csv") )
{
  ## datasetname, title, description
  datasetlist<-read.csv("./datasetlist.csv", stringsAsFactors = FALSE)
} else {
  "Dataset file is missing."
  return()
}

datafiles<-datasetlist$datasetname
datafiles<-setNames(datafiles, datasetlist$title)

emptymatch<-"."





# Define server logic to summarize and view selected dataset ------------------------------
server <- function(input, output) {
  
  # Return the requested dataset ----
  output$datafiles <-renderUI({   
    selectInput("dataset", "Selecione os dados", choices = c(None=".", arquivos), selected=".")
  }) 
  
  getDataPath<- reactive({
    
    fullpath<-paste0('./dados/', input$dataset)
  })
  
  getDatsetInfo<-function(fullpath)
  {
    nomebanco<<-NULL
    nomebanco<<-readRDS(fullpath)
    
    nomesvar<-colnames(nomebanco)
    
    nomesvar<<-NULL
    nomesvar<<-nomesvar[nomesvar != "Freq"]
    
  }
  
  # Primeiro output: a escolha da variável
  shinyServer(function(input, output) {
    
    output$datafiles <-renderUI({   
      selectInput("dataset", "Select your data", choices = c(None=".", datafiles ), selected=".")
    }) 
    getDataPath<- reactive({
      fullpath<-paste0('./data/', input$dataset)
    })
    getDatsetInfo<-function(fullpath)
    {
      datasetname<<-NULL
      datasetname<<-readRDS(fullpath)
      varnames<-colnames(datasetname)
      varnames<<-NULL
      varnames<<-varnames[varnames != "Freq"]
    }
    
    output$dependent<-renderUI({
      if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
      {
        fullpath<-getDataPath()
        getDatsetInfo(fullpath)                        
        selectInput("dependent", 
                    label = "Escolha uma variável",                   
                    choices<- c(None='.', varnames),
                    selected = '.'
        )
      } else {
        selectInput("dependent", 
                    label = "Escolha uma variável",
                    choices <- c(None='.'),
                    selected = '.'
        )
        
      }
      
    })
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    
    if (( !is.null(input$dataset) &  !identical(input$dataset, emptymatch) )
        & ( !identical(input$dependent, emptymatch) & !is.null(input$dependent) )
    )
    {    
      fullpath<-getDataPath()
      getDatsetInfo(fullpath) 
      summary(input$dependent)
    }
  })
  
}
  

# Create Shiny app ----
shinyApp(ui = ui, server = server)
