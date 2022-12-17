library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Projeto Temático"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      helpText("Escolha banco de dados e variável"),
      uiOutput("datafiles"),
      
      uiOutput("dependent")

    ),
    
    mainPanel(
      htmlOutput("title"),
      tabsetPanel(     
        
        tabPanel("Tabela",
                 
                 htmlOutput("table"),
                 
                 htmlOutput("summary")
        ),
        tabPanel("Questionário do Preliminar",
                 includeMarkdown("./docs/QuestionarioPreliminar.Rmd")
        ),
        tabPanel("Questionário da Primeira Onda",
                 includeMarkdown("./docs/QuestionarioPrimeiraOnda.Rmd")         
        )
        
      )
      
    ))
)


# Lê bancos de dados e informações -------------------------------------------------------

if (file.exists("./datasetlist.csv") )
{
  ## datasetname, title, description
  datasetlist<-read.csv("./datasetlist.csv", stringsAsFactors = FALSE)
} else {
  "Banco de dados ausente."
  return()
}

datafiles<-datasetlist$datasetname
datafiles<-setNames(datafiles, datasetlist$title)

emptymatch<-"."






# Define server logic to summarize and view selected dataset -------------------------------------------
server <- function(input, output) {
  
  output$datafiles <-renderUI({   
    selectInput("dataset", "Selecione o banco de dados:", choices = c(Nenhum=".", datafiles ), selected=".")
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
                  label = "Escolha uma variável:",                   
                  choices<- c(Nenhum='.', varnames),
                  selected = '.'
      )
    } else {
      selectInput("dependent", 
                  label = "Escolha uma variável: ",
                  choices<- c(Nenhum='.'),
                  selected = '.'
      )
      
    }
    
  })
  
  
  output$summary<-renderText({
    
    if (!is.null(input$dataset) & !identical(input$dataset, emptymatch))
    {      
      selectedData<-datasetlist[datasetlist$datasetname == input$dataset,]
      #To start off these are set to null.
      ndims<<-NULL
      ctab<<-NULL
      
      text1<-paste("<h3> <br><br> Informações sobre este banco de dados</h3>")
      ## Include an HTML description in the data file.
      text4<-paste0("<h4>",as.character(selectedData["title"]), "</h4>")
      text2<-as.character(selectedData["dataDescription"])
      text3<-paste(text1, text4, text2, sep="<br><br> ")
      
      return(text3)
    } else {
      "<h5><p><br>Por favor, escolha um banco de dados.</p><h5>"
      
    }
    
  }) 
  output$title<-renderText({
    title<-""
    if (!identical(input$dependent, emptymatch) & !is.null(input$dependent) )
    {
      title<-paste0("<h4>",input$dependent)
    }
   })
  
}   



# Create Shiny app ----
shinyApp(ui = ui, server = server)