library(shiny)
library(ggplot2)
library(Hmisc)
library(xtable)
library(hwriter)
library(MASS)

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
      #                uiOutput("independent"),
      #                uiOutput("control"),
      #                uiOutput("subsetValue")
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

## get a list of datasets
cd<-getwd();
source("./scripts/crosstabs.R")
setwd(cd)

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
  
  output$independent<-renderUI({ 
    
    if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
    {
      fullpath<-getDataPath()
      getDatsetInfo(fullpath)                        
      
      selectInput("independent", 
                  label = "Choose an independent variable ",
                  choices<- c(Nenhum='.', varnames),
                  selected = '.'
      )
    } else {
      
      selectInput("independent", 
                  label = "Choose an independent variable ",
                  choices= c(Nenhum='.'),
                  selected = '.'
      )
      
    }
  })
  
  output$control<-renderUI({ 
    
    if (!is.null(input$dataset) & !identical(input$dataset, emptymatch )
        
    )
    {
      fullpath<-getDataPath()
      getDatsetInfo(fullpath)
      
      selectInput("control", 
                  label = "Choose a control variable ",
                  choices<- c(Nenhum='.', varnames),
                  selected = '.'
      )
    } else {
      selectInput("control", 
                  label = "Choose a control variable ",
                  choices<- c(Nenhum='.'),
                  selected = '.'
      )
      
    }
  })
  
  output$subsetValue<-
    renderUI({
      if ((!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
          & (!is.null(input$control) & !identical(input$control, emptymatch ))
          
      )
      {
        
        
        subset<-datasetname[[input$control]]
        subsetValues<-names(table(subset[1]))
        
        selectInput("subsetValue", 
                    label = "Choose a subset within the control",                   
                    choices<- c(Nenhum='.', subsetValues),
                    selected = '.'
        )
        
      } else {
        selectInput("subsetValue", 
                    label = "Choose a subset within the control",
                    choices<- c(Nenhum='.'),
                    selected = '.'
        )
        
      }
      
    })
  ## Add something here to check for invalid combinations and return a message?)
  
  output$table <- renderTable({
    if (( !is.null(input$dataset) &  !identical(input$dataset, emptymatch) )
        & ( !identical(input$dependent, emptymatch) & !is.null(input$dependent) )
        
    )
    {       
      fullpath<-getDataPath()
      getDatsetInfo(fullpath)                        
      
      # For now if there is a control variable, subset based on the value selected
      # This is ugly but it works with xtable which shiny depends on.
      if (!is.null(input$subsetValue) & !identical(input$subsetValue, emptymatch) 
          & !is.null(input$control) & !identical(input$control, emptymatch)
      )
      {
        datasetnamec<-datasetname[datasetname[[input$control]] == input$subsetValue,]
        
      }
      
      # Must be in the correct order.
      factorsToUse<-c(input$dependent, input$independent)
      
      factorsToUse<-factorsToUse[factorsToUse != "."]
      ctab<<-NULL
      
      if (!exists("datasetnamec"))
      {
        
        ctab<<-creatextab( factorsToUse, datasetname)
      } else {
        ctab<-creatextab( factorsToUse, datasetnamec)
      }
      dimctab<-dim(ctab)
      
      ndims<<-NULL
      ndims<<-length(dimctab)
      
      if (ndims == 2)
      {
        cnames<-colnames(ctab)
        ncols<-dimctab[2]
        
        ctab<-prop.table(ctab,2)*100
        # Because of datasetc
        ctab<<-ctab
        
        
        
      } else if (ndims == 1)
      {
        
        parte1 <- as.data.frame(prop.table(ctab)*100)
        parte2 <- as.data.frame(round(prop.table(ctab)*nrow(datasetname),digits = 0))
        
        ctab <- cbind(parte1,parte2[,2])
        
        names(ctab) <- c("Categorias","Freq","N")
        
        ctab<<-ctab
      }                 
      
      
      return(ctab)
      
    } 
    
  })
  
  output$plot<-renderPlot({
    
    if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ) & !is.null(ndims))
    {
      if (!identical(input$dependent, emptymatch ) & identical(input$independent, emptymatch ) 
          & identical(input$subsetValue, emptymatch )
          & identical(input$control, emptymatch )
          
          & ndims == 1)
      {
        plot<-barplot(ctab, beside = TRUE)
      } else if ((ndims == 2 &  (!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
                  | (!is.null(input$independent) & !identical(input$independent,emptymatch) ))
                 & (identical(input$subValue,emptymatch) | is.null(input$subValue))
                 
      ) 
        
      {
        
        plot<-barplot(ctab, beside = FALSE)
        ndims<<-NULL
        ctab<<-NULL
      } else if (
        ((!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
         & (!is.null(input$independent) & identical(input$independant,emptymatch))
         & (!is.null(input$control) & !identical(input$control,emptymatch))
         & !identical(input$subvalue,emptymatch) & ndims == 2)
      )
      {
        
        
        ## Change this to show grouped plot?
        plot<-barplot(ctab, beside = FALSE)
        ndims<<-NULL
        ctab<<-NULL
      }
      
      plot
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
    
    if (!is.null(input$independent) & !identical(input$independent, emptymatch))
    {
      title<-paste(title,"by", input$independent, "<br/>", sep=" ")
      
    }
    if (!is.null(input$control) & !identical(input$control, emptymatch))
    {
      title<-paste0(title, "Controlling for ", input$control, " = ", input$subsetValue, "<br/>")      
    }
    if (title != "")
    {
      title<-paste0(title, " (%)</h4>")
      title<-gsub("_", " ", title)
    }
    
  })
  
}   




# Create Shiny app ----
shinyApp(ui = ui, server = server)