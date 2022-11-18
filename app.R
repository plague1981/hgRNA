# Packages

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(rapportools)
library(openxlsx)


ui <- dashboardPage(
  dashboardHeader(title = 'Bowl\'s hgRNA project'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Data', tabName = 'start', icon = icon('chart-line')),
      menuItem("How to use", tabName = "guideline", icon = icon('map'))
    ),
    fileInput(inputId = 'file',label = 'Select input file:',multiple = FALSE),
    tableOutput('filename'),
    tags$hr(),
    actionButton(inputId = "analyze", label = "Get data")  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "guideline",
              navbarPage(title = 'About this website'),
              tags$div(
                tags$p('This website is customerized for WanYing\'s hgRNA real-PCR results analysis. Please make sure you have a correct format before you upload your file.')
              ),
              tags$h3('1. Check your file format'),
              tags$div(
                tags$p('Store your data in a different sheet with a different sheetname'),
                tags$br()
              ),
              tags$h3('2. Select and upload your file on the Sidebar')
      ),
      tabItem(tabName = 'start',
              navbarPage(title ='Data analysis',
                         tabPanel('Raw data', icon = icon('file'),
                           box( width = 10, title = 'Input data',
                                uiOutput('tables')
                           ),
                           box( width = 4,title = 'Select Control',
                                uiOutput('control_gene_names')
                           ),
                           box( width = 4, title = 'Select Target',
                                uiOutput('target_gene_names')
                           ),
                           box( width = 10, title = 'Mean dCt',
                                tableOutput('ave_dCt_mean')
                           )
                         ),
                         tabPanel( title = 'Set up control',
                           box( width = 4, title = 'Select Control Sample',
                                uiOutput('control_samples')
                           ),
                           box( width = 4, title = 'Control dCt table',
                                tableOutput('Control_dCt')
                           ),
                           box( width = 4, title = 'Control Mean dt',
                                textOutput('Control_Mean_dCt')
                           ),
                           box( width = 4, title = 'Control ddCt table',
                                tableOutput('control_ddCt_table')
                           ),
                           box( width = 10, title = 'Control samples raw data',
                                tableOutput('control_rawdata')
                           )
                         ),
                         tabPanel('Select samples', icon = icon('file'),
                          column( width = 12,
                           box( width = 4, title = 'Set up groups', 
                                numericInput("obs", "How many groups?:", 1, min = 1, max = 20)
                           ),
                           box( width = 4, title = 'Select samples for each group',
                                uiOutput('sample4group')
                           ),
                           box( width = 4, title = 'Sample ddCt table',
                                tableOutput('sample_ddCt_table')
                           ),
                           box( width = 10, title = 'Target samples table',
                                uiOutput('Sample_rawdata')
                           )
                          )
                         ),
                         tabPanel('Plot', icon = icon('map')
                           
                         )
              )
      )
    )
  ),
)


server <- function(input, output, session) {
  source('global.R', local = TRUE)  
  readfile<- eventReactive(input$analyze,{
    read_data(input$file$datapath)
  })
  sample_samples<-eventReactive(input$analyze,{
    sample_table<-openxlsx::read.xlsx(xlsxFile = input$file$datapath, sheet = 1)
    sample_samples<-colnames(sample_table)
    return(sample_samples)
  })
  genes<-eventReactive(input$analyze,{
    genes <-getSheetNames(input$file$datapath)
    return(genes)
  })
  ave_dCt_table<-eventReactive(input$analyze,{
    ave_dCt<-readfile()[[input$target_gene]]['mean',]-readfile()[[input$control_gene]]['mean',]
    return(ave_dCt)
  })
  Mean_dCt<-eventReactive(input$analyze,{
    as.numeric(mean(as.numeric(ave_dCt_table()[,input$control_samples])))
  })
# Session  
# show filename in the side bar
  output$filename<-renderTable({
    input$file$name 
  })
  output$control_gene_names <- renderUI({
    selectInput(inputId = "control_gene", label = "Choose Control:", choices = genes(), selected = genes()[1])
  })
  output$target_gene_names <- renderUI({
    selectInput(inputId = "target_gene", label = "Choose Target:", choices = genes(), selected = genes()[2])
  })

# Select samples for each group
  output$sample4group<-renderUI({
      group_number <- input$obs
      table_output_list <- lapply(1:group_number, function(i) {
      table_output_object <- plotlyOutput(group_number)
      table_output_object <- renderUI({
        checkboxGroupInput(inputId = paste0("group_",i), label = "Choose Samples:", choices = sample_samples(), selected = NULL)
      })
    })
    return(table_output_list)
  })
# Output rowdata
  output$tables<-renderUI({
    if (is.empty(input$file$name)){
      return(NULL)
    } else
    tablename <- getSheetNames(input$file$datapath)
    table_output_list <- lapply(1:length(tablename), function(i) {
      table_output_object <- plotlyOutput(tablename)
      table_output_object <- renderTable({
        readfile()[i]
      },rownames = TRUE)
    })
    return(table_output_list)
  })
  # Output ave_dCt_mean_table
  output$ave_dCt_mean<-renderTable(rownames = TRUE,colnames = TRUE,{
    if (is.null(input$target_gene)){
      NULL
    } else
    readfile()[[input$target_gene]]['mean',]-readfile()[[input$control_gene]]['mean',] 
  })
  # Select for control group
  output$control_samples <- renderUI({
    checkboxGroupInput(inputId = "control_samples", label = "Choose Control Samples:", choices = sample_samples(), selected = sample_samples()[1])
  })
  # Output Control dCt table
  output$Control_dCt<-renderTable(rownames = TRUE,colnames = TRUE,{
    if (is.null(input$control_samples)){
      NULL
    } else
    ave_dCt_table()[,input$control_samples]
  })
  # Output Control Ave dCt
  output$Control_Mean_dCt<-renderPrint({
    if (is.null(input$control_samples)){
      NULL
    } else
    mean(as.numeric(ave_dCt_table()[,input$control_samples]))
  })
  # Output Control ddCt and 2^ddCt
  output$control_ddCt_table<-renderTable(rownames = TRUE,{
    dCt_list<-ave_dCt_table()[,input$control_samples]
    ddCt<-NULL
    exprs<-NULL
    if (is.null(input$control_samples)){
      NULL
    } else
   for (n in 1:length(dCt_list)){
     ddCt<-c(ddCt,as.numeric(dCt_list[n])-mean(as.numeric(ave_dCt_table()[,input$control_samples])))
     exprs<-2^(-ddCt)
   }
    ddCt_list<-data.frame(ddCt)
    ddCt_list<-cbind(ddCt_list,exprs)
    ddCt_list<-data.frame(t(ddCt_list))
    colnames(ddCt_list)<-input$control_samples
    return(ddCt_list)
  })
  # Output Control raw data
  output$control_rawdata<-renderUI({
    if (is.empty(input$file$name)){
      return(NULL)
    } else
      tablename <- getSheetNames(input$file$datapath)
    table_output_list <- lapply(1:length(tablename), function(i) {
      table_output_object <- plotlyOutput(tablename)
      table_output_object <- renderTable({
        rawdata<-readfile()[i]
        data.frame(rawdata)[,paste0(tablename[i],'.',input$control_samples)]
      },rownames = TRUE)
    })
    return(table_output_list)
  })
  
  output$control_samples_table<-renderTable(rownames = TRUE,colnames = TRUE,{
    readfile()[[input$control_gene]][,input$control_samples]
  })
  
  output$sample_ddCt_table<-renderTable(rownames = TRUE,{
    dCt_list<-ave_dCt_table()[,input$group_1]
    ddCt<-NULL
    exprs<-NULL
    if (is.null(input$group_1)){
      NULL
    } else
      for (n in 1:length(dCt_list)){
        ddCt<-c(ddCt,as.numeric(dCt_list[n])-mean(as.numeric(ave_dCt_table()[,input$control_samples])))
        exprs<-2^(-ddCt)
      }
    ddCt_list<-data.frame(ddCt)
    ddCt_list<-cbind(ddCt_list,exprs)
    ddCt_list<-data.frame(t(ddCt_list))
    colnames(ddCt_list)<-input$group_1
    return(ddCt_list)
  })
  # Output Samples raw data
  output$Sample_rawdata<-renderUI({
    if (is.null(input$group_1)){
      return(NULL)
    } else
      tablename <- getSheetNames(input$file$datapath)
    table_output_list <- lapply(1:length(tablename), function(i) {
      table_output_object <- plotlyOutput(tablename)
      table_output_object <- renderTable({
        rawdata<-readfile()[i]
        data.frame(rawdata)[,paste0(tablename[i],'.',input$group_1)]
      },rownames = TRUE)
    })
    return(table_output_list)
  })

}

shinyApp(ui, server)


