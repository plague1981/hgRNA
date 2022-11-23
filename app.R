# Packages

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(rapportools)
library(openxlsx)
library(rsconnect)
library(colourpicker)

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
                         tabPanel('Plot', icon = icon('map'),
                           box( width = 1.5, title = 'Relative Expression',
                                #actionButton(inputId = "get_plot", label = "Get Plot"),
                                tableOutput('table_data')
                           ),
                           box( width = 5, title = 'Parameters',
                                textInput(inputId = "plot_title", label = "Title",value = 'Title'),
                                textInput(inputId = "x_axis", label = "x-axis", value = "x-axis"),
                                textInput(inputId = "y_axis", label = "y-axis", value = "y-axis"),
                                sliderInput(inputId = 'shape_control', label = 'control shape',min = 0,max = 26,value = 15),
                                sliderInput(inputId = 'shape_sample', label = 'sample shape',min = 0,max = 26,value = 16),
                                #textInput(inputId = "shape_control", label = "control shape (0-26)", value = 15),
                                #textInput(inputId = "shape_sample", label = "sample shape (0-26)", value = 16),
                                actionButton(inputId = "get_plot", label = "Get Plot")
                           ),
                           box( width = 5, title = 'legend color',
                             div("Selected colour:", textOutput("value", inline = TRUE)),
                             colourInput("color_control", "Choose control color", "black"),
                             colourInput("color_sample", "Choose control color", "black"),
                             #colourInput("col", "Choose color", "red"),
                             #h3("Update colour input"),
                             #textInput("text", "New colour: (colour name or HEX value)"),
                             #selectInput("showColour", "Show colour", c("both", "text", "background")),
                             #checkboxInput("allowTransparent", "Allow transparent", FALSE),
                             #checkboxInput("returnName", "Return R colour name", FALSE),
                             #actionButton("btn", "Update")
                           ),
                           box( width = 5, title = 'Plot',
                             plotlyOutput('point_plot')
                           )
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
  Mean_dCt<-eventReactive(input$control_samples,{
    as.numeric(mean(as.numeric(ave_dCt_table()[,input$control_samples])))
  })
  control_ddCt_table<-eventReactive(input$control_samples,{
    return(ddCt_table(ave_dCt_table(),input$control_samples))
  })
  sample_ddCt_table<-eventReactive(input$group_1,{
    return(ddCt_table(ave_dCt_table(),input$group_1))
  })
  df<-eventReactive(input$get_plot,{
    df<-exprs_table(readfile(),input$control_gene,input$target_gene,input$control_samples,input$group_1)
    return(df)
  })
  table_data<-eventReactive(input$get_plot,{
    table_data(df())
  })
  
# Session
  observeEvent(input$btn, {
    updateColourInput(session, "col",
                      #value = input$text, 
                      showColour = "both"
                      #allowTransparent = input$allowTransparent,
                      #returnName = input$returnName
                      )
  })
  output$value <- renderText(input$col)
  
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
    return(ddCt_table(ave_dCt_table(),input$control_samples))
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
  # Output Sample ddCt table
  output$sample_ddCt_table<-renderTable(rownames = TRUE,{
    return(ddCt_table(ave_dCt_table(),input$group_1))
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
  output$table_data<-renderTable({
    express_table(df())
  })
  output$point_plot<-renderPlotly({
    point_plot(df())
  })

}

shinyApp(ui, server)

