
library(shiny)
library(shinyjs)
library(readxl)
library(data.table)
library(tidyverse)
library(DT)
library(cowplot)
library( plotly)

theme_set( theme_cowplot(12) )

ui <- fluidPage(
    useShinyjs(),
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: grey;  color:black}
    .tabbable > .nav > li > a[data-value='Input Data'] {background-color: OrangeRed;   color:white}
    .tabbable > .nav > li > a[data-value='Data 2'] {background-color: blue;  color:white}
    .tabbable > .nav > li > a[data-value='Dataset'] {background-color: OrangeRed;   color:white}
    .tabbable > .nav > li > a[data-value='Plot'] {background-color: blue;  color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),

    # Application title
    titlePanel("App to make an interactive scatter plot from a csv or xlsx file."),
    hr(style = "border-top: 1px solid #000000;"),

    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                id = "sidebar",
                
                tabPanel(
                         title = "Input Data",
                         div( style = "color: OrangeRed;",
                    # div( style = "border-style: solid; border-color: green; padding:5px;",
            selectizeInput("input_file",
                                "Select a dataset or click 'Upload' to add your own.",
                           choices = NULL),
            hidden(
            fileInput("upload1", "Upload  File 1")),
            selectizeInput("xvar", "Select X Variable",
                           choices = NULL),
            selectizeInput("yvar", "Select Y Variable",
                           choices = NULL),
            selectizeInput("hovervar", "Add hovertext to points",
                           choices = NULL, multiple = T),
            selectizeInput("labvar", "Label points by column",
                           choices = NULL),
            selectizeInput("labval", "Label column values",
                           choices = NULL, multiple = T),

            
            h2("NOTES:"),
            p("- Plot will only show a maximum of 7500 data points. To change the data displayed, use the filter options at the top of Dataset table. You can also sort by clicking a given column name as the plot shows the FIRST 7500 data points.",style = "color:black ; font-weight: bold")

            
        )))),


        mainPanel(
            tabsetPanel(
                id = "Tables",
                tabPanel(title = "Dataset", dataTableOutput( "dt1")),
                tabPanel(title = "Plot", plotlyOutput( "scatter_plot", width = "1000px", height = "800px"))
            )
        )
    )
)

server <- function(input, output, session) {

    datasets <-
        reactive(
            data.table(
                filepath = c(
                    "data/pokemon1.xlsx",
                    "data/pokemon2.xlsx"
                ),
                dataset = c(
                    "Kaggle Pokemon Data",
                    "Cory Pokemon Data"
                    )
            )
        )
    observeEvent( 
        datasets(),{
        updateSelectizeInput(session, "input_file", 
                 choices = c( datasets()$dataset, "Upload" ) )
        }
    )
    
    file_n1 <- reactive({
            req(input$input_file)
            if(input$input_file != "Upload"){
                fp <- read_excel( datasets()[datasets()$dataset == input$input_file,]$filepath)
                fp
            } else if (!is.null(input$upload1)){
                if(str_detect( input$upload1$datapath, "xls|xlsx")){
                    fp <- read_excel( input$upload1$datapath )
                } else { 
                    fp <- fread( input$upload1$datapath )
                }
                fp
            } else {
                NULL
            }

            
        })
    
    observeEvent( 
        file_n1(),
        {             
            updateSelectizeInput(session, "xvar", 
                                 choices = c( colnames( file_n1())),
                                 selected = c( colnames( file_n1()))[[2]])

            updateSelectizeInput(session, "yvar", 
                                 choices = c( colnames( file_n1())),
                                 selected = c( colnames( file_n1()))[[3]])
            
            updateSelectizeInput(session, "hovervar", 
                                 choices = c( colnames( file_n1())))
            
            updateSelectizeInput(session, "labvar", 
                                 choices = c( colnames( file_n1())))
            
            
        })
    
    observeEvent(input$labvar, {
        
        req(input$labvar)
        
        updateSelectizeInput(session, "labval", 
                             choices = c( unique(file_n1()[[input$labvar]])),
                             server = TRUE)
    })
    
    
    output$scatter_plot <-
        renderPlotly({
            temp_dt <- file_n1()[input$dt1_rows_all,] %>%
                select( !!sym(input$xvar), !!sym(input$yvar), all_of(input$hovervar), !!sym(input$labvar)) %>%
                mutate( labz = case_when(
                    is.null( input$labval ) ~ NA_character_,
                    !!sym(input$labvar) %in% input$labval ~ as.character( !!sym(input$labvar)),
                    T ~ NA_character_
                ))  %>%
                head( n = 7500L)
            

            textlabs <- c()
            for(nvar in input$hovervar){
                
                textlabs <- paste(textlabs,paste(nvar,": ", temp_dt[[nvar]],"\n"))
            }
            
            
            p_temp <- 
                ggplot(
                    temp_dt,
                    aes( x = !!sym(input$xvar),
                         y = !!sym(input$yvar))
                ) +
                geom_point( aes( text = textlabs )) +
                geom_text( aes( label = labz ), colour = "red")
            
            
            
            ggplotly( p_temp, tooltip = c("all")) 
            
        })
    
    
    
    observeEvent(input$input_file, {
        if( input$input_file == "Upload"){
            shinyjs::show("upload1")  
            shinyjs::show("xvar")
            shinyjs::show("yvar")  
        } else {
            hide("upload1")
        }
        
    })
    

    output$dt1 <- renderDataTable(
        datatable( file_n1(), filter = 'top',
                   options = list(  # options
                       scrollX = TRUE, # allow user to scroll wide tables horizontally
                       search = list(regex = TRUE, 
                                     caseInsensitive = TRUE, 
                                     search = "")
                   )
                   )
      )
    
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
