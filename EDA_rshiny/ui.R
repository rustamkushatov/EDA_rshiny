##########################################
####         Shiny ui                 ####
##########################################
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(DT)
library(heatmaply)
library(Hmisc)
library(pastecs)


dashboardPage( skin = "purple",
  dashboardHeader(title = "Exploratory analysis", titleWidth = 300),
  dashboardSidebar( width = 300,
    sidebarMenu(
      id = "sidebar",
      
      ###############################
      ######   Page with table   ####
      
      menuItem("Table", tabName = "table", icon  =  icon ("table")),
      conditionalPanel(
        'input.sidebar == "table"',
        fileInput(inputId = "file", "File input (CSV)"),  #File upload
        pickerInput(inputId = "choiceColumns",                            #Drop down menu with a lot of options
                    label = "Select columns",
                    choices = "",
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    choicesOpt = list(
                      style = c("color: steelblue; font-size: 150%;", 
                                "color: firebrick; text-align: right;", "font-weight: bold;", 
                                "background: forestgreen; color: white;"))
                    )
      ),
      
      
      ########################
      ###### Resume page #####
      
      menuItem(
        "Summary",
        tabName = "summary",
        icon = icon("list-alt")
      ),
      conditionalPanel(
        'input.sidebar == "summary"',
        awesomeRadio(
          inputId = "funcDescriptive",
          label = "Select option",
          choices = c("option 1", "option 2", "option 3","option 4"),
          selected = "option 1",
          inline = F,
          checkbox = T
        ),
        downloadButton('save', 'Save')
        
      ),
      
      ########################
      ###### Plot #####
      
      menuItem(
        "Plot",
        tabName = "plot",
        icon = icon("bar-chart-o"),
        menuSubItem("Scatter Plots", tabName = "Scatter_Plots"),
        menuSubItem("Historgam", tabName = "Historgam"),
        menuSubItem("Boxplot",tabName = "Boxplot"),
        menuSubItem("Heatmap",tabName = "Heatmap"),
        menuSubItem("Pie",tabName = "Pie")
      ),
      
      #########################
      ###### Scatter plots ####
      
      conditionalPanel(
        'input.sidebar == "Scatter_Plots"',
        selectInput(
          inputId = "scatter_x", label = "Select x", choices = ""
        ),
        selectInput(
          inputId = "scatter_y", label = "Select y", choices = ""
        ),
        selectInput(
          inputId = "scatter_color", label = "Select color", choices = ""
        ),
        awesomeCheckbox(
          inputId = "layout_sctatter",
          label = "Layout", 
          value = F,
          status = "succes"
        )
      ),
  
      conditionalPanel(
        'input.layout_sctatter==1 && input.sidebar == "Scatter_Plots"',
        textInput("scatter_title","Title","Sctatter plot"),
        textInput("scatter_xaxis","X Title",""),
        textInput("scatter_yaxis","Y Title","")
      ),

      #########################
      ######  Histogram   ####
      
      conditionalPanel(
        'input.sidebar == "Historgam"',
        selectInput(
          inputId = "hist_x", label = "Select x", choices = ""
        ),
        selectInput(
          inputId = "hist_2", label = "Select color", choices = ""
        ),
        awesomeCheckbox(
          inputId = "layout_histogram",
          label = "Layout", 
          value = F,
          status = "succes"
        )
      ),
      
      conditionalPanel(
        'input.layout_histogram==1 && input.sidebar == "Historgam"',
        textInput("histogram_title","Title","Histogram"),
        textInput("histogram_xaxis","X Title","")
      ),
      
      #########################
      ######  Boxplot      ####
      
      conditionalPanel(
        'input.sidebar == "Boxplot"',
        selectInput(
          inputId = "boxplot_x", label = "Select x", choices = ""
        ),
        selectInput(
          inputId = "boxplot_y", label = "Select y", choices = ""
        ),
        awesomeCheckbox(
          inputId = "layout_boxplot",
          label = "Layout", 
          value = F,
          status = "succes"
        )
      ),
      
      conditionalPanel(
        'input.layout_boxplot==1 && input.sidebar == "Boxplot"',
        textInput("boxplot_title","Title","Boxplot"),
        textInput("boxplot_xaxis","Title_X",""),
        textInput("boxplot_yaxis","Title_Y","")
      ),
      
      #########################
      ######  Heatmap      ####
      
      conditionalPanel(
        'input.sidebar == "Heatmap"',
        awesomeRadio(
          inputId = "options_heatmap",
          label = "Options", 
          choices = c("Layout", "Color", "Transform"),
          inline = TRUE,
          status = "succes"
        )
      ),
      
      conditionalPanel(
        'input.options_heatmap=="Layout" && input.sidebar == "Heatmap"',
        textInput("heatmap_title","Title","Heatmap"),
        textInput("heatmap_xlab","Title_X",""),
        textInput("heatmap_ylab","Title_Y",""),
        sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
        sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180)
      ),
      
      conditionalPanel(
        'input.options_heatmap=="Color" && input.sidebar == "Heatmap"',
        selectInput("Color_select_heatmap","Select color heatmap", 
                    c("default","correlation","is.na10","normalize","percentize"),
                    selected = "default")
      ),
      
      conditionalPanel(
        'input.options_heatmap=="Transform" && input.sidebar == "Heatmap"',
        selectInput("Trans_select","Select transform", 
                    c("default","correlation","is.na10","normalize","percentize"),
                    selected = "default")
      ),
      
      #########################
      ######  Pie         ####
      
      conditionalPanel(
        'input.sidebar == "Pie"',
        selectInput(
          inputId = "pie_labels", label = "Select labels", choices = ""
        ),
        awesomeCheckbox(
          inputId = "layout_pie",
          label = "Layout", 
          value = F,
          status = "succes"
        )
      ),
      
      conditionalPanel(
        'input.layout_pie==1 && input.sidebar == "Pie"',
        textInput("pie_title","Title","Pie chart")
      ),
      
      menuItem(
        "About a program",
        tabName = "AboutPO",
        icon = icon("fa-regular fa-book-open")
      )
    )
  ),
  
  dashboardBody(tags$head(
    tags$script(
      HTML("
          window.onload = function() {
            resize();
          }
          window.onresize = function() {
            resize();
          }
          Shiny.addCustomMessageHandler ('triggerResize',function (val) {
            window.dispatchEvent(new Event('resize'));
          });
          function resize(){
            var h = window.innerHeight - $('.navbar').height() - 150;//Get dashboardBody height
            $('#box').height(h); 
            $('#box1').height(h); 
            $('#box2').height(h);
            $('#box3').height(h);
            $('#box4').height(h);
          }"
      )
    )
  ),
    tabItems(
      tabItem(
        tabName = "table",
        dataTableOutput("table")
        
      ),
      tabItem(
        tabName = "summary",
        verbatimTextOutput("summary")
      ),
      tabItem(
        tabName = "Scatter_Plots",
        box( id ="box",
             width  = 12, 
             height = "100%",
             solidHeader = FALSE, 
             status = "primary",
             plotlyOutput("scatter",inline=F,width="100%",height="100%")
        ),
      ),
      tabItem(
        tabName = "Historgam",
        box( id ="box1",
             width  = 12, 
             height = "100%",
             solidHeader = FALSE, 
             status = "primary",
             plotlyOutput("histogram",inline=F,width="100%",height="100%")
        ),
      ),
      tabItem(
        tabName = "Boxplot",
        box( id ="box2",
             width  = 12, 
             height = "100%",
             solidHeader = FALSE, 
             status = "primary",
             plotlyOutput("boxplot",inline=F,width="100%",height="100%")
        ),
      ),
      tabItem(
        tabName = "Heatmap",
        box( id ="box3",
             width  = 12, 
             height = "100%",
             solidHeader = FALSE, 
             status = "primary",
             plotlyOutput("heatmap",inline=F,width="100%",height="100%")
        ),
      ),
      tabItem(
        tabName = "Pie",
        box( id ="box4",
             width  = 12, 
             height = "100%",
             solidHeader = FALSE, 
             status = "primary",
             plotlyOutput("pie",inline=F,width="100%",height="100%")
        ),
      ),
      
      tabItem(
        tabName = "AboutPO",
        box(id = "infobox1",
            title = "Welcome to the program for exploratory data analysis",
            width = NULL, solidHeader = TRUE, status = "primary",
            "Exploratory analysis (EDA) is the apex of data analysis in order to 
            identify the most common dependencies, patterns, trends, the nature of the 
            analyzed data, the laws of distribution of the analyzed quantities.
            It can include basic descriptive statistics, correlation analysis, and visualization using graphs."),
        fluidRow(
          column(width = 4,
                 box(
                   title = "Table", width = NULL, solidHeader = TRUE, status = "warning",
                   "On the table tab, you need to load data and select variables for analysis. 
                   A table for the initial inspection will be displayed on the right in the main window"
                 )
          ),
          
          column(width = 4,
                 box(
                   title = "Summary", width = NULL, solidHeader = TRUE, status = "warning",
                   "The summary tab allows you to view descriptive statistics"
                 )
          ),
          column(width = 4,
                 box(
                   title = "Plot", width = NULL, solidHeader = TRUE, status = "warning",
                   "the Plot tab contains a set of graphs for EDA"
                 )
          )
          
        )
      )
    )
    
  )
)
