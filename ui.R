##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "StudentPerformance",
  theme = shinytheme("flatly"),
  tabPanel(
    "Dashboard",
    # App title ----
    titlePanel(div(
      windowTitle = "StudentPerformance",
      img(src = "sg1.png", width = "100%", class = "bg"),
    )),
    
    tags$br(),
    
    
    ##########################################
    ####  Panel: Main>Summary             ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        ################################################
        #### Panel: Main>Summary>Tables & Pie Chart ####
        ################################################
        
        # ------------------
        # ranking 
        # ------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Data by Week"),
            tags$br(),
            selectInput(
              "checkweek",
              "Select Week",
              choices = list("week 1", "week 2", "week 3",
                             "week 4", "week 5", "week 6"),
              selected = "Week 7"
            )
          ),
          
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Ranking", DT::dataTableOutput("datahead"))
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
        
        
        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------
            
            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "testmarkrange",
              label = "Test Mark Range",
              min = 1,
              max = 100,
              value = c(1, 100)
            ),
            setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
            sliderInput(
              "assignmentmarkrange",
              label = "Assignment Mark Range",
              min = 0,
              max = 100,
              value = c(0, 100)
            ),
            selectInput(
              "checkweekgroup",
              "Select Week",
              choices = list("week 1", "week 2", "week 3",
                             "week 4", "week 5", "week 6"),
              # selected = "week 7",
              multiple = TRUE
            ),
            
            actionButton("actionDT", "Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Main>Plots                      ####
      ################################################
      
      tabPanel(
        "Visual Comparison",
        
        # --------------------
        # density plot section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Density Plot Panel"),
            tags$br(),
            selectInput(
              "selectvar",
              label = "Choose a variable to display",
              choices = c(
                "Class Participation" = "class_participation",
                "Attendance" = "attendance"
              ),
              selected = "Class Participation"
            ),
            
            checkboxGroupInput(
              "checkGroup",
              label = "Select Student",
              choices = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              ),
              selected = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              )
            ),
          ),
          mainPanel(
            h3("Distribution"),
            plotlyOutput(outputId = "densityPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # bar plot section 
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel - Assignment"),
            tags$br(),
            radioButtons(
              "radio",
              label = "Select Student",
              choices = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              ),
              selected = "Shiv"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Assignment Marks"),
            plotlyOutput(outputId = "uniPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
       
        
        # --------------------
        # bar plot section 
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel - Test"),
            tags$br(),
            radioButtons(
              "radio1",
              label = "Select Student",
              choices = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              ),
              selected = "Shiv"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Test Marks"),
            plotlyOutput(outputId = "uniPlot1"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # bar plot section 
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel - Class Participation"),
            tags$br(),
            radioButtons(
              "radio2",
              label = "Select Student",
              choices = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              ),
              selected = "Shiv"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Class Performance"),
            plotlyOutput(outputId = "uniPlot2"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # box plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Box Plot Panel"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupbox",
              label = "Select Student",
              choices = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              ),
              selected = list(
                "Shiv" = "Shiv",
                "Aarush" = "Aarush",
                "Ayaan" = "Ayaan",
                "Arjun" = "Arjun",
                "Atharv" = "Atharv",
                "Reyansh" = "Reyansh",
                "Dhruv" = "Dhruv",
                "Anika" = "Anika",
                "Vihaan" = "Vihaan",
                "Kabir"= "Kabir"
              )
            ),
            
            tags$hr()
          ),
          mainPanel(
            h3("Attendance"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br(),
            tags$br(),
          )
        ),
        
        tags$hr(),
        
        # --------------------
        # Scatter plot section
        # --------------------
        
        
        fluidPage(fluidRow(
          h3("Assignment Marks vs Test Results"),
          align = "center",
          plotlyOutput(outputId = "scatPlot", width = "100%"),
          div(style = "height:400px")
        )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        
      ),
      )
    )
)
