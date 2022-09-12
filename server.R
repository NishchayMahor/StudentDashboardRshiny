##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)

library(rsconnect)
library(shinythemes)

##########################################
####   Attaching datasets             ####
##########################################

data <- read.csv("student_data1.csv")

#data <- readRDS("data/employment_data.rds")
#data_g <- readRDS("data/graduates_by_institutions.rds")


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  ################################################
  #### Panel: Main>Summary>Tables ####
  ################################################
  
  # ----------------
  # Summary section
  # ----------------
  a<-reactive({
    req(data)
    data %>%
      filter(week == input$checkweek) %>%
      select(contains(c("student_name",
             "assignment_marks",
             "test_marks"))) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(test_marks))
  })
  output$datahead <- DT::renderDataTable({
   req(a())
    a()
  })
  
  # ----------------
  # pie plot section
  # ----------------
  
  # output$piePlot <- renderPlot({
  #   colmap <-
  #     c(
  #       "#bdb2ff",
  #       # NUS
  #       "#ffc6ff",
  #       # NTU
  #       "#fffffc",
  #       # SMU
  #       "#33658A",
  #       # SIT
  #       "#3a506b",
  #       # SUTD
  #       "#577590",
  #       # SUSS
  #       "#43aa8b",
  #       # NIE
  #       "#90be6d",
  #       # SP
  #       "#f9c74f",
  #       # NP
  #       "#f8961e",
  #       # TP
  #       "#f3722c",
  #       # NAYANG POLY
  #       "#f94144",
  #       # RP
  #       "#ffadad",
  #       # NAFA DEG
  #       "#ffd6a5",
  #       # LAS DEG
  #       "#fdffb6",
  #       # NAFA DIP
  #       "#caffbf",
  #       # NAFA DEG
  #       "#a8dadc"  # ITE
  #     )
  #   
  #   data %>%
  #     filter(week == input$checkweek) %>%
  #     group_by(subject) %>%
  #     tally(test_marks) %>%
  #     ggplot(aes(x = "", y = n, fill = test_marks)) +
  #     geom_bar(
  #       stat = "identity",
  #       width = 1,
  #       color = "black",
  #       size = 1
  #     ) +
  #     theme_void() +
  #     theme(legend.position = "right",
  #           plot.title = element_text(hjust = 0.5, size = 14)) +
  #     coord_polar("y", start = 0) +
  #     scale_fill_manual(values = c(colmap)) +
  #     labs(title = "Subject and Test Marks")
  #   
  # })
  
  # ------------------
  # data table section
  # ------------------
  
  # filter the checkgroup input:
  
  weekGroup <- reactive({
    input$actionDT
    isolate(return(data[data$week %in% input$checkweekgroup, ]))
  })
  
  
  filtered_DT <- reactive({
    input$actionDT
    isolate({
      mintestmarks <- input$testmarkrange[1]
      maxtestmarks <- input$testmarkrange[2]
      minassignmentmarks <- input$assignmentmarkrange[1]
      maxassignmentmarks <- input$assignmentmarkrange[2]
    })
    
    weekGroup() %>%
      filter(test_marks > mintestmarks,
             test_marks < maxtestmarks) %>%
      filter(assignment_marks > minassignmentmarks,
             assignment_marks < maxassignmentmarks) %>%
      select(1, 2, 3, 4, 5, 6)
  })
  
  # render DT:
  output$myTable <- renderDataTable({
    filtered_DT() %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "week",
          "student_name",
          "attendance_marks",
          "assignment_marks",
          "project_marks",
          "class_participation"
        )
      )
    
  })
  
  # output$myTable <- renderDataTable({
  #   filtered_DT() %>%
  #     datatable(
  #       rownames = FALSE,
  #       class = "table",
  #       options = list(pageLength = 10, scrollX = T),
  #       colnames = c(
  #         "week",
  #         "student_name",
  #         "attendance_marks",
  #         "assignment_marks",
  #         "project_marks",
  #         "class_participation"
  #       )
  #     )
  #   
  # })
  
  
  ################################################
  #### Panel: Main>Plots                      ####
  ################################################
  
  # --------------------
  # density plot section
  # --------------------
  
  # filter the checkgroup input:
  
  dent <-  reactive({
    return(data[data$student_name %in% input$checkGroup, ])
    
  })
  
  # render density plot
  
  output$densityPlot <- renderPlotly({
    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60",
                "#2d502c",
                "#50362c",
                "#502c2c",
                "#372c50")
    
    ggplotly(
      ggplot(data = dent(), aes_string(x = input$selectvar)) +
        geom_density(aes(fill = student_name), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = input$selectvar) +
        scale_fill_manual(values = colmap) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))
    
  })
  
  
  # ----------------
  # bar plot section
  # ----------------
  
  # filter the input and group data:
  
  output$uniPlot <- renderPlotly({
    monthly <- data %>%
    filter(student_name == input$radio) %>%
    group_by(week)
    #summarise_at(.vars = names(.)[3:4], .funs = c(mean = "mean"))
    
    # render bar plot
    
    colmap <-
      c(
        "#2c3e50",
        "#e67e22",
        "#f1c40f",
        "#e74c3c",
        "#F97F51",
        "#27ae60",
        "#2980b9",
        "#86BBD8",
        "#8e44ad",
        "#95a5a6",
        "#f39c12",
        "#d35400",
        "#c0392b",
        "#bdc3c7",
        "#D6A2E8",
        "#25CCF7",
        "#16a085"
      )
    
    p <- monthly %>%
      data.frame() %>%
      ggplot(.,
             aes(
               x = reorder(week, assignment_marks),
               y = assignment_marks,
               fill = student_name
             )) +
      geom_bar(
        stat = "identity",
        width = 0.5,
        color = "black",
        size = 1
      ) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    p <- ggplotly(p + coord_flip(), tooltip = ("assignment_test_marks"))
    hide_legend(p)
    
  })
  
  # ----------------
  # bar plot section
  # ----------------
  
  # filter the input and group data:
  
  output$uniPlot1 <- renderPlotly({
    monthly <- data %>%
      filter(student_name == input$radio1) %>%
      group_by(week)
    #summarise_at(.vars = names(.)[3:4], .funs = c(mean = "mean"))
    
    # render bar plot
    
    colmap <-
      c(
        "#2c3e50",
        "#e67e22",
        "#f1c40f",
        "#e74c3c",
        "#F97F51",
        "#27ae60",
        "#2980b9",
        "#86BBD8",
        "#8e44ad",
        "#95a5a6",
        "#f39c12",
        "#d35400",
        "#c0392b",
        "#bdc3c7",
        "#D6A2E8",
        "#25CCF7",
        "#16a085"
      )
    
    p <- monthly %>%
      data.frame() %>%
      ggplot(.,
             aes(
               x = reorder(week, test_marks),
               y = test_marks,
               fill = student_name
             )) +
      geom_bar(
        stat = "identity",
        width = 0.5,
        color = "black",
        size = 1
      ) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    p <- ggplotly(p + coord_flip(), tooltip = ("assignment_test_marks"))
    hide_legend(p)
    
  })
  
  # ----------------
  # bar plot section
  # ----------------
  
  # filter the input and group data:
  
  output$uniPlot2 <- renderPlotly({
    monthly <- data %>%
      filter(student_name == input$radio2) %>%
      group_by(week)
    #summarise_at(.vars = names(.)[3:4], .funs = c(mean = "mean"))
    
    # render bar plot
    
    colmap <-
      c(
        "#2c3e50",
        "#e67e22",
        "#f1c40f",
        "#e74c3c",
        "#F97F51",
        "#27ae60",
        "#2980b9",
        "#86BBD8",
        "#8e44ad",
        "#95a5a6",
        "#f39c12",
        "#d35400",
        "#c0392b",
        "#bdc3c7",
        "#D6A2E8",
        "#25CCF7",
        "#16a085"
      )
    
    p <- monthly %>%
      data.frame() %>%
      ggplot(.,
             aes(
               x = reorder(week, class_participation),
               y = class_participation,
               fill = student_name
             )) +
      geom_bar(
        stat = "identity",
        width = 0.5,
        color = "black",
        size = 1
      ) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    p <- ggplotly(p + coord_flip(), tooltip = ("assignment_test_marks"))
    hide_legend(p)
    
  })
  
  # ----------------
  # box plot section
  # ----------------
  
  
  
  
  # filter the checkgroup input:
  
  uniMedian <-  reactive({
    return(data[data$student_name%in%input$checkGroupbox, ])
  })
  
  # render box plot
  
  output$boxPlot <- renderPlotly({
    
    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60",
                "#2980b9",
                "#86BBD8",
                "#8e44ad",
                "#95a5a6",
                "#f39c12",
                "#d35400",
                "#c0392b",
                "#bdc3c7",
                "#D6A2E8",
                "#25CCF7",
                "#16a085")
    
    p <- ggplot(data = uniMedian(),
             aes(x = student_name, y = attendance, fill = student_name)) +
      geom_boxplot(color = "black",
                   size = 1,
                   width = 0.3) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    p <- ggplotly(p + coord_flip(), tooltip = ("attendance"))
    hide_legend(p)
    
  })
  
  
  # ----------------
  # scatter plot section
  # ----------------
  
  output$scatPlot <- renderPlotly({
    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60",
                "#2980b9",
                "#86BBD8",
                "#8e44ad",
                "#95a5a6",
                "#f39c12",
                "#d35400",
                "#c0392b",
                "#bdc3c7",
                "#D6A2E8",
                "#25CCF7",
                "#16a085")
    
    data <- data %>% filter(week=="week 6")
    
    p <-
      ggplot(
        data,
        aes(
          x = assignment_marks,
          y = test_marks,
          color = student_name,
          shape = as.factor(week)
        )
      ) +
      geom_point(size = 3, alpha = 0.7) +
      scale_colour_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    ggplotly(
      p,
      tooltip = c(
        "week",
        "assignment_marks",
        "university",
        "test_marks"
      ),
      height = 800
    )
    
  })
  
  }
