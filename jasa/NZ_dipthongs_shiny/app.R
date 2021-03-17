library(shiny)
library(tidyverse)
library(soundgen)
library(shinyWidgets)
library(dipsaus)

diphthongs <- readRDS("price_mouth_raw.rds")
test1 <- NULL

ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Application title
  titlePanel("NZ dipthongs"),
  
  sidebarPanel(
  sliderInput("yob",
              "Year of birth:",
              min = 1851,
              max = 1987,
              value = c(1851, 1987),
              sep = ""),
  
  checkboxGroupInput(
    'sex', 'Gender:', choices = list("Female" = "F", "Male" = "M"), selected = c("F", "M"), inline = TRUE
  ),
  
  uiOutput("speaker"),
  
  checkboxGroupInput(
    'filtered', 'Data:', choices = list("Final data" = "final", "Removed" = "raw"), inline = TRUE, selected = "final"
  ),
  
  fluidRow(
    actionButtonStyled("tracjectories", "Plot", type = "success"),
    actionButton("mouth_sound", " MOUTH", type = "info", icon = icon("volume-up")),
    actionButton("price_sound", " PRICE", type = "info", icon = icon("volume-up"))
  )
  
  ),
  
  mainPanel(
  plotOutput('plot',height = '563px')
  )
  
)

server <- function(input, output) {
  
  data <- reactive({
    
    diphthongs %>%
      select(speaker, sex, yob) %>%
      distinct() %>%
      filter(yob %in% c(min(input$yob):max(input$yob)),
             sex %in% input$sex)
    
  })
  
  data_speaker <- reactive({
    
    diphthongs %>%
      filter(speaker == input$speaker,
             filtered %in% input$filtered)
    
  })
  
  data_speaker_plot <- reactive({
    data1 <- data_speaker() %>%
      pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value") %>%
      mutate(sex = ifelse(sex == "F", "Female", "Male"))
    
    v$plot <- diphthongs %>%
      pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value") %>%
      ggplot(aes(x=measurement_no, y=F_value, colour=F_variable)) +
      geom_line(data = data1, aes(group = paste0(F_variable, id)), alpha=0.1, size = 0.5) +
      geom_smooth(data = data1, method = "gam") +
      scale_color_manual(values = c("red", "blue")) +
      scale_x_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25%", "50%", "75%")) +
      scale_y_continuous(breaks = c(0, 1000, 2000, 3000), limits = c(0, 3680)) +
      xlab("Measurement point\n(% vowel duration)") +
      ylab("Formant value (Hz)") +
      ggtitle(paste0("Speaker: ", data1$speaker, "\nYear of birth: ", data1$yob, "\nGender: ", data1$sex)) +
      facet_grid(~vowel) +
      theme_bw() +
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold"),
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_text(size = 14))
    
    #extract the smooth values from the plot so they can be synthesised
    test1 <<- ggplot_build(v$plot)$data[[2]]
    
  })
  
  output$speaker <- renderUI({
    data1 <- data()
    
    selectInput(
      'speaker', 'Speaker:', choices = c(Choose='', data1$speaker), selectize = TRUE
    )
  })
  
  v <- reactiveValues(plot = NULL)
  
  observeEvent(input$tracjectories, {
    
    data_speaker_plot()

  })
  
  observeEvent(input$mouth_sound, {
    
    # if (is.null(test1)) {
    #   return()
    # }
    
    if (input$speaker == "") {
      return()
    }
    
    plot1 <- data_speaker_plot()
    
    female_male <- data_speaker() %>%
      select(speaker, sex) %>%
      distinct() %>%
      mutate(sex = ifelse(sex == "F", 0.5, -0.5))
    
    mouth_f1_values <- test1 %>%
      filter(PANEL == 1,
             group == 1)
    
    mouth_f2_values <- test1 %>%
      filter(PANEL == 1,
             group == 2)
    
    playme(soundgen(formants = list(mouth_f1_values$y, mouth_f2_values$y), maleFemale = female_male$sex))
    
    
  })
  
  observeEvent(input$price_sound, {
    
    if (input$speaker == "") {
      return()
    }
    
    plot1 <- data_speaker_plot()
    
    female_male <- data_speaker() %>%
      select(speaker, sex) %>%
      distinct() %>%
      mutate(sex = ifelse(sex == "F", 0.5, -0.5))
    
    price_f1_values <- test1 %>%
      filter(PANEL == 2,
             group == 1)
    
    price_f2_values <- test1 %>%
      filter(PANEL == 2,
             group == 2)
    
    playme(soundgen(formants = list(price_f1_values$y, price_f2_values$y), maleFemale = female_male$sex))
    
    
  })
  
  output$plot <- renderPlot({
    if (is.null(v$plot)) return()
    v$plot
  })

}

shinyApp(ui, server)