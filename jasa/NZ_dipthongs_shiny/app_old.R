#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(soundgen)

############
# Data
############

# price_filtered1 <- readRDS("price_full_jasa.rds")
# 
# price_filtered1 <- price_filtered1 %>%
#   ungroup() %>%
#   mutate(Vowel = "PRICE")
# 
# mouth_filtered1 <- readRDS("mouth_full_jasa.rds")
# 
# mouth_filtered1 <- mouth_filtered1 %>%
#   ungroup() %>%
#   mutate(Vowel = "MOUTH")
# 
# diphthongs <- price_filtered1 %>%
#   rbind(mouth_filtered1) %>%
#   group_by(id) %>%
#   mutate(filtered = sample(c("final", "raw"), 1)) %>%
#   ungroup()

diphthongs <- readRDS("price_mouth_raw.rds")

# setdiff(names(diphthongs1), names(diphthongs))

##########
# User interface
##########

ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Application title
  titlePanel("NZ dipthongs"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("yob",
                  "Year of birth:",
                  min = 1851,
                  max = 1987,
                  value = c(1851, 1987),
                  # min = 1890,
                  # max = 1990,
                  # value = 1900,
                  sep = ""),
      
      uiOutput("gender"),
      uiOutput("speaker"),
      checkboxGroupInput(
        'filtered', 'Data:', choices = list("Final data" = "final", "Removed" = "raw"), inline = TRUE, selected = "final"
      ),
      tags$head(tags$script(src = "message-handler.js")),
      fluidRow(
        actionButton("mouth_sound", "Play MOUTH"),
        actionButton("price_sound", "Play PRICE"))
      
    ),
    
    # plot
    mainPanel(
      plotOutput("distPlot")
      
    )
  )
)

##########
# server
##########

server <- function(input, output) {

  # gender selection
  output$gender <- renderUI({
    df <- diphthongs %>% filter(yob %in% c(min(input$yob):max(input$yob)))
    checkboxGroupInput(
      'gender', 'Gender:', choices = list("Female" = "F", "Male" = "M"), selected = c("F", "M"), inline = TRUE
    )
  })
  
  #speaker selection
  output$speaker <- renderUI({
    df <- diphthongs %>% filter(yob %in% c(min(input$yob):max(input$yob)),
                                     sex %in% input$gender)
    
    selectInput(
      'speaker', 'Speaker:', choices = c(Choose='', unique(df$speaker)), selectize = TRUE
    )
  })
  
  #main plot
  output$distPlot <-
    
    renderPlot({
      
      information <- diphthongs %>%
        filter(speaker %in% input$speaker) %>%
        select(speaker, yob, sex) %>%
        mutate(sex = ifelse(sex == "F", "Female", "Male")) %>%
        distinct()
      
      speaker_plot_data <- diphthongs %>%
        filter(speaker %in% input$speaker,
               filtered %in% input$filtered) %>%
        pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value")
      
      speaker_plot1 <-  diphthongs %>%
        # mutate(f1 = f1,
        #        f2 = f2) %>%
        pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value") %>%
        ggplot(aes(x=measurement_no, y=F_value, colour=F_variable)) +
        geom_line(data = speaker_plot_data, aes(group = paste0(F_variable, id)), alpha=0.1, size = 0.5) +
        # geom_line(aes(y=f2), col="blue", alpha=0.2) +
        geom_smooth(data = speaker_plot_data, method = "gam") +
        scale_color_manual(values = c("red", "blue")) +
        scale_x_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25%", "50%", "75%")) +
        scale_y_continuous(breaks = c(0, 1000, 2000, 3000), limits = c(0, 3680)) +
        xlab("Measurement point\n(% vowel duration)") +
        ylab("Formant value (Hz)") +
        ggtitle(paste0("Speaker: ", input$speaker, "\nYear of birth: ", information$yob, "\nGender: ", information$sex)) +
        facet_grid(~vowel) +
        theme_bw() +
        theme(legend.position = "none",
              strip.text = element_text(size = 18, face = "bold"),
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 14))
      
      #extract the smooth values from the plot so they can be synthesised
      # test1 <<- ggplot_build(speaker_plot1)$data[[2]]
      
      speaker_plot1
      
    })
  
  #MOUTH sound
  observeEvent(input$mouth_sound, {
    
    if (input$speaker != '') {
      
      speaker_plot1 <-  diphthongs %>%
        # mutate(f1 = f1,
        #        f2 = f2) %>%
        pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value") %>%
        ggplot(aes(x=measurement_no, y=F_value, colour=F_variable)) +
        geom_line(data = speaker_plot_data, aes(group = paste0(F_variable, id)), alpha=0.1, size = 0.5) +
        # geom_line(aes(y=f2), col="blue", alpha=0.2) +
        geom_smooth(data = speaker_plot_data, method = "gam") +
        scale_color_manual(values = c("red", "blue")) +
        scale_x_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25%", "50%", "75%")) +
        scale_y_continuous(breaks = c(0, 1000, 2000, 3000), limits = c(0, 3680)) +
        xlab("Measurement point\n(% vowel duration)") +
        ylab("Formant value (Hz)") +
        ggtitle(paste0("Speaker: ", input$speaker, "\nYear of birth: ", information$yob, "\nGender: ", information$sex)) +
        facet_grid(~vowel) +
        theme_bw() +
        theme(legend.position = "none",
              strip.text = element_text(size = 18, face = "bold"),
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 14))
      
      #extract the smooth values from the plot so they can be synthesised
      test1 <- ggplot_build(speaker_plot1)$data[[2]]
      
    diphthongs_speakers <- diphthongs %>%
      select(speaker, sex) %>%
      distinct() %>%
      mutate(sex = ifelse(sex == "F", 0.5, -0.5))
    
    female_male <- diphthongs_speakers[diphthongs_speakers$speaker %in% input$speaker, ]
    
    mouth_f1_values <- test1 %>%
      filter(PANEL == 1,
             group == 1)
    
    mouth_f2_values <- test1 %>%
      filter(PANEL == 1,
             group == 2)
    
    playme(soundgen(formants = list(mouth_f1_values$y, mouth_f2_values$y), maleFemale = female_male$sex))
    
    insertUI(selector = "#mouth_sound",
             where = "afterEnd",
             ui = tags$audio(autoplay = TRUE, controls = NA, style="display:none;")
    )
    }
  })
  
  #PRICE sound
  observeEvent(input$price_sound, {
    
    if (input$speaker != '') {
    
      speaker_plot1 <-  diphthongs %>%
        # mutate(f1 = f1,
        #        f2 = f2) %>%
        pivot_longer(f1:f2, names_to = "F_variable", values_to = "F_value") %>%
        ggplot(aes(x=measurement_no, y=F_value, colour=F_variable)) +
        geom_line(data = speaker_plot_data, aes(group = paste0(F_variable, id)), alpha=0.1, size = 0.5) +
        # geom_line(aes(y=f2), col="blue", alpha=0.2) +
        geom_smooth(data = speaker_plot_data, method = "gam") +
        scale_color_manual(values = c("red", "blue")) +
        scale_x_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25%", "50%", "75%")) +
        scale_y_continuous(breaks = c(0, 1000, 2000, 3000), limits = c(0, 3680)) +
        xlab("Measurement point\n(% vowel duration)") +
        ylab("Formant value (Hz)") +
        ggtitle(paste0("Speaker: ", input$speaker, "\nYear of birth: ", information$yob, "\nGender: ", information$sex)) +
        facet_grid(~vowel) +
        theme_bw() +
        theme(legend.position = "none",
              strip.text = element_text(size = 18, face = "bold"),
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 14))
      
      #extract the smooth values from the plot so they can be synthesised
      test1 <- ggplot_build(speaker_plot1)$data[[2]]
      
      
    diphthongs_speakers <- diphthongs %>%
      select(speaker, sex) %>%
      distinct() %>%
      mutate(sex = ifelse(sex == "F", 0.5, -0.5))
    
    female_male <- diphthongs_speakers[diphthongs_speakers$speaker %in% input$speaker, ]
    
    price_f1_values <- test1 %>%
      filter(PANEL == 2,
             group == 1)
    
    price_f2_values <- test1 %>%
      filter(PANEL == 2,
             group == 2)
    
    playme(soundgen(formants = list(price_f1_values$y, price_f2_values$y), maleFemale = female_male$sex))
    
    insertUI(selector = "#price_sound",
             where = "afterEnd",
             ui = tags$audio(autoplay = TRUE, controls = NA, style="display:none;")
    )
    
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

