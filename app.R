
##############################################################################################
##############################################################################################
######################## April 2025 - Asma BAHAMYIROU, PHD ##################################
########################   Updated by Gemini Pro - May 2024    ###############################
##############################################################################################

## app.R ##
library(shinydashboard)
library(ggplot2)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(jsonlite, include.only = "fromJSON")
library(htmltools)
library(plotly) # Added for interactive plots

## Load the data
load("datactc.RData")

webcode = fromJSON("webcode.json")

####  BEGINING OF THE SHINY 

ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(title = "StatCTC"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("À propos de StatCTC", tabName = "ctclogo", icon = icon("info-circle")),
      menuItem("La CTC en chiffres", tabName = "dashboard", icon = icon("chart-pie")),
      menuItem("Analyse des adhésions", tabName = "adhesions_tab", icon = icon("chart-line")),
      menuItem("Analyse des sections", tabName = "dashboardsection", icon = icon("sitemap"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # "About" Tab
      tabItem(
        tabName = "ctclogo",
        fluidRow(
          column(width = 10, offset = 1, htmlOutput(outputId = "logoCTC"))
        ),
        h2("Ce tableau de bord présente des statistiques sur les membres de la Communauté Togolaise au Canada (CTC) et éclaire sur la composition de notre communauté pour mieux répondre à ses besoins."),
        
        fluidRow(
          valueBoxOutput("membersActiveBox", width = 4),
          valueBoxOutput("percentageActiveBox", width = 4),
          valueBox(
            "4,885", 
            "Togolais au Canada", 
            icon = icon("users"),
            color = "teal",
            href = "https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/dv-vd/imm/index-fr.cfm"
          )
        ),
        p("Le tableau de bord a été mis à jour pour la dernière fois le 01-05-2025."),
        hr(),
        p(
          "Vous voulez contribuer à StatCTC ? Merci de visiter la page ",
          a(href = "https://github.com/Bahamyirou/ctc", "GitHub de la CTC"),
          " ou contactez nous par courriel à \"communications@ctcanada.org\"."
        ),
        p(
          "Pour plus d'informations sur la CTC, visitez ", a(href = "https://ctcanada.org/", "notre site web"), ".",
          " Pour le Haut Commissariat du Togo au Canada (HCTC), visitez ce ", a(href = "https://hctogocanada.org/", "lien"), "."
        )
      ),
      
      # "La CTC en chiffres" Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(plotOutput("plot2"), width = 4, status = "primary", title = "Membres par Province"),
          box(plotOutput("plot3"), width = 4, status = "primary", title = "Membres par Section"),
          box(plotOutput("plot5"), width = 4, status = "primary", title = "Proportion par Type de Membre"),
          box(plotOutput("plot6"), width = 4, status = "info", title = "Statut par Type de Membre"),
          box(plotOutput("plot7"), width = 4, status = "info", title = "Type de Membre par Section"),
          box(plotOutput("plot8"), width = 4, status = "info", title = "Section par Type de Membre"),
          box(plotOutput("plot9"), width = 4, status = "warning", title = "Type de Membre par Province"),
          box(plotOutput("plot1"), width = 4, status = "warning", title = "Histogramme (Exemple)"),
          box(
            title = "Contrôles (Exemple)",
            sliderInput("slider", "Nombre d'observations:", 1, 500, 50)
          )
        )
      ),
      
      # "Analyse des adhésions" Tab
      tabItem(
        tabName = "adhesions_tab",
        fluidRow(
          box(
            title = "Filtres",
            status = "success",
            solidHeader = TRUE,
            width = 3, collapsible = TRUE,
            
            pickerInput(
              inputId = "AllSection",
              label = "Sélectionner une ou plusieurs sections",
              choices = sort(unique(TrendAdhesion$section)),
              selected = first(sort(TrendAdhesion$section)),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            dateRangeInput(
              inputId = "DateRangeSelected",
              label = "Sélectionner un intervalle de temps",
              start = min(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
              end   = max(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
              min   = min(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
              max   = max(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
              separator = "à"
            )
          ),
          box(
            width = 9,
            status = "primary",
            solidHeader=TRUE,
            title = "Évolution temporelle des adhésions",
            plotlyOutput(outputId = "adhesionsPlot", height = "500px") # Using plotlyOutput
          )
        )
      ),
      
      
      # "Analyse des sections" Tab
      tabItem(
        tabName = "dashboardsection",
        h2("Développement à venir ...")
      )
    )
  )
)

server <- function(input, output) {
  
  #----------------------------------------------------
  #-- TAB 1: À PROPOS
  #----------------------------------------------------
  output$logoCTC <- renderUI({
    tags$img(src = "LOGO.jpg", style = "max-width: 100%; height: auto; display: block; margin: 0 auto;")
  })
  
  nb_actifs <- as.numeric(table(sub$MembreStatut)[1])
  
  output$membersActiveBox <- renderValueBox({
    valueBox(
      nb_actifs, "Membres Actifs", icon = icon("user-check"),
      color = "purple"
    )
  })
  
  output$percentageActiveBox <- renderValueBox({
    valueBox(
      paste0(round(nb_actifs * 100 / 4885, digits = 1), "%"), "des Togolais recensés", icon = icon("percentage"),
      color = "yellow"
    )
  })
  
  #----------------------------------------------------
  #-- TAB 2: LA CTC EN CHIFFRES (STATIC PLOTS)
  #----------------------------------------------------
  
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    hist(histdata[seq_len(input$slider)], main = "Données Aléatoires")
  })
  
  output$plot2 <- renderPlot({
    ggplot(data = sub, aes(x = MembreProvince, fill = MembreStatut)) +
      geom_bar(position = "dodge") + labs(fill="Statut") + theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    sub %>% ggplot(aes(x = fct_rev(fct_infreq(section)))) +
      geom_bar() + coord_flip() + labs(x="Section") + theme_minimal()
  })
  
  output$plot5 <- renderPlot({
    ggplot(data = subNew, aes(x = Type, y = Proportion)) + 
      geom_bar(stat = "identity", fill="skyblue") + theme_minimal()
  })
  
  output$plot6 <- renderPlot({
    ggplot(data = sub, aes(x = Type, fill = MembreStatut)) + 
      geom_bar(position = "dodge") + labs(fill="Statut") + theme_minimal()
  })
  
  output$plot7 <- renderPlot({
    ggplot(data = sub, aes(x = section, fill = Type)) + 
      geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(fill="Type") + theme_minimal()
  })
  
  output$plot8 <- renderPlot({
    ggplot(data = sub, aes(x = Type, fill = section)) + 
      geom_bar(position = "dodge") + labs(fill="Section") + theme_minimal()
  })
  
  output$plot9 <- renderPlot({
    ggplot(data = sub, aes(x = MembreProvince, fill = Type)) + 
      geom_bar(position = "dodge") + labs(fill="Type") + theme_minimal()
  })
  
  #----------------------------------------------------
  #-- TAB 3: ANALYSE DES ADHÉSIONS (INTERACTIVE PLOT)
  #----------------------------------------------------
  
  # Reactive expression for filtered data (This part is correct and unchanged)
  filtered_adhesion_data <- reactive({
    req(input$AllSection)
    
    TrendAdhesion %>%
      dplyr::filter(
        section %in% input$AllSection,
        between(AdhésionDébut, input$DateRangeSelected[1], input$DateRangeSelected[2])
      ) %>%
      group_by(section) %>%
      arrange(AdhésionDébut) %>%
      mutate(
        new_members = cum - lag(cum, default = first(cum)),
        tooltip_text = paste0(
          "<b>Section: </b>", section, "\n",
          "<b>Date: </b>", format(AdhésionDébut, "%d %b %Y"), "\n",
          "<b>Membres (cumulé): </b>", cum, "\n",
          "<b>Nouveaux membres (ce jour): </b>", new_members
        )
      ) %>%
      ungroup()
  })
  
  # Render the interactive plot using the reactive data
  output$adhesionsPlot <- renderPlotly({
    
    p <- ggplot(filtered_adhesion_data(), aes(x = AdhésionDébut, y = cum, group = section)) +
      
      # The visual area layer (NO tooltip information here)
      geom_area(aes(fill = section), alpha = 0.5, position = 'identity') +
      
      # <-- NEW: Add an *invisible* point layer just for the hover tooltips
      geom_point(aes(text = tooltip_text), color = "transparent") +
      
      # Labels and theme
      ylab("Nombre cumulé de membres actifs") +
      xlab("Date de l'adhésion") +
      theme_minimal() +
      labs(fill = "Section")
    
    # Tell ggplotly to build its tooltip from the 'text' aesthetic we provided to geom_point
    ggplotly(p, tooltip = "text")
  })
  
}


shinyApp(ui, server)