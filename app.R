##############################################################################################
##############################################################################################
######################## April 2025 - Asma BAHAMYIROU, PHD ##################################
##############################################################################################

## app.R ##
library(shinydashboard)
library(ggplot2)
library(readxl)
library(tidyverse)
library(shinyWidgets)

## Load the data
load("datactc.RData")

####  BEGINING OF THE SHINY 

ui <- dashboardPage(
  
  
        skin = "green",  #color themes
      
        # A header
        dashboardHeader(title = "StatCTC"),
  
          # A sidebar
         dashboardSidebar(
    
                     sidebarMenu(
                                      menuItem("À propos", tabName = "widgets", icon = icon("file-lines")), # 2nd tab
                                      menuItem("La CTC", tabName = "dashboard", icon = icon("chart-line")), # first tab
                                      menuItem("Les sections", tabName = "dashboardsection", icon = icon("chart-line")), # first tab
                                      menuItem("Les sections 2", tabName = "test_FILTRE", icon = icon("chart-line")) # first tab
                                 )
    
                          ),
  
       ### A body 
        dashboardBody(
    
                       #This Items help to create different table or page inside the applcation
                        tabItems(
      
      
                          # First tab or page content
                              tabItem(tabName = "dashboard",
              
                                         fluidRow(
                
                                                #box2
                                                box(plotOutput("plot2"),width = 4) ,
                
                                                #box3
                                                box(plotOutput("plot3"),width = 4) ,
                                                
                                                #box5
                                                box(plotOutput("plot5"),width = 4) ,
                                                
                                                #box6
                                                box(plotOutput("plot6"),width = 4) ,
                                                
                                                #box7
                                                box(plotOutput("plot7"),width = 4) ,
                                                
                                                #box8
                                                box(plotOutput("plot8"),width = 4) ,
                                                
                                                #box9
                                                box(plotOutput("plot9"),width = 4) ,
                                                
                                                
                                                
                                                #box1
                                                box(plotOutput("plot1"),width = 4),
                                                
                
                                                #box4
                                                box(
                                                     title = "Controls",
                                                     sliderInput("slider", "Number of observations:", 1, 100, 50)
                                                    ),
                
                                                #box5
                                                box(
                                                     title = "xxx",
                                                     sliderInput("slider", "Number of observations:", 1, 100, 50)
                                                   )
                
                
                                             ),
              
                                        # This section display the 3 statistics in About page
                                        fluidRow(
                                             infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                                             infoBoxOutput("progressBox"),
                                             infoBoxOutput("approvalBox2")
                                            )
                                    ),
      
      
                       # Second tab or page content
      
                             tabItem(
        
                                         tabName = "widgets",
        
                                         h1("Communauté Togolaise Au Canada (CTC)"),
        
        
                                         h2("La CTC est un organisme à  but non lucratif qui représente les Togolais vivant au Canada. Depuis 32 ans, 
                                             notre mission est de favoriser la solidarité entre la diaspora togolaise, de promouvoir la culture togolaise 
                                             et d’intervenir sur les questions cruciales concernant notre pays d’origine, le Togo."),
                  
                                         h2("Ce tableau de bord fournit des statistiques sur les membres de la CTC, permet de visualiser la tendance 
                                            des adhésions dans le temps et comprendre la composition de notre communauté afin de mieux la servir."),
        
      
        
                                         p(tags$ul(  tags$li(tags$b(t("NOTRE MISSION:")),t("Servir la communauté togolaise au Canada tout en promouvant l’intégration et l’échange de richesses culturelles.")),
                                                     tags$li(tags$b(t("NOTRE VISION:")),t("Contribuer au bien-être des personnes originaires du Togo au Canada et à l’essor socio-économique du Canada.")),
                                                     tags$li(tags$b(t("NOS VALEURS:")),t("Service, Respect et intégrité, Excellence, Équité")),
                                                     style = "font-size:18px;"
                                                    )
                                            ),
       
        
      
        
        
                                         #This is where The statistics are derived for About PAGE
        
                                         fluidRow(
                                                       #infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                                                       #infoBoxOutput("progressBox"),
                                                       #infoBoxOutput("approvalBox2")
                                                       # valueBox(10 * 2, "Membres Actifs", width = 3,color = "green",icon = icon("fa-solid fa-signal-bars")),
                          
                                                       # Dynamic valueBoxes
                                                       valueBoxOutput("progressBox", width = 4),
                                                       valueBoxOutput("approvalBox", width = 4),
                                                       valueBox(4885, a(href = "https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/dv-vd/imm/index-fr.cfm", "Statistique Canada, Recensement de la population, 2021."), width = 4,color = "red",,icon = icon("glist-alt"))
                                                   ),
        
                                         p("Le tableau de bord a été mis à jour pour la dernière fois le 24-04-2025 "),
                                         a(href = "https://ctcanada.org/", "Pour plus d'informations sur la CTC")
        
        
                                     ),
      
      
                # third tab content
                        tabItem(
        
                                      tabName = "dashboardsection",
                                      h2("À venir ...")
        
                               ),
      
               # third tab content
                       tabItem(
                                   tabName = "test_FILTRE",
                                   fluidRow(
                                     
                                              box( 
                                                      title = t("Filtre"),
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                       width = 3,
            
                                                       # Filter by Section
                                                      pickerInput(   inputId = "AllSection",
                                                                     label = t("Selectionner une section"),
                                                                      choices =  sort(unique(TrendAdhesion$section)),
                                                                      selected = first(sort(TrendAdhesion$section)),
                                                                      multiple = FALSE
                                                                  ),
            
                                                        # Date range slider (THE OUTPUT WILL be call dat - this will contain each min/max that user will chose and use below in servser side for filtration)
                                                       dateRangeInput(   inputId = "DateRangeSelected",
                                                                         label = t("Selectionner un intervalle de temps"),
                                                                          start = min(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
                                                                          end =   max(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
                                                                           min =   min(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
                                                                           max =   max(TrendAdhesion$AdhésionDébut, na.rm = TRUE),
                                                                           separator = t("à")
                                                                      )
            
                                                )# column
          ,
                                           box(
                                                  width = 10,
                                                  fluidRow(
                                                             column(
                                                                      width = 12, 
                                                                      plotOutput(outputId = "plot4",  height = "500px")
                                                                )#End of column
                                                        )  #end of the fluidRow
                                              ) #end of the box ,
          
                                       )#end of fluidRow
        
        
                             ) # end of tabItem
      
      
          ) # end of tabItems 
    ) # end of dashboardBody
  
  
  
  
  # END OF TAB ITEMS
)




server <- function(input, output) {
  

  
                          # first data input
                           set.seed(122)
                           histdata <- rnorm(500)
  
                           output$plot1 <- renderPlot({
    
                                                          data <- histdata[seq_len(input$slider)]
                                                          hist(data)
                                                      })

  
                            output$plot2 <- renderPlot({
                           
                                                       ggplot(data = sub, aes(x = MembreProvince, fill = MembreStatut)) +
                                                        geom_bar(position = "dodge")
                                                    })
  
  
              # ctc input (df contains the frequency of observations based on section)
  
      
  
                        output$plot3 <- renderPlot({
                                          
                                                      sub %>% ggplot(aes(x = section)) +
                                                      geom_bar(position = "dodge")
                                                   })
  
                # section qui tient compte du Nb membres actifs
             
  
            
                       output$plot4 <- renderPlot({
    
                                                      #Create the data (filter data based on the select date range that user will chose)
                                                      datAdhesion_subset <- TrendAdhesion %>% dplyr::filter(section %in% input$AllSection,
                                                                                                                        between(AdhésionDébut, 
                                                                                                                                 input$DateRangeSelected[1], 
                                                                                                                                 input$DateRangeSelected[2]
                                                                                                                                ) 
                                                                                                            )
    
                                                     histo <- datAdhesion_subset %>% ggplot( aes(x = AdhésionDébut, y = cum)) +
                                                                                              ylab("Nombre de membres actifs") +
                                                                                              xlab("Date de l'adhésion") +
                                                                                               geom_line()
          
                                                     histo
  
                                                  })#END NB actifs
                       
                       ### Distribution des types de membres
                       output$plot5 <- renderPlot({
                         
                                                      ggplot(data = subNew, aes(x = Type, y = Proportion)) + geom_bar(stat = "identity")
                                                  })
                       
                       
                       ### Statut des membres selon le type
                       output$plot6 <- renderPlot({
                         
                                                      ggplot(data = sub, aes(x = Type, fill = MembreStatut)) + geom_bar(position = "dodge")
                                                 })
                       
                       
                       ### Type des membres selon la section
                       output$plot7 <- renderPlot({
                         
                                                      ggplot(data = sub, aes(x = section, fill = Type)) + geom_bar(position = "dodge")
                         
                                                  })
                        
                       ### Section selon la type de membre
                       output$plot8 <- renderPlot({
                         
                                                   ggplot(data = sub, aes(x = Type, fill = section)) + geom_bar(position = "dodge")
                         
                                                  })
                       
                       ### Type selon la province
                       output$plot9 <- renderPlot({
                         
                                                     ggplot(data = sub, aes(x = MembreProvince, fill = Type)) + geom_bar(position = "dodge")
                         
                                                  })
              
  
  #progressBox
  output$progressBox <- renderValueBox({
    valueBox(
      as.numeric(table(sub$MembreStatut)[1]),  "Membres Actifs", icon = icon("list"),
      color = "purple"
    )
  })
  
  

  output$approvalBox <- renderInfoBox({
    valueBox(
      "Actifs",paste0(round(as.numeric(table(sub$MembreStatut)[1])*100/4885, digits = 2), "%"), icon = icon("list"),
      color = "yellow"
    )
  })
}

shinyApp(ui, server)