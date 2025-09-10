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
library(jsonlite, include.only = "fromJSON")
library(htmltools)

## Load the data
load("datactc.RData")

webcode = fromJSON("webcode.json")

####  BEGINING OF THE SHINY 

ui <- dashboardPage(
  
  
        skin = "green",  #color themes
      
        # A header
        dashboardHeader(title = "StatCTC"),
  
          # A sidebar
         dashboardSidebar(
    
                     sidebarMenu(
                                      menuItem("About StatCTC", tabName = "ctclogo", icon = icon("chart-line")), # 4 th tab
                                      #menuItem("À propos", tabName = "widgets", icon = icon("file-lines")), # first  tab
                                      menuItem("La CTC", tabName = "dashboard", icon = icon("chart-line")), # 2nd tab
                                      menuItem("Les adhésions", tabName = "test_FILTRE", icon = icon("chart-line")), # 3rd tab
                                      menuItem("Les sections", tabName = "dashboardsection", icon = icon("chart-line")) # 4 th tab
                                      
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
                                                box(plotOutput("plot2"),width = 4, status = "primary") ,
                
                                                #box3
                                                box(plotOutput("plot3"),width = 4, status = "primary") ,
                                                
                                                #box5
                                                box(plotOutput("plot5"),width = 4, status = "primary") ,
                                                
                                                #box6
                                                box(plotOutput("plot6"),width = 4, status = "info") ,
                                                
                                                #box7
                                                box(plotOutput("plot7"),width = 4, status = "info") ,
                                                
                                                #box8
                                                box(plotOutput("plot8"),width = 4, status = "info") ,
                                                
                                                #box9
                                                box(plotOutput("plot9"),width = 4, status = "warning") ,
                                                
                                                #box1
                                                box(plotOutput("plot1"),width = 4, status = "warning"),
                                                
                
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
        
                                         #h1("Communauté Togolaise Au Canada (CTC)"),
        
        
                                         # h2("La CTC est un organisme à  but non lucratif qui représente les Togolais vivant au Canada. Depuis 32 ans, 
                                         #    notre mission est de favoriser la solidarité entre la diaspora togolaise, de promouvoir la culture togolaise 
                                         #   et d’intervenir sur les questions cruciales concernant notre pays d’origine, le Togo."),
                  
                                         h2("Ce tableau de bord présente des statistiques sur les membres de la Communauté Togolaise au Canada (CTC). Il permet de visualiser l'évolution des adhésions dans le temps et de mieux comprendre la composition de notre communauté, afin de mieux répondre à ses besoins."),
                                         h1(""),
                                         h1(""),
        
                                         p(tags$ul(  tags$li(tags$b(t("NOTRE MISSION:")),t("Servir la communauté togolaise au Canada tout en promouvant l’intégration et l’échange de richesses culturelles.")),
                                                     tags$li(tags$b(t("NOTRE VISION:")),t("Contribuer au bien-être des personnes originaires du Togo au Canada et à l’essor socio-économique du Canada.")),
                                                     tags$li(tags$b(t("NOS VALEURS:")),t("Service, Respect et intégrité, Excellence, Équité")),
                                                     style = "font-size:18px;"
                                                    )
                                            ),
       
                                         h1(""),
                                         h1(""),
                                         h1(""),
                                         h1(""),
        
        
                                         #This is where The statistics are derived for About PAGE.
        
                                         fluidRow( # Dynamic valueBoxes
                                                       valueBoxOutput("progressBox", width = 4),
                                                       valueBoxOutput("approvalBox", width = 4),
                                                       valueBox(4885, a(href = "https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/dv-vd/imm/index-fr.cfm", "Statistique Canada, Recensement de la population, 2021."), width = 4,color = "teal",icon = icon("fa-regular fa-user"))
                                                   ),
                                         p("Le tableau de bord a été mis à jour pour la dernière fois le 01-05-2025 "),
                                         p(""),
                                         #a(href = "https://github.com/Bahamyirou/ctc", "Voulez-vous contribuer à StatCTC, Merci de visiter la page GitHub de la CTC ou de contacter communications@ctcanada.org."),
                                         p(""),
                                         #a(href = "https://ctcanada.org/", "Pour plus d'informations sur la CTC"),
                                         p(""),
                                         #a(href = "https://hctogocanada.org/", "Pour plus d'informations sur le Haut Commissariat du Togo au Canada (HCTC)."),
                                         
                                         
                                         p(t("Vous voulez contribuer à StatCTC ? merci de visiter la page"),
                                           a(href = "https://github.com/Bahamyirou/ctc", "GitHub"),
                                           t(" de la CTC ou contactez nous par courriel à \"communications@ctcanada.org\"."),
                                           t(" Reférez-vous à ce"),
                                           a(href = "https://ctcanada.org/", "lien"),
                                            t("pour plus d'informations sur la CTC."),
                                           t(" Reférez-vous à ce"),
                                           a(href = "https://hctogocanada.org/", "lien"),
                                           t("pour plus d'informations sur le Haut Commissariat du Togo au Canada (HCTC).")
                                           )

        
        
                                     ),
      
      
                # third tab content
                        tabItem(
        
                                      tabName = "dashboardsection",
                                      h2("Développement À venir ...")
        
                               ),
      
               # third tab content
                       tabItem(
                                   tabName = "test_FILTRE",
                                   fluidRow(
                                     
                                              box( 
                                                      title = t("Filtre"),
                                                      status = "success",
                                                      solidHeader = TRUE,
                                                      width = 3, collapsible=TRUE,collapsed=TRUE,
                     
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
        
        
                             ), # end of tabItem
           
          tabItem(
            
            
                   tabName = "ctclogo",
                   h2("Ce tableau de bord présente des statistiques sur les membres de la Communauté Togolaise au Canada (CTC) et éclaire sur la composition de notre communauté pour mieux répondre à ses besoins."),
            
                   fluidRow(
                                  column(width = 10, offset = 1, htmlOutput(outputId = "logoCTC"))
                           ),
                   #This is where The statistics are derived for About PAGE.
                   
                   fluidRow( # Dynamic valueBoxes
                     valueBoxOutput("progressBox", width = 4),
                     valueBoxOutput("approvalBox", width = 4),
                     valueBox(4885, a(href = "https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/dv-vd/imm/index-fr.cfm", "Statistique Canada, Recensement de la population, 2021."), width = 4,color = "teal",icon = icon("fa-regular fa-user"))
                   ),
                   p("Le tableau de bord a été mis à jour pour la dernière fois le 01-05-2025 "),
                   p(""),
                   #a(href = "https://github.com/Bahamyirou/ctc", "Voulez-vous contribuer à StatCTC, Merci de visiter la page GitHub de la CTC ou de contacter communications@ctcanada.org."),
                   p(""),
                   #a(href = "https://ctcanada.org/", "Pour plus d'informations sur la CTC"),
                   p(""),
                   #a(href = "https://hctogocanada.org/", "Pour plus d'informations sur le Haut Commissariat du Togo au Canada (HCTC)."),
                   
                   
                   p(t("Vous voulez contribuer à StatCTC ? merci de visiter la page"),
                     a(href = "https://github.com/Bahamyirou/ctc", "GitHub"),
                     t(" de la CTC ou contactez nous par courriel à \"communications@ctcanada.org\"."),
                     t(" Reférez-vous à ce"),
                     a(href = "https://ctcanada.org/", "lien"),
                     t("pour plus d'informations sur la CTC."),
                     t(" Reférez-vous à ce"),
                     a(href = "https://hctogocanada.org/", "lien"),
                     t("pour plus d'informations sur le Haut Commissariat du Togo au Canada (HCTC).")
                   )
                  )
          
      
      
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
                                                       geom_area(fill = rgb(0, 0.5, 1, alpha = 0.5))+ 
                                                       ggtitle("Nombre des adhésions en fonction du temps")+ 
                                                       theme(plot.title = element_text(size = 30)) 
          
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
                       
                       # TODO: convert to plotly figure
                       #output$timeline <- renderUI({
                       #  tags$img(src = "timeline-jobs.png",
                       #           style = getWebCode("JPGStyle"))
                       #})
                      
  
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
  
  #################################################
  ## TAB 9 - About StatCTC
  #################################################

  # TODO: convert to plotly figure
  output$logoCTC <- renderUI({
    tags$img(src = "LOGO.jpg",
             style = "max-width: 100%; height: auto; display: block; margin: 0 auto;")
  })
  
  
}

shinyApp(ui, server)