## app.R ##
library(shinydashboard)
library(ggplot2)
library(readxl)
library(tidyverse)

load("datactc.RData")

ui <- dashboardPage(
 
  # A header
  dashboardHeader(title = "My Dashboard"),
  
  # A sidebar
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("À propos", tabName = "widgets", icon = icon("file-lines")), # 2nd tab
      menuItem("La CTC", tabName = "dashboard", icon = icon("chart-line")), # first tab
      menuItem("Les sections", tabName = "dashboardsection", icon = icon("chart-line")), # first tab
      menuItem("Les sections 2", tabName = "test_FILTRE", icon = icon("chart-line")) # first tab
    )
    
  ),
  
  ##  # A body 
  dashboardBody(
    
    # This Items help to create different table or page inside the applcation
    tabItems(
      
      
      # First tab or page content
      tabItem(tabName = "dashboard",
              fluidRow(
                
                #box1
                box(plotOutput("plot1", height = 250)),
                
                #box4
                box(plotOutput("plot2", height = 250)) ,
                
                #box4
                box(plotOutput("plot3", height = 250)) ,
                
                #box2
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                ),
                
                #box3
                box(
                  title = "xxx",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
                
                
              ),
              
              # infoBoxes with fill=TRUE
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
        
        
        h2("La CTC est un organisme à
                  but non lucratif qui représente les Togolais vivant au Canada. Depuis 32 ans, notre mission est de favoriser la solidarité entre la diaspora togolaise, de promouvoir la culture togolaise et d’intervenir sur les questions cruciales concernant notre pays d’origine, le Togo."),
        
        
        
        fluidRow(
          box(
            title = "NOTRE MISSION", width = 4, background = "yellow",
            "Servir la communauté togolaise au Canada tout en promouvant l’intégration et l’échange de richesses culturelles."
          ),
          box(
            title = "NOTRE VISION", width = 4, background = "red",
            "Contribuer au bien-être des personnes originaires du Togo au Canada et à l’essor socio-économique du Canada."
          ),
          box(
            title = "NOS VALEURS",width = 4, background = "green",
            "Service, Respect et intégrité, Excellence, Équité"
          )
        ),
        
        
        # infoBoxes with fill=TRUE
        fluidRow(
          #infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          #infoBoxOutput("progressBox"),
          #infoBoxOutput("approvalBox2")
          valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
          # Dynamic valueBoxes
          valueBoxOutput("progressBox"),
          valueBoxOutput("approvalBox")
        ),
        p("Le tableau de bord a été mis à jour pour la dernière fois le xxx")
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
            
            # Date range slider (THE OUTPUT WILL be call dat - this will contain each min/max that user will chose and use below in servser side for filtration)
            dateRangeInput(inputId = "DateRangeSelected",
                           label = t("Select date range"),
                           start = min(datAdhesion$AdhésionDébut, na.rm = TRUE),
                           end =   max(datAdhesion$AdhésionDébut, na.rm = TRUE),
                           min =   min(datAdhesion$AdhésionDébut, na.rm = TRUE),
                           max =   max(datAdhesion$AdhésionDébut, na.rm = TRUE),
                           separator = t("to"))
            
          )# column
          ,
          box(
            width = 12,
            fluidRow(
              column(
                width = 12, 
                plotOutput(outputId = "plot4", 
                           height = "600px"))
            )
          ) # column  box(plotOutput("plot3", height = 150)) ,
        )
        
        
      ) # fluidRow
      
      
    )  
  )
  
  
  
  
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
  
  # ctc input (All data)
  
  #data <- read_excel('Membres.xlsx')
  #sub <- read.csv('sub.csv')
  #Load the data files
 
  
  
           output$plot2 <- renderPlot({
                                            ggplot(data = sub, aes(x = MembreProvince, fill = MembreStatut)) +
                                             geom_bar(position = "dodge")
                                     })
  
  
  # ctc input (df contains the frequency of observations based on section)
  
           #df <- read.csv('df.csv')
  
             output$plot3 <- renderPlot({
                                          sub %>% ggplot(aes(x = section)) +
                                           geom_bar(position = "dodge")
                                        })
  
  # section qui tient compte du Nb membres actifs
             
             #datAdhesion <- read.csv('datAdhesion.csv');  datAdhesion$AdhésionDébut <- as.Date(datAdhesion$AdhésionDébut)
  
            
  output$plot4 <- renderPlot({
    
                             #Create the data (filter data based on the select date range that user will chose)
                                datAdhesion_subset <- datAdhesion %>% dplyr::filter(between(AdhésionDébut, 
                                                                                            input$DateRangeSelected[1], 
                                                                                             input$DateRangeSelected[2]) )
    
                                 histo <- datAdhesion_subset %>% ggplot(
                                                                        aes(x = AdhésionDébut, y = cum)) +
                                                                         ylab("Nombre de membres actifs") +
                                                                         xlab("Date de l'adhésion") +
                                                                          geom_line()
          
                                  histo
  
                              })#END NB actifs
  
  
  #progressBox
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  #approvalBox2
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Actif",paste0(as.numeric(table(sub$MembreStatut)[1]), "%"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}

shinyApp(ui, server)