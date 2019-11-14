library(shiny)
library(shinydashboard)



dashboardPage(
    dashboardHeader(title = "Détection des fraudes sur les cartes de crédits", titleWidth=800),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            menuItem("Données", tabName = "datafile", icon = icon("table")),
            menuItem("Analyse", tabName = "analysis", icon = icon("chart-bar")
            ),
            menuItem("Paper", tabName = "paper", icon = icon("file-pdf-o")),
            hr(),
            sidebarUserPanel(name = a("Camille AZANGHO",
                                      href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                             subtitle = "brunellekas@gmail.com"),br(),
            sidebarUserPanel(name = a("MAKHOKH Lamyae",
                                      href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                             subtitle = "lam.makhokh@gmail.com"),br(),
            sidebarUserPanel(name = a("KITIHOUN Nancy",
                                      href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                             subtitle = "nancy.kitihoun@yahoo.com")
            
            
            
            
        ),
        tags$head(tags$style(HTML(
            '/*header title font*/
      .main-header .logo{
      font-family: "Geogia",Times,"Times New Roman",Serif;
      font-weight:  bold;
      font-size: 24px;
      }
      
      /*background color of hearder (logo part)*/
      .skin-blue  .main-header .logo{
      background-color: #8a6777;
      }
      
      /* change the background color of hearder (logo part) when mouse hover */
      .skin-blue .main-header .logo:hover {
      background-color:orange;
      }
      
      /*background color for remaining part of the header */
      .skin-blue .main-header .navbar {
      background-color: #67818a;
      }
      
      /* main sidebar */
      .skin-blue .main-sidebar {
      background-color:#67818a;
      }
      
      
      /* active sidebar menu item */
      
      .skin-blue .main-sidebar .sidebar-menu .active a{
      background-color: white;
      color:black;
      
      }
      /*  sidebar menuitems */
      .skin-blue .main-sidebar .sider .sidebar-menu a{
      background-color: orange;
      color:white;
      
      }
      /* sider menuitems when mouse hover */
      
      .skin-blue .main-sidebar  .sidebar .sidebar-menu a:hover{
      background-color: brown;
      color:white;
      
      }
      /* sidebar toggle button */
      .skin-blue .main-header  .navbar .sidebar-toggle:hover {
      background-color:black;
      }
      
      ')))
        
    ),
    dashboardBody(
        
        
        tabItems(
            #Introduction 
            # Read data
            tabItem(tabName = "intro", includeHTML("D:/M2/SVM/test/intro.html")), 
            # Read data
            
            tabItem(tabName = "datafile",  
                    
                    
                    # The id lets us use input$tabset1 on the server to find the current tab
                    
                    tabBox(selected = "Description1",width = 12,
                           tabPanel("Stat.descriptives1",
                                    
                                    box(width = 12, status = "primary",
                                        title = "detection de fraude sur des cartes bancaires", 
                                        DT::dataTableOutput("dataTable")
                                    ),
                                    fluidPage(
                                        box(status = "warning", 
                                            title=" simple statistique descriptive",
                                            DT::dataTableOutput("Des1"))),
                                    
                                    box(status = "info",tags$footer(tags$em(
                                        "le nombre de valeurs nulles (q_zeros) et son pourcentage (p_zeros)",br(),
                                        "le nombre de valeurs manquantes (q_na) et son pourcentage (p_na)",br(),
                                        "le nombre de valeur infinies (q_inf) et son pourcentage (p_inf)",br())),
                                        
                                        DT::dataTableOutput("Des2"))),
                           tabPanel("Stat.descriptives2", 
                                    
                                    fluidPage(
                                        box( status = "primary",
                                             
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     
                                                     #Definition 1er menu deroulant
                                                     selectInput("variable1", "Variable (axe des x):",
                                                                 list("Time" , "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                                      "V9","V10","V11" , "V12","V13","V14"  , 
                                                                      "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                                      "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount" )),
                                                     
                                                     #Definition du second menu deroulant
                                                     selectInput("variable2", "Variable (axe des y):",
                                                                 list( "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                                       "V9","V10","V11" , "V12","V13","V14"  , 
                                                                       "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                                       "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount","Time"))),
                                                 
                                                 mainPanel(  plotOutput("plot2"),width = "100%",tags$footer(tags$em("le lien entre les variables explicatives"))))),
                                        box(with=5 , status = "info",  plotOutput("Grap2", height = "300px"), tags$footer(tags$em("Proportion des 0 et 1  dans la variable dépendante"))),
                                        box( with=5 ,title="Corrélation", plotOutput("tabCor") ,status = "primary",tags$footer(tags$em("En probabilités et en statistique, 
                              la corrélation entre plusieurs variables aléatoires ou statistiques est une notion de liaison qui contredit leur indépendance.
                               Ici  nous avons des varaiables trés peu correler entre elles "))))
                           ),
                           tabPanel("Densité des variables",status = "info",width = 24,
                                    plotOutput("Grap3", width = "100%" ),tags$footer(tags$em("Les variables sont toutes centrées sur zéro "))
                                    
                           ))),
            # Analyse
            tabItem(tabName="analysis",  
                    
                    fluidRow(
                        sidebarPanel(position = "center",
                                     sliderInput("prop ", "Choisir la propotion des 0 dan l'echantillon d'apprentissage" , min=0.01, max=0.1, value=0.003,
                                                 width = '95%'),br(),
                                     actionButton(inputId="proportion", "Go!"),
                                     p("Click the button to update the value displayed in the main panel.")),
                        
                        mainPanel(
                            
                            tabBox(type = "pills",selected = "SVM",width = 12,
                                   
                                   tabPanel("SVM", 
                                            fluidPage(
                                                box(width = 6, verbatimTextOutput("svm1"),status = "primary", title="sortie svm"),
                                                box(width=6,verbatimTextOutput("performence"),status = "primary",title="matrice de confusion"),
                                                box(plotOutput("ROC", height = "300px" ),status = "primary",title="courbe AUC_SVM"))
                                            
                                            
                                            
                                   ),
                                   tabPanel("Abre de classification",
                                            fluidPage(
                                                box(width = 6, verbatimTextOutput("tree"),status = "primary", title="sortie de arbre de classification",tags$footer(tags$em("Misclassification error rate estt ici le taux d'erreure d'apprentissage. Comme nous pouvons le voir est faible "))),
                                                box(width = 6, plotOutput("abre", height = "300px" ),status = "primary",title="Performence de la méthode sur des données test"),
                                                box(width = 6, verbatimTextOutput("treroc"),status = "primary"),
                                                box(plotOutput("ROC2", height = "300px" ),status = "primary",title="courbe AUC_AM"))
                                            
                                   ),
                                   tabPanel("gradient boosting",
                                            fluidPage(
                                                box(width = 6, plotOutput("gradient"),status = "primary", title="gradient boosting"),
                                                box(width = 6, verbatimTextOutput("perfgradient" ),status = "primary"))
                                            #box(plotOutput("ROC23", height = "300px" ),status = "primary",title="courbe AUC_Boost")
                                   ),
                                   tabPanel("logistique",
                                            fluidPage(
                                                box(width = 6, verbatimTextOutput("logs"),status = "primary", title="logistique"),
                                                box(width = 6, verbatimTextOutput("perflog" ),status = "primary",title="performance"),
                                                # box(plotOutput("ROC4", height = "300px" ),status = "primary",title="courbe AUC_logistique"))
                                                
                                            ))))
                        
                        
                        
                        
                    )))))