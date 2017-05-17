library(dplyr)
library(shinydashboard)
library(shinyFiles)
library(stringr)
library(psych)
library(corrgram)
library(dendextend)

#Definimos los estilos generales que vamos a usar en el diseño de la aplicación
blueStyle="color: #fff; background-color: #337ab7; border-color: #2e6da4"

#tags$style(type='text/css', "#dosvariables_Action_relacionTab { width:100%; margin-top: 25px;}"),
#tags$dashboardBody(tags$style(HTML(.sidebar {height: 90vh; overflow-y: auto;})))
  
dashboardPage(
  dashboardHeader(title = "SMART DATA"),
  dashboardSidebar(
    #SideBar con las opciones de la aplicación
    sidebarMenu(
      id = "tabs",
      menuItem("CARGA DE DATOS", tabName = "carga",icon = icon("upload")),
      menuItem("OPERACIONES", tabName = "operaciones", icon = icon("sticky-note-o"),
              collapsible = TRUE,
              menuSubItem("Consultas", tabName = "consulta",icon = icon("book")),
              menuSubItem("Limpieza", tabName = "limpieza",icon = icon("shower")),
              menuSubItem("Edición", tabName = "edicion",icon = icon("edit"))
               ),
     
      menuItem("EXPLORACIONES", tabName = "exploracionDatos", icon = icon("binoculars"),
               collapsible = TRUE,
               menuSubItem("Factorizar", tabName = "factorizar",icon = icon("tag")),
               menuSubItem("Una Variable", tabName = "unaVariable",icon = icon("line-chart")),
               menuItem("Dos Variables", tabName = "dosVariables",icon = icon("map-signs"),collapsible = TRUE,
                           menuSubItem("Exp. Gráfica", tabName = "expGrafica",icon = icon("map-signs")),
                           menuSubItem("Correlaciones", tabName = "correlacionesdosvar",icon = icon("handshake-o"))
                        ),
               menuItem("Multivariable", tabName = "multivariable",icon = icon("star-o"),collapsible = TRUE,
                          menuSubItem("Exp. Gráfica", tabName = "multi_expGrafica",icon = icon("map-signs")),
                          menuSubItem("Correlaciones", tabName = "multi_cor",icon = icon("handshake-o"))
               )),
      
      menuItem("R.LINEAL", tabName = "regresionlineal", icon = icon("line-chart"),
               collapsible = TRUE,
               menuSubItem("RL Simple", tabName = "reglineal_simple",icon = icon("line-chart")),
               menuSubItem("RL Múltiple", tabName = "reglineal_multi",icon = icon("line-chart"))),
      
      menuItem("CLUSTERS", tabName = "clusters",icon = icon("snowflake-o"),
               collapsible = TRUE,
               menuSubItem("K-means", tabName = "kmeans",icon = icon("braille")),
               menuSubItem("Jerarquía", tabName = "jerarquia",icon = icon("tree")),
               menuSubItem("Evaluaciónes", tabName = "evaluaciones",icon = icon("tree"))),
      
      menuItem("BBDD", tabName = "basesDeDatos", icon = icon("database"),
           collapsible = TRUE,
           menuSubItem("MongoDB", tabName = "mongodb",icon = icon("envira")))
      )),
  
  dashboardBody(style = 'overflow-y: scroll', #De esta manera añadimos un scroll vertical al Body
    tabItems(
      tabItem(tabName = "carga",
                      fluidRow(
                                box(fileInput('datafile', 'Selecciona CSV',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                    verbatimTextOutput("mensajes_carga"))),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado",
                                    box(title = "Datos Cargados", width = NULL, status = "primary",
                                     div(style = 'overflow-x: scroll', tableOutput("filetable"))
                                   )))
                      #fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_carga")))
              ),
                    
      tabItem(tabName = "consulta",
              tags$style(type='text/css', '#controlDeCarga_Consulta {background-color: rgba(0,0,255,0.10);font-weight: bold; color: black;font-size: 14px}'),             
              verbatimTextOutput("controlDeCarga_Consulta"),
              conditionalPanel(condition="output.filedatacargado",
              fluidRow(
                       box(
                            uiOutput("variables"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("var1")),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("val1")),
                            br(),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("var2")),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("val2")),
                            br(),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            br(),
                            
                            #The action button prevents an action firing before we're ready
                            actionButton("SeleccionarVariables", "Seleccionar Variables",style=blueStyle),
                            br(),
                            br(),
                            verbatimTextOutput("consulta_msj")
                          ),
                      box(title="Estructura del Dataset",verbatimTextOutput("TextoSTR",placeholder = TRUE)
                      ),
                      box(#titlePanel("Guardar archivo"),
                          shinySaveButton("guardarFiltro", "Guardar Filtro", "Guardar archivo como ...", filetype=list(csv="csv"))
                          
                      )),
              fluidRow(tableOutput("filetablecolumnas")))
            ),
      
      tabItem(tabName = "limpieza",
              tags$style(type='text/css', '#controlDeCarga_Limpieza {background-color: rgba(0,0,255,0.10); color: blue;}'),
              verbatimTextOutput("controlDeCarga_Limpieza"),
              conditionalPanel(condition ="output.filedatacargado",
                fluidRow(
                  box(title="Buscar Valores NA",width = 12,
                    actionButton("valoresNA", "Buscar valores NA",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                    actionButton("eliminarNA_Limpiar","Eliminar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    actionButton("restaurar_Limpiar","Restaurar Dataset",style="float:right;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    br(),
                    br(),
                    verbatimTextOutput("mensajes_limpiezaNA"),
                    tableOutput("resultados_limpiezaNA")
                    )
                  ),
                
                fluidRow(
                  box(title="Buscar Valores Anómalos",width = 12,
                    div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosLimpieza")),
                    div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoDato_Limpieza", "Tipo de Dato:",
                                                                                                     c("String" = "string",
                                                                                                       "Número" = "numero"
                                                                                                       ))),
                    div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                    tags$style(type='text/css', "#valoresAnomalos_Buscar { width:100%; margin-top: 25px;}"),
                    div(style="display: inline-block;vertical-align:top; width: 100px;",actionButton("valoresAnomalos_Buscar","Buscar",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                    tags$style(type='text/css', "#valoresAnomalos_Limpiar { width:100%; margin-top: 25px;}"),
                    div(style="display: inline-block;vertical-align:top; width: 120px;",actionButton("valoresAnomalos_Limpiar","Eliminar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    br(),
                    verbatimTextOutput("mensajes_limpiezaAnomalos"),
                    tableOutput("resultados_limpiezaAnomalos")
                    #div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("eliminarValoresErroneos_Limpiar","Borrar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    
              )),
                
                #fluidRow(box(title="Resultados",width = 12, tableOutput("resultados_limpieza"))),
                #fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_limpieza"))),
                fluidRow(box(width = 12,
                             shinySaveButton("guardar_limpieza", "Guardar Cambios", class="shinySave btn-primary","Guardar archivo como ...", filetype=list(csv="csv"))
                             )))
              ),
              
      tabItem(tabName = "edicion",
              tags$style(type='text/css', '#controlDeCarga_Edicion {background-color: rgba(0,0,255,0.10); color: blue;}'),
              verbatimTextOutput("controlDeCarga_Edicion"),
              conditionalPanel(condition="output.filedatacargado",
               fluidRow(
                box(title="Añadir Atributos",width = 12,
                  div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("nuevoAtributo", "Nuevo Atributo:")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosEdicion")),
                  div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("operacion", "Operacion:",
                                                                                                   c("Suma" = "sum",
                                                                                                     "Resta" = "rest",
                                                                                                     "Multiplicación" = "mul",
                                                                                                     "División" = "div"))),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  
                  div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("edicion_tipoDato", "Tipo de dato:",c("Factor","Atributo"),selected=1,width="150px")),
                  
                  div(style="display: inline-block;vertical-align:top; width: 100px;",conditionalPanel(condition="input.edicion_tipoDato=='Factor'",textInput("factorNumerico", "Valor",width="100px"))),
                  div(style="display: inline-block;vertical-align:top; width: 100px;",conditionalPanel(condition="input.edicion_tipoDato=='Atributo'",div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("otroAtributo")))),

                  br(),
                  div(style="display: inline-block;vertical-align:bottom; width: 150px;",actionButton("ejecutarAtributo", "Ejecutar",style=blueStyle))
              )),
      fluidRow(box(title = "Tabla Resultante", width = NULL, status = "primary",
                   div(style = 'overflow-x: scroll', tableOutput("filetabledicion")))),

      fluidRow(box(width = 12,
                   shinySaveButton("guardar_edicion", "Guardar Cambios", class="shinySave btn-primary","Guardar archivo como ...", filetype=list(csv="csv"))))
      )),
      
      tabItem(tabName="factorizar",
              fluidRow(box(title="Sumario del Dataset",width = 12,
                           verbatimTextOutput("dosvariables_sumarioGeneral",placeholder = TRUE))),
              # fluidRow(
              #   box(title="Factorizar",width = 12,
              #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosCambioDeTipos")),
              #       div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("factores", "Factores (;)")),
              #       br(),
              #       div(style="display: inline-block;vertical-align:bottom; width: 150px;",actionButton("ejecutarFactorizacion", "Ejecutar",style=blueStyle))
              #   )),
              fluidRow(box(title="Factorizar una varibale",width = 12,
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_atributos")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("dosvariables_TextInput_intervalos", "Nº de intervalos")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           tags$style(type='text/css', "#dosvariables_Action_factorizar { width:100%; margin-top: 25px;}"),
                           tags$style(type='text/css', "#dosvariables_Action_factoReset { width:100%; margin-top: 25px;}"),
                           div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvariables_Action_factorizar", "Ejecutar",style=blueStyle)),
                           div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvariables_Action_factoReset", "Reset",style=blueStyle)),
                           br(), 
                           br(),
                           verbatimTextOutput("dosvariables_mensajes_factorizar"),
                           verbatimTextOutput("dosvariables_mensajes_print")
              ))),
              
              # fluidRow(box(title="Mensajes",width = 12,
              #              verbatimTextOutput("mensajes_factorizar"))),
              # fluidRow(box(title = "Datos Resultantes", width = NULL, status = "primary",
              #              div(style = 'overflow-x: scroll', verbatimTextOutput("edicion_print"))))
              # ),
      
      tabItem(tabName = "unaVariable",
              tags$style(type='text/css', '#controlDeCarga_Exploracion1 {background-color: rgba(0,0,255,0.10); color: blue;}'),
              verbatimTextOutput("controlDeCarga_Exploracion1"),
              conditionalPanel(condition ="output.filedatacargado",
              
              fluidRow(
                box(title="Exploración Tabular de una Variable",width = 12,
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariable")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracion1", "Tipo de Exploración:",
                                                                                                 c("Sumario" = "sumario",
                                                                                                   "Media" = "media",
                                                                                                   "Desviación St." = "desviacion",
                                                                                                   "Varianza" = "varianza"))),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  #Añadimos el estilo al botón de Ejecutar
                  tags$style(type='text/css', "#Exploraciones_ejecutar1 { width:100%; margin-top: 25px;}"),
                  div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("Exploraciones_ejecutar1", "Ejecutar",style=blueStyle)),
                  br(),
                 verbatimTextOutput("mensajes_exploracion1"),
                 verbatimTextOutput("resultados_exploracion1")
                 )),

              fluidRow(box(title="Exploración Gráfica de una Variable",width = 6,
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariableGrafica")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracionGrafica1", "Gráfica 1:",
                                                                                                     c("Histograma" = "histograma",
                                                                                                       "Diagrama de Caja" = "caja",
                                                                                                       "Plot" = "plot"))),
                           verbatimTextOutput("mensajes_exploracionGrafica")
                        ),
                       box(title="Exploración Gráfica de una Variable",width = 6,
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariableGrafica2")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracionGrafica2", "Gráfica 2:",
                                                                                                            c("Histograma" = "histograma",
                                                                                                              "Diagrama de Caja" = "caja",
                                                                                                              "Plot" = "plot"))),
                           verbatimTextOutput("mensajes_exploracionGrafica2")
                       )
                ),
            
                fluidRow(box(title="Gráfica 1",width = 6,
                           plotOutput("explor1_grafica1",click = "plot1_click")),
                       box(title="Grafica 2",width = 6,
                           plotOutput("explor1_grafica2",click = "plot2_click2"))
                       
              )
              
    )),
      
    tabItem(tabName = "expGrafica",
            # fluidRow(box(title="Sumario del Dataset",width = 12,
            #              verbatimTextOutput("dosvariables_sumarioGeneral",placeholder = TRUE))),
                         
            # fluidRow(box(title="Factorizar una varibale",width = 12,
            #               div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_atributos")),
            #               div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("dosvariables_TextInput_intervalos", "Nº de intervalos")),
            #               div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
            #               tags$style(type='text/css', "#dosvariables_Action_factorizar { width:100%; margin-top: 25px;}"),
            #               div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvariables_Action_factorizar", "Ejecutar",style=blueStyle)),
            #               br(), 
            #               br(),
            #               verbatimTextOutput("dosvariables_mensajes_factorizar"),
            #               verbatimTextOutput("dosvariables_mensajes_print")
            #               )),
            fluidRow(box(title="Relación tabular entre dos variables",width = 12,
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_rela_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_rela_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#dosvar_Action_relaTab { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvar_Action_relaTab", "Ejecutar",style=blueStyle)),
                         verbatimTextOutput("dosvar_Print_relaTab")
                        )
                         
                     ),
            fluidRow(box(title="Exploración Gráfica de dos Variables",width = 12,
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoDosVariablesGraficas1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoDosVariablesGraficas2")),
                         #div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         #div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("dosvar_tipoExploracionGrafica", "Gráfica 1:",
                         #                                                                                 c("FF" = "ff",
                         #                                                                                  "FN" = "fn",
                         #                                                                                   "NN" = "nn"))),
                         verbatimTextOutput("mensajes_dosvar_exploracionGrafica"),
                         plotOutput("explor1_dosvar_grafica1",click = "plot1_dosvar_click")

                         #br(),
                         #div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("ExploracionesGraficas_ejecutar1", "Ejecutar",style=blueStyle))
                      ))
            ),


    
    #CORRELACIONES ENTRE VARIABLES
    tabItem(tabName = "correlacionesdosvar",
            fluidRow(box(tags$p("CORRELACIÓN ENTRE DOS ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                  br(),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at1")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at2")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  tags$style(type='text/css', "#dosvar_Action_correlacion { width:100%; margin-top: 25px;}"),
                  div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvar_Action_correlacion", "Test de Correlación",style=blueStyle)),
                  br(), 
                  verbatimTextOutput("dosvar_msj_correlacion"),
                  verbatimTextOutput("dosvar_Print_correlacion"),
                  plotOutput("graf_correla_dosVariables",click = "plot1_correladosvar_click"))
          
            )),
              
           
    tabItem(tabName = "multi_expGrafica",
            fluidRow(box(tags$p("ANÁLISIS GRÁFICO MULTIVARIABLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         #div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("multivariable_Ui_gra_at1")),
                         #div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         #tags$style(type='text/css', "#multivar_Action_gra { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("multivar_Action_gra", "Relaciones Gráficas",style=blueStyle)),
                         br(), 
                         br(),
                         verbatimTextOutput("multivar_msj_graf"),
                         plotOutput("multivar_graf_plot",click = "multivar_graf_plot_click"))
                     
            )),          
                     
    tabItem(tabName = "multi_cor",
            fluidRow(box(tags$p("CORRELACIÓN MULTIVARIABLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         #div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at1")),
                         #div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at2")),
                         #div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         #tags$style(type='text/css', "#multivar_Action_correlacion { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("multivar_Action_correlacion", "Test de Correlación",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("multivar_msj_correlacion"),
                         verbatimTextOutput("multivar_print_correlacion"),
                         plotOutput("multivar_graf_correla",width = "100%", height = "800px",click = "plot1_correlamultivar_click"))

            )),
    
    tabItem(tabName = "reglineal_simple",
            fluidRow(box(tags$p("REGRESIÓN LINEAL SIMPLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealsimple_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealsimple_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#reglinealsimple_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("reglinealsimple_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("reglienalsimple_msj"),
                         verbatimTextOutput("reglienalsimple_print"))
                     ),
                         
                         fluidRow(box(width = 6,
                                      plotOutput("reglienalsimple_plot1",click = "reglienalsimple_plot_click1")),
                                  box(width = 6,
                                      plotOutput("reglienalsimple_plot2",click = "reglienalsimple_plot_click2")),
                                  box(width = 12,
                                      plotOutput("reglienalsimple_plot3",click = "reglienalsimple_plot_click3"))
                         ),
                          fluidRow(box(tags$p("PREDICCIONES DEL MODELO SLR", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                                       br(),
                                       div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("valorX", "Valores de X (sep=;)")),
                                       div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                       div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("intervaloConfianza", "Int. de Confianza",
                                                                                                                        c("99%" = ".99",
                                                                                                                          "95%" = ".95",
                                                                                                                          "90%" = ".90"))),
                                       tags$style(type='text/css', "#SLR_prediccion_Action { width:100%; margin-top: 25px;}"),
                                       div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("SLR_prediccion_Action", "Predecir Valores",style=blueStyle)),
                                       verbatimTextOutput("SLR_prediccion_print"))
                             )),
    tabItem(tabName = "reglineal_multi",
            fluidRow(box(tags$p("REGRESIÓN LINEAL MÚLTIPLE", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("reglinealmulti_at1","Columnas X (sep=;)")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealmulti_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#reglinealmulti_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("reglinealmulti_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("reglienalmulti_msj"),
                         verbatimTextOutput("reglienalmulti_print"))
            ),
            fluidRow(box(width = 6,
                         plotOutput("reglienalmulti_plot1",click = "reglienalmulti_plot1_click")),
                     box(width = 6,
                         plotOutput("reglienalmulti_plot2",click = "reglienalmulti_plot2_click")),
                     box(tags$p("GRÁFICAS DEL MODELO", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         plotOutput("reglienalmulti_plot3",click = "reglienalmulti_plot3_click"))
            )),
    #Clusters
    tabItem(tabName = "kmeans",
            fluidRow(box(tags$p("ALGORITMO K-MEANS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("cluster_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("cluster_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("cluster_n1","Nº Clusters 1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("cluster_n2","Nº Clusters 2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#cluster_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("cluster_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         verbatimTextOutput("cluster_msj")
                         )),

                  fluidRow(
                           conditionalPanel(condition ="input.cluster_Action",
                              box(width = 6, 
                              plotOutput("cluster_plot1",click = "cluster_plot1_click"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("cluster_explo1", "Cluster 1",
                                                                                                               c("sumario"="Sumario",
                                                                                                                 "cluster" = "Clusters",
                                                                                                                 "centers" = "Centers",
                                                                                                                 "totss" = "Totss",
                                                                                                                 "withinss"="Withinss",
                                                                                                                 "tot.withinss"="Tot.Withinss",
                                                                                                                 "betweens"="Betweens",
                                                                                                                 "size"="Size",
                                                                                                                 "iter"="Iter",
                                                                                                                 "ifault"="Ifault"),
                                                                                                                  selected="Sumario"
                                                                                                                 )),
                              verbatimTextOutput("cluster_print1"))),
                           
                          conditionalPanel(condition ="input.cluster_Action",
                              box(width = 6, 
                              plotOutput("cluster_plot2",click = "cluster_plot2_click"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("cluster_explo2", "Cluster 2",
                                                                                                                c("sumario"="Sumario",
                                                                                                                "cluster" = "Clusters",
                                                                                                                "centers" = "Centers",
                                                                                                                "totss" = "Totss",
                                                                                                                "withinss"="Withinss",
                                                                                                                "tot.withinss"="Tot.Withinss",
                                                                                                                "betweens"="Betweens",
                                                                                                                "size"="Size",
                                                                                                                "iter"="Iter",
                                                                                                                "ifault"="Ifault"),
                                                                                                                  selected="Sumario"
                                                                                                               )),
                              verbatimTextOutput("cluster_print2")))
                          
                          
                  )),
    #Jerarquía
    tabItem(tabName = "jerarquia",
            fluidRow(box(tags$p("CLUSTERING JERARQUICO", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("clusterj_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("clusterj_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("clusterj_nclusters","Número de clusters:")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#clusterj_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clusterj_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         verbatimTextOutput("clusterj_msj")
            )),
            fluidRow(
              conditionalPanel(condition ="input.clusterj_Action",
                               box(width = 12, 
                                   plotOutput("clusterj_plot1",click = "clusterj_plot1_click"),
                                   div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("clusterj_corte","Valor de corte:")),
                                   tags$style(type='text/css', "#clusterj_AddValorCorte { width:100%; margin-top: 25px;}"),
                                   div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clusterj_AddValorCorte", "Dibujar",style=blueStyle)),
                                   br(),
                                   verbatimTextOutput("clusterj_print")
                               ))
            )),
            
    #Evaluación de los clusters    
    tabItem(tabName = "evaluaciones",
            fluidRow(box(tags$p("EVALUACIONES DE LOS ALGORITMOS DE CLUSTERING", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("clustereva_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("clustereva_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#clustereva_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clustereva_Action", "Evaluar",style=blueStyle)),
                         br(),
                         verbatimTextOutput("clustereva_msj"))),
            fluidRow(conditionalPanel(condition ="input.clustereva_Action",
                     box(tags$p("GRÁFICAS DE EVALUACIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                          plotOutput("clusterelbow_plot1",click = "clustereelbow_plot1_click"),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("clustereva_compa","2 valores (;)")),
                         tags$style(type='text/css', "#clustereva_CompaAction { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clustereva_CompaAction", "Comparar",style=blueStyle)),
                         br()))),
            fluidRow(conditionalPanel(condition ="input.clustereva_CompaAction",           
                       box(tags$p("GRÁFICAS ", style = "font-size: 115%;color:blue;font-weight: bold"),width = 6,
                          plotOutput("clustereva_plot1",click = "clustereva_plot1_click"),
                          plotOutput("clustereva_plot2",click = "clustereva_plot2_click")),
                       box(tags$p("GRÁFICAS ", style = "font-size: 115%;color:blue;font-weight: bold"),width = 6,
                          plotOutput("clustereva_plot3",click = "clustereva_plot3_click"),
                          plotOutput("clustereva_plot4",click = "clustereva_plot4_click")),
                          br(),
                          verbatimTextOutput("clustereva_print")
                      ))
                    ),
            
                 
    #MongoDB         
    tabItem(tabName = "Mongodb",
              h2("Mongodb tab content")
      )
    ) #el del DIV del dashboard
  )
)
