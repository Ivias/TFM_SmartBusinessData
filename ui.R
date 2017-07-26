# Library 
library(shinyjs)
library(shiny)
library(stats)
library(dplyr)
library(shinydashboard)
library(shinyFiles)
library(stringr)
library(psych)
library(corrgram)
library(dendextend)

#Series temporales
library(forecast)
library(lubridate) #Agregacion de datos en series temporales y fechas

#Filtrado colaborativo
library(reshape2)
library(recommenderlab)

#Visualizaciones
library(ggplot2)
library(scales)
library(magrittr)
library(leaflet)
library(TSP)
library(shinycssloaders) #Animaciones CSS - withSpinner

#APIs
library(httr)
library(jsonlite)

#Redes Neuronales
library(neuralnet)

#Mongodb
library(mongolite)


#---------CSS----------------------------------------
#Definimos los estilos generales que vamos a usar en el diseño de la aplicación
blueStyle="color: #fff; background-color: #337ab7; border-color: #2e6da4"

dashboardPage(
  dashboardHeader(title = "SMART BUSINESS ANALYTICS"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu"))
                ),
  
  dashboardBody(style = 'overflow-y: scroll', #De esta manera añadimos un scroll vertical al Body
    tabItems(
      tabItem(tabName = "importacionDatos",
              fluidRow(box(width = 12,
                fluidRow(
                box(width=12, radioButtons("tipoImport", tags$p("TIPO DE ARCHIVO A IMPORTAR", style = "font-size: 120%;color:blue;font-weight: bold"),
                                  c("CSV" = "csv",
                                    "API" = "api")))
                ),
     
                  fluidRow( conditionalPanel(condition="input.tipoImport=='csv'",
                                                
                                  box(width = 6,
                                  fileInput('datafile', tags$p("Seleccionar archivo CSV", style = "font-size: 120%;color:blue;font-weight: bold"),
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                                   ),
                                  box(width=6, tags$p("Opciones de Importación", style = "font-size: 120%;color:blue;font-weight: bold"),
                                      div(style="display: inline-block;vertical-align:top; width: 150px;",radioButtons("importFactor","¿Importar factores?:",
                                                                                                                       c("TRUE" = "trueFactor",
                                                                                                                         "FALSE" = "falseFactor"),selected="trueFactor")),
                                      div(style="display: inline-block;vertical-align:top; width: 150px;", radioButtons("delimitadorCSV","Delimitador del csv:",
                                                                                                                        c("," = "csvComa",
                                                                                                                          ";" = "csvPuntoyComa")))))
                      ),
                  fluidRow(conditionalPanel(condition="input.tipoImport=='api'",
                                box(tags$p("Importar datos desde API-WEB", style = "font-size: 120%;color:blue;font-weight: bold"),width = 9,
                                    div(style="display: inline-block;vertical-align:top; width: 400px;",textInput("URL", "URL Compuesta:")),
                                    br(),
                                    tags$style(type='text/css', "#API_Action { width:20%; margin-top: 25px;}"),
                                    actionButton("API_Action", "Importar",style=blueStyle)
                                    ),
                                    box(tags$p("Nodos del documento", style = "font-size: 120%;color:blue;font-weight: bold"),width = 3,
                                        radioButtons("requiereNodo", "¿Extraer un nodo en particular?",
                                                 c("NO" = "no",
                                                   "SI" = "si")),
                                    
                                    conditionalPanel(condition="input.requiereNodo=='si'",div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("nodo")),
                                                     br(),
                                                     tags$style(type='text/css', "#API_nodeAction { width:100%; margin-top: 25px;}"),
                                                     actionButton("API_nodeAction", "Extraer Nodo",style=blueStyle))
                                    ))
                        ))),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado || input.API_Action",
                                                box(tags$p("Mensaje del Sistema", style = "font-size: 120%;color:blue;font-weight: bold"), width = 12,
                                                    verbatimTextOutput("API_msj")))
                      ),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado",
                                    box(tags$p("Datos Cargados", style = "font-size: 120%;color:blue;font-weight: bold"), width = 12, status = "primary",
                                     div(style = 'overflow-x: scroll', tableOutput("filetable"))
                                   )))
              ),
      
      
      tabItem(tabName = "consulta",
              
              fluidRow(
                        box(tags$p("ESTRUCTURA DEL DATASET", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                            br(),
                            verbatimTextOutput("TextoSTR",placeholder = TRUE)
                        ),
                        box(tags$p("FILTRO DE BÚSQUEDA", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                            br(),
                            div(style="display: block; text-align:left; width: 100%",uiOutput("variables")),
                            br(),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("var1")),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("val1")),
                            br(),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("var2")),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("val2")),
                            br(),
                            tags$style(type='text/css', "#SeleccionarVariables { width:100%; margin-top: 25px;}"),
                            div(style="display: inline-block;vertical-align:bottom; width: 150px;",actionButton("SeleccionarVariables", "Filtrar Datos",style=blueStyle)),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                            shinySaveButton("guardarFiltro", "Guardar Cambios", class="shinySave btn-info", "Guardar archivo como ...", filetype=list(csv="csv")),
                            br(),
                            br(),
                            verbatimTextOutput("consulta_msj")
                            )),
                      
                    fluidRow(conditionalPanel(condition="input.SeleccionarVariables",box(tags$p("Datos Filtrados", style = "font-size: 120%;color:blue;font-weight: bold"), width = 12, status = "primary",
                  div(style = 'overflow-x: scroll', tableOutput("filetablecolumnas")))))
            ),
      
              
      tabItem(tabName = "edicion",
              
               fluidRow(
                  box(tags$p("AÑADIR ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                    br(),
                    div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("nuevoAtributo", "Nuevo Atributo:")),
                    div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosEdicion")),
                    div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top; width: 130px;", selectInput("operacion", "Operacion:",
                                                                                                     c("Suma" = "sum",
                                                                                                       "Resta" = "rest",
                                                                                                       "Multiplicación" = "mul",
                                                                                                       "División" = "div"))),
                    div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                    
                    div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("edicion_tipoDato", "Tipo de dato:",c("Factor","Atributo"),selected=1,width="150px")),
                    
                    div(style="display: inline-block;vertical-align:top; width: 100px;",conditionalPanel(condition="input.edicion_tipoDato=='Factor'",textInput("factorNumerico", "Valor",width="100px"))),
                    div(style="display: inline-block;vertical-align:top; width: 100px;",conditionalPanel(condition="input.edicion_tipoDato=='Atributo'",div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("otroAtributo")))),
                    br(),
                    tags$style(type='text/css', "#ejecutarAtributo { width:100%; margin-top: 25px;}"),
                    div(style="display: inline-block;vertical-align:bottom; width: 150px;",actionButton("ejecutarAtributo", "Añadir Atributo",style=blueStyle)),
                    div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                    shinySaveButton("guardar_edicion", "Guardar Cambios", class="shinySave btn-info","Guardar archivo como ...", filetype=list(csv="csv")),
                    br(),
                    br(),
                    verbatimTextOutput("edicion_msj")
                  )),
               
      fluidRow(conditionalPanel(condition="input.ejecutarAtributo",
                   box(width=12, 
                   tags$p("Dataset Resultante", style = "font-size: 120%;color:blue;font-weight: bold"), status = "primary",
                   div(style = 'overflow-x: scroll', tableOutput("filetabledicion")))
               ))),
        
      tabItem(tabName = "limpieza",
              
                               fluidRow(
                                 box(tags$p("BUSCAR VALORES NA", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,

                                     tags$style(type='text/css', "#valoresNA { width:100%; margin-top: 25px;}"),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("valoresNA", "Buscar Valores NA",style=blueStyle)),
                                     div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                     tags$style(type='text/css', "#eliminarNA_Limpiar { width:100%; margin-top: 25px;}"),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("eliminarNA_Limpiar","Eliminar Valores NA",style=blueStyle)),
                                     div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                     tags$style(type='text/css', "#restaurar_Limpiar { width:100%; margin-top: 25px;}"),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;float: right",actionButton("restaurar_Limpiar","Restaurar Dataset",style=blueStyle)),
                                     br(),
                                     br(),
                                     verbatimTextOutput("mensajes_limpiezaNA"),
                                     tableOutput("resultados_limpiezaNA")
                                 )
                               ),
                               
                               fluidRow(
                                 box(tags$p("BUSCAR VALORES ANÓMALOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                                     br(),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosLimpieza")),
                                     div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoDato_Limpieza", "Tipo de Dato:",
                                                                                                                      c("String" = "string",
                                                                                                                        "Número" = "numero"
                                                                                                                      ))),
                                     div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                                     tags$style(type='text/css', "#valoresAnomalos_Buscar { width:100%; margin-top: 25px;}"),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("valoresAnomalos_Buscar","Buscar",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                     div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                                     tags$style(type='text/css', "#valoresAnomalos_Limpiar { width:100%; margin-top: 25px;}"),
                                     div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("valoresAnomalos_Limpiar","Eliminar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                     br(),
                                     verbatimTextOutput("mensajes_limpiezaAnomalos"),
                                     tableOutput("resultados_limpiezaAnomalos")
                                 )),
                              fluidRow(box(width = 12,
                                            shinySaveButton("guardar_limpieza", "Guardar Cambios", class="shinySave btn-info","Guardar archivo como ...", filetype=list(csv="csv"))
                               ))
      ),
      tabItem(tabName="factorizar",
              
              fluidRow(box(tags$p("SUMARIO DEL DATASET", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                           br(),
                           verbatimTextOutput("dosvariables_sumarioGeneral",placeholder = TRUE))),
             
              fluidRow(box(tags$p("FACTORIZAR ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                           br(),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_atributos")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")), 
                           div(style="display: inline-block;vertical-align:top; width: 150px;",conditionalPanel(condition="output.intervalos=='TRUE'",textInput("dosvariables_TextInput_intervalos", "Nº de intervalos:"))),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           tags$style(type='text/css', "#dosvariables_Action_factorizar { width:100%; margin-top: 25px;}"),
                           tags$style(type='text/css', "#dosvariables_Action_factoReset { width:100%; margin-top: 25px;}"),
                           div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvariables_Action_factorizar", "Factorizar",style=blueStyle)),
                           div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvariables_Action_factoReset", "Reset",style=blueStyle)),
                           br(), 
                           br(),
                           verbatimTextOutput("dosvariables_mensajes_factorizar"),
                           verbatimTextOutput("dosvariables_mensajes_print")
              ))),
              
      
      tabItem(tabName = "unaVariable",
              
              
              fluidRow(box(tags$p("EXPLORACIÓN TABULAR DE UN ATRIBUTO", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                           br(),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariable")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracion1", "Tipo de Exploración:",
                                                                                                 c("Sumario" = "Sumario",
                                                                                                   "Media" = "Media",
                                                                                                   "Desviación St." = "Desviacion",
                                                                                                   "Varianza" = "Varianza"))),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  #Añadimos el estilo al botón de Ejecutar
                  tags$style(type='text/css', "#Exploraciones_ejecutar1 { width:100%; margin-top: 25px;}"),
                  div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("Exploraciones_ejecutar1", "Ejecutar",style=blueStyle)),
                  br(),
                 verbatimTextOutput("mensajes_exploracion1"),
                 verbatimTextOutput("resultados_exploracion1")
                 )),

              fluidRow(box(tags$p("FILTRO DE EXPLORACIÓN GRÁFICA 1", style = "font-size: 120%;color:blue;font-weight: bold"),width = 6,
                           br(),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariableGrafica")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracionGrafica1", "Gráfica 1:",
                                                                                                     c("Histograma" = "histograma",
                                                                                                       "Diagrama de Caja" = "caja",
                                                                                                       "Plot" = "plot",
                                                                                                       "Pie-Chart"="pie_chart"))),
                           verbatimTextOutput("mensajes_exploracionGrafica")
                        ),
                       box(tags$p("FILTRO DE EXPLORACIÓN GRÁFICA 2", style = "font-size: 120%;color:blue;font-weight: bold"),width = 6,
                           br(),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariableGrafica2")),
                           div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracionGrafica2", "Gráfica 2:",
                                                                                                            c("Histograma" = "histograma",
                                                                                                              "Diagrama de Caja" = "caja",
                                                                                                              "Plot" = "plot",
                                                                                                              "Pie-Chart"="pie_chart"))),
                           verbatimTextOutput("mensajes_exploracionGrafica2")
                       )
                ),
            
                fluidRow(box(tags$p("Gráfica 1", style = "font-size: 120%;color:blue;font-weight: bold"),width = 6,
                             withSpinner(plotOutput("explor1_grafica1",click = "explor1_grafica1_click", dblclick = "explor1_grafica1_dblclick",brush = brushOpts(
                               id = "explor1_grafica1_brush",
                               resetOnNew = TRUE
                             ))),verbatimTextOutput("explor1_grafica1_info")),
                       box(tags$p("Gráfica 2", style = "font-size: 120%;color:blue;font-weight: bold"),width = 6,
                           withSpinner(plotOutput("explor1_grafica2",click = "explor1_grafica2_click")),verbatimTextOutput("explor1_grafica2_info"))
                       
              )
              
    ),
      
    tabItem(tabName = "expGrafica",
            
            fluidRow(box(tags$p("RELACIÓN TABULAR ENTRE DOS ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_rela_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_rela_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#dosvar_Action_relaTab { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvar_Action_relaTab", "Ejecutar",style=blueStyle)),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#dosvar_Action_resetTab { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvar_Action_resetTab", "Reset",style=blueStyle)),
                         br(),
                         verbatimTextOutput("dosvar_Print_relaTab")
                        )
                         
                     ),
            fluidRow(box(tags$p("EXPLORACIÓN GRÁFICA DE DOS ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoDosVariablesGraficas1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoDosVariablesGraficas2")),
                         verbatimTextOutput("mensajes_dosvar_exploracionGrafica"),
                         withSpinner(plotOutput("explor1_dosvar_grafica1",click = "plot1_dosvar_click")),verbatimTextOutput("explor1_dosvar_info")
                      ))
            ),


    
    #CORRELACIONES ENTRE VARIABLES
    tabItem(tabName = "correlacionesdosvar",
            
            fluidRow(box(tags$p("CORRELACIÓN ENTRE DOS ATRIBUTOS", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                  br(),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at1")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("dosvariables_Ui_correla_at2")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  tags$style(type='text/css', "#dosvar_Action_correlacion { width:100%; margin-top: 25px;}"),
                  div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("dosvar_Action_correlacion", "Test de Correlación",style=blueStyle)),
                  br(), 
                  verbatimTextOutput("dosvar_msj_correlacion"),
                  verbatimTextOutput("dosvar_Print_correlacion"),
                  conditionalPanel(condition="input.dosvar_Action_correlacion",withSpinner(plotOutput("graf_correla_dosVariables",click = "plot1_correladosvar_click"))))
          
            )),
              
           
    tabItem(tabName = "multi_expGrafica",
            fluidRow(box(tags$p("ANÁLISIS GRÁFICO MULTIVARIABLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("multivar_Action_gra", "Relaciones Gráficas",style=blueStyle)),
                         br(), 
                         br(),
                         verbatimTextOutput("multivar_msj_graf"),
                         conditionalPanel(condition="input.multivar_Action_gra",withSpinner(plotOutput("multivar_graf_plot",click = "multivar_graf_plot_click"))))
                     
            )),          
                     
    tabItem(tabName = "multi_cor",
            fluidRow(box(tags$p("CORRELACIÓN MULTIVARIABLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("multivar_Action_correlacion", "Test de Correlación",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("multivar_msj_correlacion"),
                         verbatimTextOutput("multivar_print_correlacion"),
                         conditionalPanel(condition="input.multivar_Action_correlacion",withSpinner(plotOutput("multivar_graf_correla",width = "100%", height = "800px",click = "plot1_correlamultivar_click"))))

            )),
    
    tabItem(tabName = "reglineal_simple",
            fluidRow(box(tags$p("REGRESIÓN LINEAL SIMPLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealsimple_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealsimple_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#reglinealsimple_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("reglinealsimple_Action", "Ejecutar",style=blueStyle)),
                         verbatimTextOutput("reglienalsimple_msj")
                         )
                    ),
                     
           fluidRow(conditionalPanel(condition="output.salidaOkMostrarVentanasSLR=='TRUE'",
                     box(id="box_SLR1",width=6,
                         br(),
                         verbatimTextOutput("reglienalsimple_print"),
                         br(),
                         verbatimTextOutput("reglienalsimple_MSE")
                     ),
                     box(width=6,
                         withSpinner(plotOutput("reglienalsimple_plotSLR",click = "reglienalsimple_plotSLR_click",dblclick = "reglienalsimple_plotSLR_dblclick",
                                     brush = brushOpts(id = "reglienalsimple_plotSLR_brush",resetOnNew = TRUE))),
                                      verbatimTextOutput("reglienalsimple_plotSLR_info"))
                     )
              ),
                     
          fluidRow(conditionalPanel(condition="output.salidaOkMostrarVentanasSLR=='TRUE'",          
                     box(id="box_SLR2",width = 6,
                        withSpinner(plotOutput("reglienalsimple_plot1",click = "reglienalsimple_plot_click1"))),
                    box(id="box_SLR3",width = 6,
                        withSpinner(plotOutput("reglienalsimple_plot2",click = "reglienalsimple_plot_click2"))),
                    box(id="box_SLR4",width = 6,
                        withSpinner(plotOutput("reglienalsimple_plot3",click = "reglienalsimple_plot_click3"))),
                    box(id="box_SLR5",width = 6,
                        withSpinner(plotOutput("reglienalsimple_plot4",click = "reglienalsimple_plot_click4"))))
           ),
            fluidRow(conditionalPanel(condition="output.salidaOkMostrarVentanasSLR=='TRUE'",box(id="box_SLR6",tags$p("PREDICCIONES DEL MODELO SLR", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("valorX", "Valores de X (sep=;):")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("intervaloConfianza", "Int. de Confianza:",
                                                                                                          c("99%" = ".99",
                                                                                                            "95%" = ".95",
                                                                                                            "90%" = ".90"))),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#SLR_prediccion_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("SLR_prediccion_Action", "Predecir",style=blueStyle)),
                         verbatimTextOutput("SLR_prediccion_print")))
               )),
     
    tabItem(tabName = "reglineal_multi",
            fluidRow(box(tags$p("REGRESIÓN LINEAL MÚLTIPLE", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 350px;",textInput("reglinealmulti_at1","(Atributo X1; Atributo X2;...) / (vacio = ALL):")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("reglinealmulti_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#reglinealmulti_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("reglinealmulti_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("reglienalmulti_msj"),
                         verbatimTextOutput("reglienalmulti_print"),
                         verbatimTextOutput("reglienalmulti_MSE"))
            ),
            fluidRow(conditionalPanel(condition="output.salidaOkMostrarVentanasMLR=='TRUE'",box(width = 6,
                         withSpinner(plotOutput("reglienalmulti_plot1",click = "reglienalmulti_plot1_click"))),
                     box(width = 6,
                         withSpinner(plotOutput("reglienalmulti_plot2",click = "reglienalmulti_plot2_click"))),
                     box(width = 12,
                         withSpinner(plotOutput("reglienalmulti_plot3",click = "reglienalmulti_plot3_click")))
            )),
            fluidRow(conditionalPanel(condition="output.salidaOkMostrarVentanasMLR=='TRUE'",
                     box(tags$p("PREDICCIONES DEL MODELO DE REGRESIÓN MÚLTIPLE", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                           fileInput('regreMultiEvaluation', 'Selecciona CSV para evaluación',
                            accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                  tags$style(type='text/css', "#evalCSVmultiRegre_Action { width:100%; margin-top: 25px;}"),
                  div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("evalCSVmultiRegre_Action", "Predicción",style=blueStyle)),
                  br(),
                  br(),
                  verbatimTextOutput("regreMultiEvaluation_msj")
                  
              )),
              conditionalPanel(condition ="output.fileMLRdatacargado",
                               box(title = "Predicciones - Modelo de Regresión Múltiple", width = 12, status = "primary",
                                   div(style = 'overflow-x: scroll', tableOutput("regreMultiEvaluation"))
                               ))
              )),
               
                         
                         
    #Clusters
    tabItem(tabName = "kmeans",
            fluidRow(box(tags$p("ALGORITMO K-MEANS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("cluster_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("cluster_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("cluster_n1","Nº Clusters 1:")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("cluster_n2","Nº Clusters 2:")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#cluster_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("cluster_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("cluster_msj")
                         )),
            
                  fluidRow(conditionalPanel(condition="output.salidaOKClusters=='TRUE'",
                              box(width = 6, 
                              withSpinner(plotOutput("cluster_plot1",click = "cluster_plot1_click")),
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
                              verbatimTextOutput("cluster_print1")),
                              
                             box(width = 6, 
                              withSpinner(plotOutput("cluster_plot2",click = "cluster_plot2_click")),
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
                              verbatimTextOutput("cluster_print2"))
                          
                          
                  ))),
    
    #Jerarquía
    tabItem(tabName = "jerarquia",
            fluidRow(box(tags$p("CLUSTERING JERÁRQUICO", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
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
                         br(),
                         verbatimTextOutput("clusterj_msj")
            )),
            fluidRow(
              conditionalPanel(condition ="output.salidaOKClustersJe=='TRUE'",
                               box(width = 12, 
                                   withSpinner(plotOutput("clusterj_plot1",click = "clusterj_plot1_click")),
                                   div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("clusterj_corte","Valor de corte:")),
                                   tags$style(type='text/css', "#clusterj_AddValorCorte { width:100%; margin-top: 25px;}"),
                                   div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clusterj_AddValorCorte", "Dibujar",style=blueStyle)),
                                   br(),
                                   verbatimTextOutput("clusterj_print")
                               )),
              conditionalPanel(condition ="output.salidaOKClustersJeCorte=='TRUE'",
                              box(width = 12, 
                                   withSpinner(plotOutput("clusterj_plotFinal",click = "clusterj_plotFinal_click",width = "50%")),
                                   verbatimTextOutput("clusterj_print1"),
                                   verbatimTextOutput("clusterj_print2")
                               )
                            
            ))),
            
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
                         br(),
                         verbatimTextOutput("clustereva_msj"))),
                fluidRow(conditionalPanel(condition ="output.salidaOKClustersEva=='TRUE'",
                     box(tags$p("GRÁFICA DE EVALUACIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         withSpinner(plotOutput("clusterelbow_plot1",click = "clustereelbow_plot1_click")),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("cluster_eval1", "Nº Clusters 1:",
                                                                                                         c("2"="2",
                                                                                                           "3" = "3",
                                                                                                           "4" = "4",
                                                                                                           "5"="5",
                                                                                                           "6"="6",
                                                                                                           "7"="7",
                                                                                                           "8"="8",
                                                                                                           "9"="9",
                                                                                                           "10"="10"),
                                                                                                         selected="dos$cluster"
                         )),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("cluster_eval2", "Nº Clusters 1:",
                                                                                                         c("2"="2",
                                                                                                           "3" = "3",
                                                                                                           "4" = "4",
                                                                                                           "5"="5",
                                                                                                           "6"="6",
                                                                                                           "7"="7",
                                                                                                           "8"="8",
                                                                                                           "9"="9",
                                                                                                           "10"="10"),
                                                                                                         selected="tres$cluster"
                         )),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#clustereva_CompaAction { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("clustereva_CompaAction", "Comparar",style=blueStyle)),
                         br()))),
             fluidRow(conditionalPanel(condition ="output.salidaOKClustersEva=='TRUE' && input.clustereva_CompaAction",           
                       box(tags$p("GRÁFICAS MODELO 1", style = "font-size: 115%;color:blue;font-weight: bold"),width = 6,
                           withSpinner(plotOutput("clustereva_plot1",click = "clustereva_plot1_click")),
                          withSpinner(plotOutput("clustereva_plot2",click = "clustereva_plot2_click"))),
                       box(tags$p("GRÁFICAS MODELO 2", style = "font-size: 115%;color:blue;font-weight: bold"),width = 6,
                           withSpinner(plotOutput("clustereva_plot3",click = "clustereva_plot3_click")),
                          withSpinner(plotOutput("clustereva_plot4",click = "clustereva_plot4_click")))
             ))   
            ),
    #Redes Neuronales
    tabItem(tabName = "redneuronal",
            fluidRow(box(tags$p("RED NEURONAL DE RETROPROPAGACIÓN", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("redneuronal_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("hidenLayers", "Capas Ocultas",
                                                                                                          c("1" = "1",
                                                                                                            "2" = "2"))),
                         br(),
                         conditionalPanel(condition ="input.hidenLayers==1 || input.hidenLayers==2", div(style="display: inline-block;vertical-align:top; width: 300px;", sliderInput("neurLayer1", "Nº de neuronas de la capa 1", 
                                                                                                                                                                                      min = 1, max = 20, value = 1, step= 1))),
                         br(),
                         conditionalPanel(condition ="input.hidenLayers==2", div(style="display: inline-block;vertical-align:top; width: 300px;", sliderInput("neurLayer2", "Nº de neuronas de la capa 2", 
                                                                                                                                                              min = 1, max = 10, value = 1, step= 1))),
                         tags$style(type='text/css', "#redneuronal_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("redneuronal_Action", "Ejecutar",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("redneuronal_msj")),
                    
                    conditionalPanel(condition ="output.salidaOKNN=='TRUE'",
                                     
                     box(tags$p("MODELO DE RED NEURONAL GENERADO (NN)", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         withSpinner(plotOutput("redneural_plot1",click = "redneural_plot1_click")),
                         verbatimTextOutput("redneuronal_msj2")),
                     
                     box(tags$p("EVALUACIÓN DEL MODELO", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                         withSpinner(plotOutput("redneural_plot2",click = "redneural_plot2_click"))),

                     box(tags$p("PREDICCIONES DEL MODELO DE RED NEURONAL", style = "font-size: 120%;color:blue;font-weight: bold"),width = 12,
                        fileInput('neuralFileEvaluation', 'Selecciona CSV para evaluación',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                        tags$style(type='text/css', "#evalCSVNeuronal_Action { width:100%; margin-top: 25px;}"),
                        div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("evalCSVNeuronal_Action", "Predicción",style=blueStyle)),
                        br(),
                        br(),
                        verbatimTextOutput("redneuronal_msj3")
                        
                    )),
                     conditionalPanel(condition ="output.fileNeuraldatacargado",
                                      box(title = "Predicciones - Modelo  de la Red Neuronal (NN)", width = 12, status = "primary",
                                          div(style = 'overflow-x: scroll', tableOutput("tableEvalNeuronal"))
                                      ))
            )),     
       
    #Series temporales
    tabItem(tabName = "arima",
            fluidRow(box(tags$p("ANÁLISIS ARIMA", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("arima_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("tipoPrediccion", "Frecuencia:",
                                                                                                         c("Mensual"="mensual"),
                                                                                                           #"Anual" = "anual"),
                                                                                                         selected="mensual")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#arima_predAction { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("arima_predAction", "Analizar",style=blueStyle)),
                         br(),
                         verbatimTextOutput("arima_msj"),
                         verbatimTextOutput("arima_print1")
                        ),
                         
                         conditionalPanel(condition ="output.salidaOKARIMA=='TRUE'",
                                    box(width=12,
                                          box(tags$p("GRÁFICAS DE DESCOMPOSICIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12, height = "650px",
                                                                               withSpinner(plotOutput("arima_plot1",click = "arima_plot1_click"))),
                         box(tags$p("GRÁFICAS DE DIFERENCIACIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                                                               withSpinner(plotOutput("arima_plot2",click = "arima_plot2_click"))),
                         
                         box(tags$p("GRÁFICAS ACF Y PACF", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                                                               withSpinner(plotOutput("arima_plot3",click = "arima_plot3_click"))),
                         
                         box(tags$p("GENERACIÓN DE MODELOS ARIMA", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                                                               div(style="display: inline-block;vertical-align:top; width: 150px;",h4('No Estacional'), selectInput("modeloNoEsta_p", "p",
                                                                                                                                                               c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                               selected="0"),
                                                                                                                                                  selectInput("modeloNoEsta_d", "d",
                                                                                                                                                               c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                               selected="0"),     
                                                                                                                                                  selectInput("modeloNoEsta_q", "q",
                                                                                                                                                              c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                                selected="0")),
                                                                               div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                                                               div(style="display: inline-block;vertical-align:top; width: 150px;",h4('Estacional'), selectInput("modeloEsta_P", "P",
                                                                                                                                                               c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                                 selected="0"),
                                                                                                                                                   selectInput("modeloEsta_D", "D",
                                                                                                                                                               c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                                 selected="0"),     
                                                                                                                                                   selectInput("modeloEsta_Q", "Q",
                                                                                                                                                               c("0"="0","1"="1","2"="2","3"="3", "4"="4","5"="5","6"="6","7"="7","8"="8","9"="9","10"="10"),
                                                                                                                                                                 selected="0")),
                                                                               br(),
                                                                               tags$style(type='text/css', "#arima_generModelArima { width:100%; margin-top: 25px;}"),
                                                                               div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("arima_generModelArima", "Generar Modelo",style=blueStyle)),
                                                                               br(),
                                                                               br(),
                                                                               verbatimTextOutput("arima_msj2"))
                                                                               
                         )),
                         conditionalPanel(condition ="output.salidaOKARIMA=='TRUE' && input.arima_generModelArima",box(tags$p("MODELO ARIMA GENERADO", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                                                                       withSpinner(plotOutput("arima_plot4",click = "arima_plot4_click")))
                         ),
                         conditionalPanel(condition ="output.salidaOKARIMA=='TRUE' && output.arima_plot4",box(tags$p("PREDICCIONES DEL MODELO ARIMA", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                                                              sliderInput("slider_predarima", "Nº de puntos proyectados", 
                                                                                          min = 1, max = 25, value = 1, step= 1),
                                                                              tags$style(type='text/css', "#arima_predicAction { width:100%; margin-top: 25px;}"),
                                                                              div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("arima_predicAction", "Predicción",style=blueStyle)),
                                                                              withSpinner(plotOutput("arima_plot5",click = "arima_plot5_click")))
                         )
                         
    )),
    
    #Time series con TBATS
    tabItem(tabName = "tbats",
            fluidRow(box(tags$p("PRONÓSTICO TBATS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("tbats_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("tipoPrediccionTbats", "Frecuencia:",
                                                                                                         c("Mensual"="mensual"),
                                                                                                         selected="mensual")),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 50%",sliderInput("slider_predtbats", "Nº de puntos proyectados", 
                                     min = 1, max = 25, value = 1, step= 1)),
                          br(), 
                         tags$style(type='text/css', "#tbats_predAction { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("tbats_predAction", "Predicción",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("tbats_msj"),
                         conditionalPanel(condition ="output.salidaOKTBATS=='TRUE'", withSpinner(plotOutput("tbats_plot1")))))
    ),
    
    #Series temporales
    tabItem(tabName = "agrupaciones",
            fluidRow(box(tags$p("VISUALIZACIONES DE LOS DATOS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("visual_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("visual_at2")),
                         br(),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("visual_factor")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),

                         div(style="display: inline-block;vertical-align:top; width: 400px;",conditionalPanel(condition="output.salidaOKFactorAgrupacion=='TRUE'", sliderInput("visual_grupos", "Nº de Grupos", 
                                                                                                      min = 2, max = 10, value = 1, step= 1))),
                         br(),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("visual_color")),
                         
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),

                         div(style="display: inline-block;vertical-align:top; width: 400px;",conditionalPanel(condition="output.variableColor=='TRUE'",sliderInput("visual_colorIntervalos", "Nº de Intervalos de Densidad", 
                                                                                                                                                                    min = 2, max = 10, value = 1, step= 1))),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),

                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         br()),
                          conditionalPanel(condition ="input.visual_at1!=''",box(tags$p("GRÁFICAS DE AGRUPACIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12, height = "650px",
                                                                                 withSpinner(plotOutput("visual_plot1",click = "visual_plot1_click"))))
            )),
    
    #Geolocalización
    tabItem(tabName = "geolocalizacion",
            fluidRow(box(tags$p("MAPA DE COORDENADAS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         tags$style(type='text/css', "#geo_action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("geo_action", "Geolocalización",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("geo_msj"),
                         br(),
                         conditionalPanel(condition ="input.geo_action",leafletOutput("geo_plot"))))
            ),
    
    #Ruta Optima
    tabItem(tabName = "rutaoptima",
            fluidRow(box(tags$p("RUTA ÓPTIMA", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         tags$style(type='text/css', "#ruta_action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("ruta_action", "Ruta Óptima",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("ruta_msj"),
                     br(),
                     conditionalPanel(condition ="input.ruta_action",leafletOutput("ruta_plot"))))
    ),
    
    #Filtrado Colaborativo - consulta de los datos
    tabItem(tabName = "datosRecomendaciones",
            fluidRow(box(tags$p("DISTRIBUCIÓN DE LAS VALORACIONES", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("datosRecomendaciones_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("datosRecomendaciones_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("datosRecomendaciones_at3")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#datosRecomendaciones_Action { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("datosRecomendaciones_Action", "Consulta",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("datosRecomendaciones_msj")
            ),
            conditionalPanel(condition ="output.salidaOKDispersion=='TRUE'",
                      box(width = 12,
                         splitLayout(cellWidths = c("50%", "50%"), 
                                     withSpinner(plotOutput("datosRecomendaciones_plot1")),
                                     withSpinner(plotOutput("datosRecomendaciones_plot2"))),
                         br(),
                         verbatimTextOutput("dispersionMatriz_msj"),
                         verbatimTextOutput("dispersionMatriz_msj2"))
            ),
            conditionalPanel(condition ="output.salidaOKDispersion=='TRUE'",
                             br(),
                             box(tags$p("REDUCCIÓN DE LA DISPERSIÓN", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                                 conditionalPanel(condition="output.salidaOKSlidersDisp=='TRUE' ",
                                 div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput("RedDisperMatrix")),
                                 div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                 div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput("RedDisperMatrix2"))),
                                 br(),
                                 tags$style(type='text/css', "#RedDisp_Action { width:100%; margin-top: 25px;}"),
                                 div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("RedDisp_Action", "Reducir Dispersión",style=blueStyle)),
                                 div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                 tags$style(type='text/css', "#RedDisp_Guardar { width:100%; margin-top: 25px;}"),
                                 div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("RedDisp_Guardar", "Guardar Cambios",style=blueStyle)),
                                 div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                 tags$style(type='text/css', "#RedDisp_Reset { width:100%; margin-top: 25px;}"),
                                 div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("RedDisp_Reset", "Reset",style=blueStyle)),
                                 br(),
                                 br(),
                                 verbatimTextOutput("dispersionReduccionHecha_msj"))
            ))
    ),        
    #Filtrado Colaborativo - evaluación de modelo
    tabItem(tabName = "modelEval",
            fluidRow(box(tags$p("EVALUACIÓN DE MODELOS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("modelEval_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("modelEval_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("modelEval_at3")),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 400px;",sliderInput("modelEval_sliderEval", "K - Validación Cruzada", 
                                                                                                         min = 2, max = 10, value = 1, step= 1)),
                         br(),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("modelEval_Action", "Evaluación",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("modelEval_msj")
                         ),
                         
                     conditionalPanel(condition ="output.salidaOKEvalRecomen=='TRUE'",
                          box(width = 12,
                         splitLayout(cellWidths = c("50%", "50%"), 
                                     withSpinner(plotOutput("modelEval_plot1")),
                                     withSpinner(plotOutput("modelEval_plot2")))
                          )   
            ))),
    
    #Filtrado Colaborativo - recomendaciones
    tabItem(tabName = "recomendaciones",
            fluidRow(box(tags$p("RECOMENDACIONES COLABORATIVAS", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("colaborativo_at1")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("colaborativo_at2")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("colaborativo_at3")),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("colaborativo_metodo", "Método",
                                                                                                         c("POPULAR"="popular",
                                                                                                           "UBCF" = "ubcf",
                                                                                                           "IBCF" = "ibcf",
                                                                                                           "RANDOM"="random"),
                                                                                                         selected="popular")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("colaborativo_distancia", "Distancia",
                                                                                                         c("Jaccard"="jaccard",
                                                                                                           "Cosine" = "cosine"),
                                                                                                         selected="cosine")),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 400px;",sliderInput("colaborativo_slider", "Nº de Recomendaciones", 
                                                                                                         min = 1, max = 10, value = 1, step= 1)),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("colaborativo_at4")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#colaborativo_Recomen { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("colaborativo_Recomen", "Recomendaciones",style=blueStyle)),
                         br(),
                         verbatimTextOutput("colaborativo_msj"),
                         br(),
                         conditionalPanel(condition ="output.salidaOKrecomendarAction=='TRUE'",
                                          tableOutput("colaborativo_table"))
                         
            ))),
    
    #MongoDB         
    tabItem(tabName = "mongodb",
            fluidRow(box(tags$p("CONEXIONES CON MONGODB", style = "font-size: 115%;color:blue;font-weight: bold"),width = 12,
                         br(),
                         verbatimTextOutput("mongo_msj_archivo"),
                         br(),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("baseDeDatos","Base de Datos:")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;",textInput("coleccion","Colección:")),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 250px;",textInput("url_mongo","URL:", value = "mongodb://localhost:27017")),
                         br(),
                         tags$style(type='text/css', "#Importar_DocFromBBDD { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("Importar_DocFromBBDD", "Importar Documento",style=blueStyle)),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         tags$style(type='text/css', "#Exportar_DocToBBDD { width:100%; margin-top: 25px;}"),
                         div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("Exportar_DocToBBDD", "Exportar Documento",style=blueStyle)),
                         br(),
                         br(),
                         verbatimTextOutput("mongo_msj_action"),
                         br(),
                         tableOutput("mongo_table"),
                         br(),
                         verbatimTextOutput("mongo_msj_table"),
                         br(),
                         tags$style(type='text/css', "#Exportar_DocToBBDD { width:100%; margin-top: 25px;}"),
                         conditionalPanel(condition="output.importadaColeccionDeMongo=='TRUE'",shinySaveButton("guardarImportFromMongo", "Exportar documento a csv.", class="btn btn-info", "Guardar archivo como ...", filetype=list(csv="csv")))
                         ))
            
      )#tabItem
    ) #tabItems
  )
)

