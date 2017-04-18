library(dplyr)
library(shinydashboard)
library(shinyFiles)
library(stringr)


#Definimos los estilos generales que vamos a usar en el diseño de la aplicación
blueStyle="color: #fff; background-color: #337ab7; border-color: #2e6da4"

dashboardPage(
  dashboardHeader(title = "SMART DATA"),
  dashboardSidebar(
    #SideBar con las opciones de la aplicación
    sidebarMenu(
      id = "tabs",
      menuItem("DATOS", tabName = "cargaDatos", icon = icon("sticky-note-o"),
              collapsible = TRUE,
              menuSubItem("Carga", tabName = "carga",icon = icon("upload")),
              menuSubItem("Consultas", tabName = "consulta",icon = icon("book")),
              menuSubItem("Limpieza", tabName = "limpieza",icon = icon("shower")),
              menuSubItem("Edición", tabName = "edicion",icon = icon("edit"))
               ),
     
      menuItem("EXPLORACIONES", tabName = "exploracionDatos", icon = icon("binoculars"),
               collapsible = TRUE,
               menuSubItem("Factorizar", tabName = "factorizar",icon = icon("tag")),
               menuSubItem("Una Variable", tabName = "unaVariable",icon = icon("line-chart"))
               ),
               
      menuItem("BBDD", tabName = "basesDeDatos", icon = icon("database"),
           collapsible = TRUE,
           menuSubItem("MongoDB", tabName = "mongodb",icon = icon("envira")))
      )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "carga",
                      fluidRow(
                        box(fileInput('datafile', 'Selecciona CSV',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                            )),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado",
                                    box(title = "Datos Cargados", width = NULL, status = "primary",
                                     div(style = 'overflow-x: scroll', tableOutput("filetable"))
                                   ))),
                      fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_carga")))
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
                            actionButton("SeleccionarVariables", "Seleccionar Variables",style=blueStyle)
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
                    actionButton("restaurar_Limpiar","Restaurar Dataset",style="float:right;color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
                    br(),
                    div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("valoresErroneos_Limpiar","Buscar",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    #div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("eliminarValoresErroneos_Limpiar","Borrar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    
              )),
                
                fluidRow(box(title="Resultados",width = 12, tableOutput("resultados_limpieza"))),
                fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_limpieza"))),
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
              fluidRow(
                box(title="Factorizar",width = 12,
                    div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributosCambioDeTipos")),
                    br(),
                    div(style="display: inline-block;vertical-align:bottom; width: 150px;",actionButton("ejecutarFactorizacion", "Ejecutar",style=blueStyle))
                )),
              
              fluidRow(box(title = "Datos Resultantes", width = NULL, status = "primary",
                           div(style = 'overflow-x: scroll', verbatimTextOutput("edicion_print"))))
              ),
      
      tabItem(tabName = "unaVariable",
              tags$style(type='text/css', '#controlDeCarga_Exploracion1 {background-color: rgba(0,0,255,0.10); color: blue;}'),
              verbatimTextOutput("controlDeCarga_Exploracion1"),
              conditionalPanel(condition ="output.filedatacargado",
              
              fluidRow(
                box(title="Exploración de Una Variable",width = 12,
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("atributoUnaVariable")),
                  div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("tipoExploracion1", "Tipo de Exploración:",
                                                                                                 c("Sumario" = "sumario",
                                                                                                   "Media" = "media",
                                                                                                   "Desviación St." = "desviacion",
                                                                                                   "Varianza" = "varianza"))),
                  br(),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("Exploraciones_ejecutar1", "Ejecutar",style=blueStyle))
                
                 )),
              fluidRow(box(title="Resultados",width = 12,
                           verbatimTextOutput("resultados_exploracion1",placeholder = TRUE))),
              fluidRow(box(title="Mensajes",width = 12,
                           verbatimTextOutput("mensajes_exploracion1"))),
              fluidRow(box(title="Graficas",width = 12,
                           plotOutput("explor1_grafica1",click = "plot_click1"),
                           plotOutput("explor1_grafica2",click = "plot_click2"))
                       
                       )
              
    )),
                
      #MongoDB         
      tabItem(tabName = "Mongodb",
              h2("Mongodb tab content")
      )
    )
  )
)
