library(dplyr)
library(shinydashboard)
library(shinyFiles)


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

      menuItem("BBDD", tabName = "basesDeDatos", icon = icon("database"),
               collapsible = TRUE,
               menuSubItem("MongoDB", tabName = "mongodb",icon = icon("envira"))
      )
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "carga",
                      fluidRow(
                        box(fileInput('datafile', 'Selecciona CSV',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                            )),
                      fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_carga"))),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado",
                                    box(title = "Datos Cargados", width = NULL, status = "primary",
                                     div(style = 'overflow-x: scroll', tableOutput("filetable"))
                                   )))
              ),
                    
      tabItem(tabName = "consulta",
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
              #verbatimTextOutput("mensajes_limpieza"),
              conditionalPanel(condition ="output.filedatacargado",
                fluidRow(
                  box(title="Limpieza de Datos",width = 12,
                    actionButton("valoresNA", "Buscar valores NA",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                    actionButton("eliminarNA_Limpiar","Eliminar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    actionButton("restaurar_Limpiar","Restaurar Dataset",style="float:right;color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("mensajes_limpieza"))),
                fluidRow(box(title="Resultados",width = 12, tableOutput("salidaNA"))),
                fluidRow(box(width = 12,
                             shinySaveButton("guardar_limpieza", "Guardar Cambios", class="shinySave btn-primary","Guardar archivo como ...", filetype=list(csv="csv"))
                             )))
              ),
              
      tabItem(tabName = "edicion",
              verbatimTextOutput("mensajes_edicion"),
              conditionalPanel(condition="output.filedatacargado",
               fluidRow(
                box(title="Operaciones",width = 12,
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
                  
              )
      ),
      fluidRow(box(title = "Datos resultantes", width = NULL, status = "primary",
                   div(style = 'overflow-x: scroll', tableOutput("filetabledicion"))))
      )
      ),
      
      tabItem(tabName = "Mongodb",
              h2("Mongodb tab content")
      )
    )
  )
)
