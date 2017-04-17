library(dplyr)
library(shinydashboard)
library(shinyFiles)


#Definimos los estilos que vamos a usar en el diseño de la aplicación
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
                            #These column selectors are dynamically created when the file is loaded
                            )
                            #box(verbatimTextOutput("strcarga",placeholder = FALSE)),
                      ),
                      fluidRow(conditionalPanel(condition ="output.filedatacargado",
                                    box(title = "Datos Cargados", width = NULL, status = "primary",
                                     div(style = 'overflow-x: scroll', tableOutput("filetable"))
                                   ))
                        #tableOutput("filetable")
                        #tableOutput("geotable")
                      )
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
                            #div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("columna2")),
                            br(),
                            
                            #The action button prevents an action firing before we're ready
                            actionButton("SeleccionarVariables", "Seleccionar Variables",style=blueStyle)
                          ),
                       #box(
                         
                         #br(),
                         #uiOutput("fromCol"),
                         #uiOutput("toCol"),
                         #uiOutput("amountflag"),
                         #The conditional panel is triggered by the preceding checkbox
                         #conditionalPanel(
                         # condition="input.amountflag==true",
                         #  uiOutput("amountCol")
                         #),
                         #The action button prevents an action firing before we're ready
                        # actionButton("Filtro", "Filtro")
                       #),
                      box(title="Estructura del Dataset",verbatimTextOutput("TextoSTR",placeholder = TRUE)
                      ),
                      box(#titlePanel("Guardar archivo"),
                          shinySaveButton("guardarFiltro", "Guardar Filtro", "Guardar archivo como ...", filetype=list(csv="csv"))
                          
                      )),
              fluidRow(tableOutput("filetablecolumnas")))
            ),
      
      tabItem(tabName = "limpieza",
              verbatimTextOutput("controlDeCarga_Limpieza"),
              conditionalPanel(condition ="output.filedatacargado",
                fluidRow(
                  box(title="Limpieza de Datos",width = 12,
                    actionButton("valoresNA", "Buscar valores NA",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                    actionButton("eliminarNA_Limpiar","Eliminar Valores",style="display: inline-block;color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    actionButton("restaurar_Limpiar","Restaurar Dataset",style="float:right;color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                fluidRow(box(title="Resultados",width = 12, tableOutput("salidaNA"))),
                fluidRow(box(title="Mensajes",width = 12,verbatimTextOutput("NAeliminados_Limpiar"))),
                fluidRow(box(width = 12,
                             shinySaveButton("guardarTransformaciones", "Guardar Cambios", class="shinySave btn-primary","Guardar archivo como ...", filetype=list(csv="csv"))
                             )))
              ),
              
      tabItem(tabName = "edicion",
              verbatimTextOutput("controlDeCarga_Edicion"),
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
                  div(style="display: inline-block;vertical-align:top; width: 100px;",textInput("factorNumerico", "Factor Num.:")),
                  div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<p>   / </p>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("otroAtributo")),
                  
                  #The action button prevents an action firing before we're ready
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
