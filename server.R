shinyServer(function(input, output, session) {
  
  #Se fija el tamaño max de los archivos importados a 100Mb
  options(shiny.maxRequestSize=100*1024^2)
  
  #inicializamos variables de control de carga de archivos
  filesalida<-NULL
  fileCSV<-FALSE
  fileAPI<-FALSE
  
  #-----El menú lateral de la aplicación se genera dinámicamente en función de si existen o no datos cargados.----------
  
  observe({
    
  #Variables que se observan
  ficherocargando<-input$datafile
  ficherocargandoAPI<-input$API_Action
  
  #Renderizamos el menú una vez se hayan cargado datos
  output$menu <- renderMenu({
    if (is.null(filesalida) && fileAPI==FALSE) {
      sidebarMenu(
                  menuItem("CARGA DE DATOS", tabName = "importacionDatos",icon = icon("upload")
                           
                  ),
                  menuItem("BBDD", tabName = "basesDeDatos", icon = icon("database"),
                                          collapsible = TRUE,
                                         menuSubItem("MongoDB", tabName = "mongodb",icon = icon("envira")))
      )
    }
    else{
      sidebarMenu(
                  menuItem("CARGA DE DATOS", tabName = "importacionDatos",icon = icon("upload")
                           
                  ),
                  menuItem("OPERACIONES", tabName = "operaciones", icon = icon("sticky-note-o"),
                           collapsible = TRUE,
                           menuSubItem("Consultas", tabName = "consulta",icon = icon("book")),
                           menuSubItem("Edición", tabName = "edicion",icon = icon("edit")),
                           menuSubItem("Limpieza", tabName = "limpieza",icon = icon("shower"))
                  ),
                  
                  menuItem("EXPLORACIONES", tabName = "exploracionDatos", icon = icon("binoculars"),
                           collapsible = TRUE,
                           menuSubItem("Factorizar", tabName = "factorizar",icon = icon("tag")),
                           menuSubItem("Una Variable", tabName = "unaVariable",icon = icon("bolt")),
                           menuItem("Dos Variables", tabName = "dosVariables",icon = icon("balance-scale"),collapsible = TRUE,
                                    menuSubItem("Exp. Gráfica", tabName = "expGrafica",icon = icon("sun-o")),
                                    menuSubItem("Correlaciones", tabName = "correlacionesdosvar",icon = icon("handshake-o"))
                           ),
                           menuItem("Multivariable", tabName = "multivariable",icon = icon("star-o"),collapsible = TRUE,
                                    menuSubItem("Exp. Gráfica", tabName = "multi_expGrafica",icon = icon("sun-o")),
                                    menuSubItem("Correlaciones", tabName = "multi_cor",icon = icon("handshake-o"))
                           )),
                  
                  menuItem("REGRESIONES LINEALES", tabName = "regresionlineal", icon = icon("expand"),
                           collapsible = TRUE,
                           menuSubItem("RL. Simple", tabName = "reglineal_simple",icon = icon("line-chart")),
                           menuSubItem("RL. Múltiple", tabName = "reglineal_multi",icon = icon("bar-chart"))
                  ),
                  
                  menuItem("CLUSTERING", tabName = "clusters",icon = icon("cubes"),
                           collapsible = TRUE,
                           menuSubItem("K-means", tabName = "kmeans",icon = icon("braille")),
                           menuSubItem("Jerarquía", tabName = "jerarquia",icon = icon("sitemap")),
                           menuSubItem("Evaluaciónes", tabName = "evaluaciones",icon = icon("spinner"))
                  ),
                  
                  menuItem("REDES NEURONALES", tabName = "redneuronal", icon = icon("snowflake-o")
                  ),
                  
                  menuItem("FILTRADO COLABORATIVO", tabName = "colaborativo",icon = icon("handshake-o"),
                           collapsible = TRUE,
                           menuSubItem("Dispersión de Datos", tabName = "datosRecomendaciones",icon = icon("shopping-basket")),
                           menuSubItem("Evaluación de Modelos", tabName = "modelEval",icon = icon("shopping-cart")),
                           menuSubItem("Recomendaciones", tabName = "recomendaciones",icon = icon("thumbs-o-up"))
                  ),
                  
                  menuItem("SERIES TEMPORALES", tabName = "s_temporales", icon = icon("area-chart"),
                           collapsible = TRUE,
                           menuSubItem("ARIMA", tabName = "arima",icon = icon("hourglass-2")),
                           menuSubItem("TBATS", tabName = "tbats",icon = icon("hourglass"))
                  ),
                  
                  menuItem("VISUALIZACIONES", tabName = "visualizaciones", icon = icon("eye"),
                           collapsible = TRUE,
                           menuSubItem("Agrupaciones", tabName = "agrupaciones",icon = icon("object-group")),
                           menuSubItem("Geolocalización", tabName = "geolocalizacion",icon = icon("globe")),
                           menuSubItem("RutaOptima", tabName = "rutaoptima",icon = icon("motorcycle"))
                  ),
                  menuItem("BBDD", tabName = "basesDeDatos", icon = icon("database"),
                                         collapsible = TRUE,
                                          menuSubItem("MongoDB", tabName = "mongodb",icon = icon("envira")))
       )
    
     }
   })
  })
  
  #--------------------------------Fin de la generación del menú dinámico---------------------
  
#Función que devuelve los datos cargados mediante la API-Web
api<-function(){
  
  #Comprobamos que haya una URL insertada
  if (input$URL!=""){
      
      #Obtenemos los datos base
      raw.result <<- GET(url = input$URL)
      
      #Por si aún no se ha introducido ningún archivo en el editable
      if (is.null(raw.result)) {
        return(NULL)
      }
      
      #Formateo de datos para ser mostrados por pantalla
      this.raw.content <- rawToChar(raw.result$content)
      this.content <- fromJSON(this.raw.content)
      this.content.df <- do.call(what = "rbind",
                                 args = lapply(this.content, as.data.frame))
      
      #Se devuelve el archivo de datos obtenido
      fileout<-this.content.df
      
  }else{
    
    #No hay URL insertada para buscar
    return(NULL)
  }
}



#Cargamos el archivo CSV
  observe({
    
    #Leemos el tipo de importación
    tipo<-input$tipoImport
    
    #Si es de tipo csv
    if(input$tipoImport=="csv"){
      
      infile <- input$datafile 
      
      #Reiniciamos el mensaje del sistema
      output$API_msj <- renderText({invisible()})
      
      if(is.null(infile)){ return(NULL)}
      
      #Recogemos el valor de la configuración de importación para variables factor
      requiereFactores <- switch(isolate(input$importFactor),
                                 trueFactor = TRUE,
                                 falseFactor = FALSE)
      
      #Recogemos el tipo de separador escogido para importar archivos cvs           
      delimitadorCSVescogido <- switch(isolate(input$delimitadorCSV),
                                csvComa = ",",
                                 csvPuntoyComa = ";")
      
      #Leemos el archivo y tratamos los '?' como NA
      file<-read.csv(infile$datapath,na.strings=c("?",""),stringsAsFactors = requiereFactores, sep=delimitadorCSVescogido)
      
      if (ncol(file)==1){
        #Mostramos los datos por pantalla
        output$API_msj <- renderText({
          "Es posible que haya un error en la configuración de importación. Revisar el delimitador escogido."
        })
        
      }else{
        #Mostramos los datos por pantalla
        output$API_msj <- renderText({
          "Se muestran los 100 primeros registros del archivo CSV."
        })
        
      }
      
    
      
      #Se visualiza el contenido del archivo
      output$filetable <- renderTable({
        if(nrow(file)>100){
          fileReduced<-file[1:100,]
        }else{
          file
        }
      })
      
      #Sobreescribimos las varibales de control de carga
      fileCSV<<-TRUE
      fileAPI<<-FALSE
      
      #Exportamos globalmente el fichero cargado
      filesalida<<-file

    }
  })
  
  observeEvent(input$API_Action,{
    
    #Control del error en obtención de datos mediante API
    out<-tryCatch(
      
    {
    
    #Llamada a la función que carga los datos desde la API-Web 
    fileAPIcargado<-api()
    
    #Si la función anterior devuelve NULL
    if (is.null(fileAPIcargado)){
      
      #Mensaje de error
      output$API_msj<-renderText({
        print("Error: No se ha especificado ninguna ruta.")
        
      })
      
      return(NULL)
    }
    
    #Si es status code de la llamada a la API es distinto de no encontrado 404
    if (raw.result$status_code!=404){
      
      #Guardamos el número de filas del archivo importado
      nmax<-nrow(fileAPIcargado)
      
      #Para tener en cuenta las filas que mostramos, y no relentizar el sistema en datasets > 100 filas
      if(nmax>100 || nmax==100){
        
        #Mensaje OK
        output$API_msj <- renderText({print("Se muestran los primeros 100 registros obtenidos con la API")})
        
        #Se visualiza el contenido del archivo
        output$filetable <- renderTable({fileReduced<-fileAPIcargado[1:100,]})
        
      }else{
        #Mensaje OK
        output$API_msj <- renderText({print(paste("Se muestran los primeros ",nmax, " registros obtenidos con la API", sep=""))})
        
        #Se visualiza el contenido del archivo
        output$filetable <- renderTable({fileReduced<-fileAPIcargado[1:nmax,]})
      }
    }else{
      
      #Mensaje ERROR 404
      output$API_msj <- renderText({
        print("Error: Status Code 404")
      })
      
      #Se reinicia el contenido de la tabla
      output$filetable <- renderTable({})
    }
    
    #Sobreescribimos glas vriables globales de control de carga
    fileCSV<<-FALSE
    fileAPI<<-TRUE
    filesalida<<-fileAPIcargado
    
    },
    #En caso de warning mostramos el log del sistema
    warning = function(w) {
      
      output$API_msj<-renderText({
        print(paste("Warning: ", log(input)))
      })
    },
    #En caso de error mostramos el log del sistema
    error = function(e) {
      
      output$API_msj<-renderText({ 
        print("Error: No existe conexión mediante la URL especificada.")
      })
    }
    
    )
    
    #Se devuelve la salida de Try-Catch
    return(out)
    
    
    
  })  
  


  #En caso de que haya que seleccionar algún nodo JSON de la información devuelta por la API-Web
  #Mostramos los nodos encontrados en el fichero (xml)
  output$nodo <- renderUI({
      df <-filedata()
      if(fileAPI==TRUE){
      if (is.null(df)) return(NULL)
      items=rownames(df)
      selectInput("Nodo", "Nodo:",choices=c("",items))
      }
  })
  
 #Definimos las funciones que borraran los elementos gráficos desplegados, al detectar una nueva carga de datos
  Funcion_BorraPlots<-function(){
    output$graf_correla_dosVariables<- renderPlot({})
    output$multivar_graf_plot<- renderPlot({})
    output$multivar_graf_correla<- renderPlot({})
    output$reglienalsimple_plot1 <- renderPlot({})
    output$reglienalsimple_plot2 <- renderPlot({})
    output$reglienalsimple_plot3 <- renderPlot({})
    output$reglienalsimple_plot4 <- renderPlot({})
    output$reglienalmulti_plot1 <- renderPlot({})
    output$reglienalmulti_plot2 <- renderPlot({})
    output$reglienalmulti_plot3 <- renderPlot({})
    output$redneural_plot1 <- renderPlot({})
    output$redneural_plot2 <- renderPlot({})
    output$cluster_plot1 <- renderPlot({})
    output$cluster_plot2 <- renderPlot({})
    output$clusterj_plot1 <- renderPlot({})
    output$clusterj_plotFinal <- renderPlot({})
    output$clusterelbow_plot1 <- renderPlot({})
    output$clustereva_plot1 <- renderPlot({})
    output$clustereva_plot2 <- renderPlot({})
    output$clustereva_plot3 <- renderPlot({})
    output$clustereva_plot4 <- renderPlot({})
    output$arima_plot1 <- renderPlot({})
    output$arima_plot2 <- renderPlot({})
    output$arima_plot3 <- renderPlot({})
    output$arima_plot4 <- renderPlot({})
    output$arima_plot5 <- renderPlot({})
    output$tbats_plot1 <- renderPlot({})
    output$datosRecomendaciones_plot1 <- renderPlot({})
    output$datosRecomendaciones_plot2 <- renderPlot({})
    output$modelEval_plot1 <- renderPlot({})
    output$modelEval_plot2 <- renderPlot({})
    
  }
  
  #Funciones que reinician los Print cada vez que se carga un nuevo archivo
  Funcion_BorraPrints<-function(){
    
    output$consulta_msj <- renderPrint({invisible()})
    output$edicion_msj <- renderPrint({invisible()})
    output$mensajes_limpiezaNA <- renderPrint({invisible()})
    output$mensajes_limpiezaAnomalos <- renderPrint({invisible()})
    output$dosvariables_mensajes_factorizar <- renderPrint({invisible()})
    output$dosvariables_mensajes_print <- renderPrint({invisible()})
    output$mensajes_exploracion1 <- renderPrint({invisible()})
    output$resultados_exploracion1 <- renderPrint({invisible()})
    output$mensajes_exploracionGrafica <- renderPrint({invisible()})
    output$mensajes_exploracionGrafica2 <- renderPrint({invisible()})
    output$dosvar_Print_relaTab <- renderPrint({invisible()})
    output$mensajes_dosvar_exploracionGrafica <- renderPrint({invisible()})
    output$dosvar_msj_correlacion <- renderPrint({invisible()})
    output$dosvar_Print_correlacion <- renderPrint({invisible()})
    output$multivar_msj_graf <- renderPrint({invisible()})
    output$multivar_msj_correlacion <- renderPrint({invisible()})
    output$multivar_print_correlacion <- renderPrint({invisible()})
    output$reglienalsimple_msj <- renderPrint({invisible()})
    output$reglienalmulti_MSE <- renderPrint({invisible()})
    output$reglienalsimple_print <- renderPrint({invisible()})
    output$SLR_prediccion_print <- renderPrint({invisible()})
    output$reglienalmulti_msj <- renderPrint({invisible()})
    output$reglienalmulti_print <- renderPrint({invisible()})
    output$redneuronal_msj <- renderPrint({invisible()})
    output$redneuronal_msj2 <- renderPrint({invisible()})
    output$cluster_msj <- renderPrint({invisible()})
    output$cluster_print1 <- renderPrint({invisible()})
    output$cluster_print2 <- renderPrint({invisible()})
    output$clusterj_msj <- renderPrint({invisible()})
    output$clusterj_print <- renderPrint({invisible()})
    output$clusterj_print1 <- renderPrint({invisible()})
    output$clusterj_print2 <- renderPrint({invisible()})
    output$clustereva_msj <- renderPrint({invisible()})
    output$arima_msj <- renderPrint({invisible()})
    output$arima_print1 <- renderPrint({invisible()})
    output$arima_msj2 <- renderPrint({invisible()})
    output$tbats_msj <- renderPrint({invisible()})
    output$geo_msj <- renderPrint({invisible()})
    output$ruta_msj <- renderPrint({invisible()})
    output$datosRecomendaciones_msj <- renderPrint({invisible()})
    output$dispersionReduccionHecha_msj <- renderPrint({invisible()})
    output$modelEval_msj <- renderPrint({invisible()})
    output$colaborativo_msj <- renderPrint({invisible()})
    output$mongo_msj_action <- renderPrint({invisible()})
    output$mongo_msj_table <- renderPrint({invisible()})
    
  }
  
  #Funciones que reinician las tablas cada vez que se carga un nuevo archivo
  Funcion_BorraTablas<-function(){
    input$SeleccionarVariables==FALSE
    output$filetablecolumnas <- renderTable({})
    output$filetabledicion <- renderTable({})
    output$resultados_limpiezaNA <- renderTable({})
    output$resultados_limpiezaAnomalos <- renderTable({})
    output$regreMultiEvaluation <- renderTable({})
    output$tableEvalNeuronal <- renderTable({})
    output$colaborativo_table <- renderTable({})
    output$mongo_table <- renderTable({})
    
  }
  
#Ocultamos paneles cada vez que se realiza una nueva carga de datos
 Funcion_OcultarPaneles<-function(){
   
   output$salidaOkMostrarVentanasSLR <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOkMostrarVentanasMLR <- reactive({valorDevueltoMLR<-"FALSE"})
   output$salidaOKNN <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKClusters <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKClustersJe <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKClustersJeCorte <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKClustersEva <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKDispersion <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKARIMA <- reactive({valorDevuelto<-"FALSE"})
   output$salidaOKTBATS <- reactive({valorDevuelto<-"FALSE"})
   
 }
 
 #Reset de todas las variables generadas globalmente
 Function_ResetVariablesGlobales<-function(){
   
   rm(  )
   
 }
    
  #Definimos la función que es llamada por todas las acciones del script y devuelve el dataset cargado
  filedata<-reactive({
    #Variables trigger
    cargaAPI<-input$API_Action
    cargaCSV<-input$datafile
    obtenerNodoAPI<-input$API_nodeAction
    tipoImport<-input$tipoImport
    
    #Llamamos a la función que borra los plots, prints,tablas y paneles, al tratarse de una nueva carga
    Funcion_BorraPlots()
    Funcion_BorraPrints()
    Funcion_BorraTablas()
    Funcion_OcultarPaneles()
    
    #Reiniciamos en caso de factorización previa
    ficheroFactorizado<<-"False"
    
    
    #Recogemos el fichero cargado
    file<-filesalida
    if(is.null(file)){return(NULL)}
    
    #Para hacer reset se los combos en caso de factorización
    fileout_fact_dosVar<<-file
    fileout<-file
    
  })
  
  #Guardamos una salida out para consulta cada vez que se llama a la función filedata()
  output$filedatacargado <- reactive({
    filedata()
  })
  
  #Devolvemos la condición de que el fichero se ha cargado a la variable para consultar desde ui.R <conditionalPanel>
  outputOptions(output, "filedatacargado", suspendWhenHidden = FALSE) 
  
  #Función de devuelve el nodo seleccionado
  funcion_seleccionarNodoAPI<-function(){
    if(input$requiereNodo=="si"){
      #Le pasamos el fichero filesalida vigente
      df<-filesalida
      if (is.null(df)) return(NULL)
      #Devolvemos el fichero filesalida, pero únicamente el nodo seleccionado
      filesalida<<- fromJSON(as.vector(df[input$Nodo,]))[[1]]
    }
    
  }

  #Evento que ejecuta la funcion anterior
  observeEvent(input$API_nodeAction,{
    
    if(fileAPI==TRUE){
      
    fileAPIcarganode<-funcion_seleccionarNodoAPI()
    
    if (raw.result$status_code!=404){
      
      nmax<-nrow(fileAPIcarganode)
      
      #Mostramos sólo los 100 primeros registros para no tener problemas de renderización en las tablas en caso de datos masivos.
      if(nmax>100 || nmax==100){
        
        #Se visualiza el contenido del archivo
        output$API_msj <- renderText({print(paste("Se muestran los primeros 100 registros obtenidos con la API. Nodo ",input$Nodo,sep=""))})
        
        #Se visualiza el contenido del archivo
        output$filetable <- renderTable({fileReduced<-fileAPIcarganode[1:100,]})
        
        
        #Para que no se reinicie el combo de Nodos
        fileAPI<<-FALSE
        
      }else{
        #Se visualiza el contenido del archivo
        output$API_msj <- renderText({print(paste("Se muestran los primeros ",nmax, " registros obtenidos con la API. Nodo ",input$Nodo, sep=""))})
        
        #Se visualiza el contenido del archivo
        output$filetable <- renderTable({fileReduced<-fileAPIcarganode[1:nmax,]})
        
        #Para que no se reinicie el combo de Nodos
        fileAPI<<-FALSE
      }
    }else{
      #Se visualiza el contenido del archivo
      output$API_msj <- renderText({
        print("Error: Status Code 404.")
      })
      
      #Reseteamos la tabla
      output$filetable <- renderTable({})
    }
    }else{
      #Se estáintentando ejecutar sobre un CSV, y debe ser sobre la API
      output$API_msj <- renderText({
        print("Error: No se puede ejecutar la operación sobre un dataset final. Debe cargar previamente un fichero con nodos XML desde la opción de API correspondiente.")
      })
    }
  
  })


  #------------------CONSULTA DE DATOS--------------------------
  
  
  #Los combos los atributos deben actualizarse de acuerdo a los checkbox seleccionados en la consulta
  #Combo Atributo 1
  observeEvent(input$variablesLista,{
    file<- filedata()
    file.subset <- file[, input$variablesLista]
    
    output$var1 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("Variable1", "Atributo 1:",items)
      
    })
    
    #Combo valor Atributo1
    output$val1 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      fr=input$Variable1
      items=df[,fr]
      
      #Si es de tipo factor hay que mostrar los levels
      if (class(items)!="factor"){
        selectInput("Valor1", "Valor:",choices=c("",items))
      }else{
        selectInput("Valor1", "Valor:",choices=c("",levels(df[,fr])))
      }
      
      
    })
    
    #Combo Atributo2
    output$var2 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("Variable2", "Atributo 2:",items)
      
    })
    
    #Combo valores Atributo 2
    output$val2 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      fr=input$Variable2
      items=df[,fr]
      
      #Si es de tipo factor hay que mostrar los levels
      if (class(items)!="factor"){
        selectInput("Valor2", "Valor:",choices=c("",items))
      }else{
        selectInput("Valor2", "Valor:",choices=c("",levels(df[,fr])))
      }
      
    })
    
  })
  
  # #Cuadro sumario de la estructura del dataset
  # output$str <- renderPrint({
  #  df <-filedata()
  #   if (is.null(df)) return(NULL)
  # 
  #   str(df)
  # })
  # 
  
 
  #checkboxGroupInput de la consulta
  output$variables <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    checkboxGroupInput("variablesLista", "Atributos del Dataset:",items,selected=items, inline = TRUE)
    
  })
  
  #Cuadro sumario de la estructura del dataset
  output$TextoSTR <- renderPrint({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    str(df)
  })
  
  #Función que filtra el dataset de acuerdo al contenido de los checkbox
  FuncionFiltroColumnas <-eventReactive(input$SeleccionarVariables,{
    file=filedata()
    if (is.null(file)) return(NULL)
    
    #Fichero filtrado por los checkbox
    file.subset <- file[, input$variablesLista]
    
    #Recogemos los valores de los atributos siempre que se haya seleccionado más de un checkbox
   if (length(input$variablesLista)>1){
     var1=input$Variable1
     val1=input$Valor1
     var2=input$Variable2
     val2=input$Valor2
    #Filtramos el contenido de acuerdo a los atributos y valores
     if(val1!="" && val2=="" ){
       fileout <- file.subset[file.subset[,var1]==val1,]
     }else if(val2!="" && val1!="" ){
       fileout_pre<-file.subset[file.subset[,var1]==val1,]
       fileout<- fileout_pre[fileout_pre[,var2]==val2,]
     }else{
       fileout<-file.subset
     }
     
     
   }else{
     
     #Si se ha seleccionado sólo un check devuelve NULL
     return(NULL)
   }
   
    
  })
  
  observeEvent(input$SeleccionarVariables, {
    
    filtroCol<-FuncionFiltroColumnas()
    
    #Se deben al menos seleccionar 2 checks
    if(is.null(filtroCol)){
      output$consulta_msj <- renderText({
        print("Se debe seleccionar más de un atributo para consultar.")
      })
      
      #Reiniciamos la tabla
      output$filetablecolumnas <- renderTable({})
      
    }else{
      
      #Mostramos los 100 primeros valores de la consulta para evitar porblemas de renderización
      if (nrow(filtroCol)>100){
        output$filetablecolumnas <- renderTable({
          filtroCol[1:100,]
        })
        
        output$consulta_msj <- renderText({
          print("Se muestran los 100 primeros resultados de la consulta.")
        })
      
      }else{
        
        output$filetablecolumnas <- renderTable({
          filtroCol
        })
        
        output$consulta_msj <- renderText({
          print("Se muestran los resultados de la consulta.")
        })
        
      }
    }
    
  })
  
  #Sistema de guardado de cambios
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardarFiltro", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardarFiltro)
    data <- FuncionFiltroColumnas()
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath),row.names=FALSE)
    }
  })

  #------------------MENÚ DE LIMPIEZA DE DATOS--------------------------
  
  #Inicializamos las varibales globales que indican segunda vuelta de las funciones
  eliminadosNA<-"False"
  segundaeliminacion<<-"False"
  
  #Función que devuelve los alores NA encontrados en el dataset
  Funcion_valoresNA<-eventReactive(input$valoresNA,{
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    output$mensajes_limpiezaNA <- renderText({
      print("Se muestran el número de registros NA en el dataset.")
    })
    
    if (eliminadosNA=="False") {
    fileout<-table(is.na(df)) 
    }else{
      fileout<-table(is.na(Funcion_eliminarValoresNA()))
    }
  })
  #Evento que ejecuta la función anterior
  observeEvent(input$valoresNA, {
    output$resultados_limpiezaNA <- renderTable({
      Funcion_valoresNA()
    })
  })
  
  #Función que elimina los valores NA
  Funcion_eliminarValoresNA<-eventReactive(input$eliminarNA_Limpiar,{
    df <-filedata()
    if (is.null(df)) return(NULL)
    #Cambia el valor de la varibale para saber que ya se ha ejecutado
    eliminadosNA<<-"True"
    fileout<-na.omit(df)
  })
  
  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$eliminarNA_Limpiar, {
    Funcion_eliminarValoresNA()
    output$mensajes_limpiezaNA <- renderText({
      if (segundaeliminacion=="False"){
        segundaeliminacion<<-"True"
        print("Registros con valores NA eliminados.")
      }else{
        print("Los registros con valores NA habian sido previamente eliminados.")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$resultados_limpiezaNA <- renderTable({})
  })
  
  
  
  #Función que restaura los valores NA eliminados previamente
  Funcion_restauraValoresNA<-eventReactive(input$restaurar_Limpiar,{
    df <-filedata()
    if (is.null(df)) return(NULL)
      segundaeliminacion<<-"False"
      fileout<-df
  })
  
  
  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$restaurar_Limpiar, {
    Funcion_restauraValoresNA()
    output$mensajes_limpiezaNA <- renderText({
      if (eliminadosNA=="False"){
        print("Aún no se ha realizado ninguna operación sobre el dataset.")
      }else{
        #Restauramos para que se pueda volver a eliminar
        eliminadosNA<<-"False"
        print("Se ha restaurado el dataset original.")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$resultados_limpiezaNA <- renderTable({})
  })
  
  
  #---Pasamos a la búsqueda de valores anómalos---------
  #Iniciamos variables de control
  buscadosvaloreserror<-"False"
  borradosvaloreserror<<-"False"

  #Combo atributos
  output$atributosLimpieza <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)

    items=names(df)
    selectInput("atributosLimpieza", "Atributo a analizar:",items)

  })

  #Función que busca valores anomalos
  Funcion_buscaValoresErroneos<-eventReactive(input$valoresAnomalos_Buscar,{
    
    #Comprobamso si lo hacemos a partir del fichero con los valores NA eliminado previamente o no
    if (eliminadosNA=="True"){
      df<-Funcion_eliminarValoresNA()
    }else{
      df <-filedata()
    }
    
    if (is.null(df)) return(NULL)
    atributo<-input$atributosLimpieza
    
    #Verificamos el tipo de búsqueda de valores anómalos
    if(input$tipoDato_Limpieza=="string"){
      subset <- str_subset(as.vector(df[,atributo]), "[a-z A-Z]")
    }else{
      subset <- str_subset(as.vector(df[,atributo]), "[0-9]")
    }
    
    location <- str_detect(as.vector(df[,atributo]), subset)
    buscadosvaloreserror<<-"True"
    
    #Se devuelven los valores anómalos encontrados a la llamada principal
    fileout<-df[location, ]

  })

  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$valoresAnomalos_Buscar, {
    
    #Evento que dibuja el resultado de la búsqueda anterior
    output$resultados_limpiezaAnomalos <- renderTable({Funcion_buscaValoresErroneos()})
    output$mensajes_limpiezaAnomalos <- renderText({

        print("Ejecutada la búsqueda de valores anómalos.")

    })

  })
  
  
  #Control de eliminaciones de registros anómalos
  eliminadosValoresAnomalos<-"False"
  segundaeliminacionAnomala<-"False"
  
  #Función que elimina los valores Anómalos
  Funcion_eliminarValoresAnomalos<-eventReactive(input$valoresAnomalos_Limpiar,{
    #Comprobamos si se han eliminado los valores anómalos
    if (eliminadosNA=="True"){
      df<-Funcion_eliminarValoresNA()
    }else {
      df <-filedata()
    }
    if (is.null(df)) return(NULL)
    atributo<-input$atributosLimpieza
    if(input$tipoDato_Limpieza=="string"){
      subset <- str_subset(as.vector(df[,atributo]), "[a-z A-Z]")
    }else{
      subset <- str_subset(as.vector(df[,atributo]), "[0-9]")
    }
    location <- str_detect(as.vector(df[,atributo]), subset)
    eliminadosValoresAnomalos<<-"True"
    fileout<-subset(df,location=="FALSE")
    
  })
  
  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$valoresAnomalos_Limpiar, {
    
    Funcion_eliminarValoresAnomalos()
    output$mensajes_limpiezaAnomalos <- renderText({
      
        print("Registros eliminados. Para actualizar el dataset debe guardar los cambios.")
      
    })
    
    #Evento que reinicia los resultados de la tabla
    output$resultados_limpiezaAnomalos <- renderTable({})
  })
  
  
  
  #Botón de guardado
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardar_limpieza", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardar_limpieza)
    if (eliminadosValoresAnomalos=="True"){
        data<-Funcion_eliminarValoresAnomalos()
    }else if(eliminadosNA=="True"){
        data<-Funcion_eliminarValoresNA()
    }else{
      data<-filedata()
    }
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath),row.names=FALSE)
    }
  })

  
  #------------------MENU DE EDICION DE DATOS--------------------------
  
  #Carga de Atributos en combos
  output$atributosEdicion <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("atributosEdicion", "Atributo Origen:",items)
    
  })
  
  #Carga de Atributos en combos
  output$otroAtributo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("otroAtributo", "Otro Atributo:",items)
    
  })
  
  #Función que realiza las operaciones aritméticas de entre atributos o con factores en el menú de edición
  FuncionAddAtributo <-eventReactive(input$ejecutarAtributo,{
    df=filedata()
    if (is.null(df)) return(NULL)
    #Definimos el tipo de operación
    switch(input$operacion, 
           sum={
             oper<-"+"
           },
           rest={
             oper<-"-"
           },
           mul={
             oper<-"*"    
           },
           div={
             oper<-"/"  
           }

          )
    
    #En el caso de que la operación sea mediante un factor numérico
    if (input$edicion_tipoDato=="Factor") {
      
      if (class(df[,input$atributosEdicion]) == "numeric" && input$factorNumerico!="" &&  !is.na(as.numeric(input$factorNumerico))){
        
        
        #Añadimos la nueva columna con el cálculo
        fileout<-mutate_(df, .dots=setNames(paste0(input$atributosEdicion,oper,input$factorNumerico), input$nuevoAtributo))
        
        if(nrow(fileout)<100){
          
          output$edicion_msj<-renderText({
            print("Se muestra el resultado del fichero editado.")
          })
          
          fileout
          
        }else{
          
          output$edicion_msj<-renderText({
            print("Se muestran los 100 primeros registros del resultado del fichero editado.")
          })
          
          fileout[1:100,]
        }
      
      }else{
        
        output$edicion_msj<-renderText({
          print("Error: El atributo origen no es numérico, o bien no se introducido un factor aritmético correcto.")
        })
        return(NULL)
    }
    
      #Si la operación es entre atributos del dataset
    }else if (class(df[,input$atributosEdicion]) == "numeric"  && input$otroAtributo!="") {
      if (class(df[,input$otroAtributo])=="numeric") {
        fileout<-mutate_(df, .dots=setNames(paste0(input$atributosEdicion,oper,input$otroAtributo), input$nuevoAtributo))
     
              if(nrow(fileout)<100){
                
                output$edicion_msj<-renderText({
                  print("Se muestra el resultado del fichero editado.")
                })
                
                fileout
                
              }else{
                
                output$edicion_msj<-renderText({
                  print("Se muestran los 100 primeros registros del resultado del fichero editado.")
                })
                
                fileout[1:100,]
              }
        
         }else{
           
             output$edicion_msj<-renderText({
               print("Error: El segundo atributo no es numérico.")
             })
             return(NULL)
      #fileout<-paste0("El atributo <",input$otroAtributo,">, no es numérico")
         }
    }else{
      
      output$edicion_msj<-renderText({
        print("Error: El atributo origen no es numérico.")
      })
      return(NULL)
    }

      })
  
  #Llamada a la función de operaciones aritméticas anterior
  observeEvent(input$ejecutarAtributo, {

    #Generamos la nueva tabla de datos
    tabla<-FuncionAddAtributo()
    
    #Pintamos la tabla
    output$filetabledicion <- renderTable({
      tabla
    })

  })
  
  #Botón de guardado de cambios
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardar_edicion", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardar_edicion)
    data <- FuncionAddAtributo()
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath),row.names=FALSE)
    }
  })

  
####-------EXPLORACIONES---------#########
  
  ####-------Factorización---------#########
  #Variables de control de factorización de atributos
  ficheroFactorizado<-"False"
  
  #Evento que muestra las gráficas sin esperar un ActionButton
  observe({ 
    
    tipo<-input$datafile
    APIAction<-input$API_Action
    
    #Mostramos el sumario general del dataset
    output$dosvariables_sumarioGeneral <- renderPrint({
      file<-filedata()
      summary(file)
    })
  })
  
  
  #Renderizamos los atributos en 'dosvariables_Ui_atributos'
  output$dosvariables_Ui_atributos <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("dosvariables_Ui_atributos", "Atributo:",items)
    
  })
  

  #Para dibujar en UI.R el cuadro de intervalos en caso de que sea una variable numérica
    output$intervalos<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      atributo<-input$dosvariables_Ui_atributos
      if(is.numeric(df[,atributo])){interv<-"TRUE"}else{inter<-"FALSE"}
    })
  
    #Devolvemos el valor de la variable para mostrar o no el combo
    outputOptions(output, 'intervalos', suspendWhenHidden = FALSE)
    
  
  
  #Función que factoriza un atributo en un número de intervalos
  Funcion_FactorizarConDosVariables<-eventReactive(input$dosvariables_Action_factorizar,{
    if (ficheroFactorizado=="True"){
      df<-fileout_fact_dosVar
    }else{
      df <-filedata()
      if (is.null(df)) return(NULL)
    }
    
    
    atributo<-input$dosvariables_Ui_atributos
    
    if (is.numeric(df[,atributo]) && !is.na(as.numeric(input$dosvariables_TextInput_intervalos))){
      df[,paste(atributo,"_factor_",input$dosvariables_TextInput_intervalos,sep="")]<-cut(df[,atributo],as.numeric(input$dosvariables_TextInput_intervalos))
      
      #Mostramos el mensaje
        output$dosvariables_mensajes_factorizar <- renderText({
        #Mensaje de ejecución
        print(paste("Se ha factorizado el atributo <",isolate(input$dosvariables_Ui_atributos),"> en ",input$dosvariables_TextInput_intervalos," intervalos.",sep=""))
        
      })
      
      #Mostramos los nuevos intervalos
      output$dosvariables_mensajes_print <- renderPrint({
        atributo<-isolate(input$dosvariables_Ui_atributos)
        summary(df[,paste(atributo,"_factor_",isolate(input$dosvariables_TextInput_intervalos),sep="")])
      })
      
    }else if (class(df[,atributo])!="factor" && !is.na(as.numeric(input$dosvariables_TextInput_intervalos))){
      #En caso de que el atributo sea de tipo caracter y ya esté factorizado o no previamente.
        #En este caso es que alguna variable es de tipo caracter con lo que hay que usar la estrategia names de columna
        factoresArray<-unique(df[,atributo])
        
        #Se factoriza el atributo
        df[,atributo]<-factor(df[,atributo], levels=sort(factoresArray))
        
        #Escribimos el msj por pantalla
        output$dosvariables_mensajes_factorizar <- renderText({
          #Mensaje de ejecución
          print(paste("Se ha factorizado el atributo:",atributo,",en",intervalos,"intervalos."))
          
        })
        
        #Reiniciamos la salida 
        output$dosvariables_mensajes_print <- renderText({ })
        
      }else if(class(df[,atributo])=="factor"){
        
        #Escribimos el msj por pantalla
        output$dosvariables_mensajes_factorizar <- renderText({
          #Mensaje de ejecución
          print(paste("El atributo <",atributo,"> ya es de tipo factor.",sep=""))
          
        })
          
        #Reiniciamos la salida 
        output$dosvariables_mensajes_print <- renderText({ })
        
        }else{#En caso que el número de intervalos introducido no sea numérico
      
          #Mostramos el mensaje de que el intervalo no es numérico
          output$dosvariables_mensajes_factorizar <- renderText({
            #Mensaje de ejecución
            print("Error: el intervalo introducido no es numérico.")
            
          })
          #Reiniciamos la salida 
          output$dosvariables_mensajes_print <- renderPrint({invisible()})
          
          #No se ejecuta ninguna operación      
          return(NULL)
         }
    
    ficheroFactorizado<<-"True"
    fileout_fact_dosVar<<-df
    
  })
  
  #Ejecutamos la función anterior por evento
  observeEvent(input$dosvariables_Action_factorizar, {
    
    if (ficheroFactorizado=="True"){
      df<-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
    }else{
      df <-filedata()
      if (is.null(df)) return(NULL)
    }
    
        fileout<-Funcion_FactorizarConDosVariables()
        
        if (is.null(fileout)) return(NULL)
        
        if (ficheroFactorizado=="True"){
          
          #Actualizamos el sumario general
          output$dosvariables_sumarioGeneral <- renderPrint({
            summary(fileout)
          })
          
        }else{
          #Mostramos el mensaje de que el atributo no es numérico
          output$dosvariables_mensajes_factorizar <- renderText({
            #Mensaje de ejecución
            print(paste("Error: el atributo ",isolate(input$dosvariables_Ui_atributos)," no es numérico.",sep=""))
            
          })
          #Reiniciamos la salida 
          output$dosvariables_mensajes_print <- renderPrint({invisible()})
        }
    
  })
  
  #En caso de que se pulse el botón reset
  observeEvent(input$dosvariables_Action_factoReset, {
  
    #Reiniciamos la variable global que indica que se ha factorizado
    ficheroFactorizado<<-"False"
    
    #Mostramos el sumario general del dataset
    output$dosvariables_sumarioGeneral <- renderPrint({
      file<-filedata()
      summary(file)
    })
    
    #Reiniciamos las salidas
    output$dosvariables_mensajes_factorizar <- renderText({ print("Se ha reiniciado el dataset a los tipos originales.")})
    output$dosvariables_mensajes_print <- renderText({ })
    
  })
  
  
  #------EXPLORACIÓN DE UNA VARIABLE---------
  
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
      #Variables que se observan
      ficherocargando<-input$datafile
      APIAction<-input$API_Action
      
    #Variables a explorar
    output$atributoUnaVariable <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items<-names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
    })
  
  })
  
  #Cambiamos los valores de los combos si se produce el evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoUnaVariable <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
    })
  })
  
  #RESET del evento de factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoUnaVariable <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
    })
  })
  
  Funcion_ExploracionTabular<-eventReactive(input$Exploraciones_ejecutar1,{
    #Seleccionamos el archivo según se ha factorizado o no.
    if (ficheroFactorizado=="True"){
    df<-fileout_fact_dosVar
    }else{
      df<-filedata()
    }
    if (is.null(df)) return(NULL)
    #Definimos el tipo de operación
    switch(input$tipoExploracion1, 
           Sumario={
              fileout<-summary(df[,input$atributoUnaVariable])
           },
           Media={
             fileout<-mean(df[,input$atributoUnaVariable])
           },
           Desviacion={
             fileout<-sd(df[,input$atributoUnaVariable])   
           },
           Varianza={
             fileout<-var(df[,input$atributoUnaVariable])  
           }
    )
    salidaExploraciones1<-fileout
  })
  
  #Evento que espera el botón de acción y llama a la función anterior
  observeEvent(input$Exploraciones_ejecutar1, {
    Valores<-Funcion_ExploracionTabular()
    output$resultados_exploracion1 <- renderPrint({
      Valores
    })
    output$mensajes_exploracion1 <- renderText({
      print(paste(isolate(input$tipoExploracion1)," del atributo ",isolate(input$atributoUnaVariable),".",sep=""))
    })

  })


  
  #--Exploraciones Gráficas---------
  
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
    #Variables que se observan
    ficherocargando<-input$datafile
    APIAction<-input$API_Action
    
    #inicializamos el combo
    output$atributoUnaVariableGrafica <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica", "Atributo:",items)
      
    })
  
  })
  
  #En caso de evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoUnaVariableGrafica <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica", "Atributo:",items)
      
    })
  
  })
  
  #RESET de la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoUnaVariableGrafica <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica", "Atributo:",items)
      
    })
  })
  
  
  
  #Evento que muestra las gráficas sin esperar un ActionButton
  observe({
    
    output$explor1_grafica1 <- renderPlot({
      #Comprobamos si se ha factorizado antes o no
      if (ficheroFactorizado=="True"){
        df<-fileout_fact_dosVar
      }else{
        df<-filedata()
      }
      #Diferenciamos los casos según corresponda
      #Sólo en caso de que el atributo no sea nominal
      if(class(df[,input$atributoUnaVariableGrafica])!="character"){
      switch(input$tipoExploracionGrafica1, 
             histograma={
               if (class(df[,input$atributoUnaVariableGrafica])=="numeric" || class(df[,input$atributoUnaVariableGrafica])=="integer" ){

                   hist(df[,input$atributoUnaVariableGrafica],main = "Histograma", xlab=input$atributoUnaVariableGrafica)
                 
                       #Mensaje positivo
                      output$mensajes_exploracionGrafica <- renderText(
                      print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
                      )
                 
                 }else{
                   #Mensaje de error
                   output$mensajes_exploracionGrafica <- renderText(
                     print(paste("Error: el atributo <",input$atributoUnaVariableGrafica,"> no es numérico.",sep=""))
                   )
                  }
             },
             caja={
               
                 if (class(df[,input$atributoUnaVariableGrafica])!="factor"){
                   
                       boxplot(df[,input$atributoUnaVariableGrafica],main = "Diagrama de Caja", xlab=input$atributoUnaVariableGrafica)
                   
                      #Mensaje positivo 
                       output$mensajes_exploracionGrafica <- renderText(
                        print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
                       )
                       
                 }else{
                   #Mensaje error factor
                   output$mensajes_exploracionGrafica <- renderText(
                     print(paste("Error: el atributo <",input$atributoUnaVariableGrafica,"> no es numérico.",sep=""))
                   )
                 }
               },
             plot={
                     plot(df[,input$atributoUnaVariableGrafica],main = "Plot", xlab=input$atributoUnaVariableGrafica, ylab="Valores")
               
                      #Mensaje positivo
                      output$mensajes_exploracionGrafica <- renderText(
                      print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
               )
               },
             pie_chart={
               pie(table(df[,input$atributoUnaVariableGrafica]))
               
               #Mensaje positivo 
               output$mensajes_exploracionGrafica <- renderText(
                 print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
               )
             }
             
             
          )
      }else{
        #Mensaje de error
        output$mensajes_exploracionGrafica <- renderText(
          print(paste("Error: el atributo: ",input$atributoUnaVariableGrafica," no es numérico o necesita ser factorizado."))
        )
      }
      
    })
    
     output$explor1_grafica1_info <- renderText({
       paste0("x=", input$explor1_grafica1_click$x, "\ny=", input$explor1_grafica1_click$y)
     })
     
     output$explor1_grafica2_info <- renderText({
       paste0("x=", input$explor1_grafica2_click$x, "\ny=", input$explor1_grafica2_click$y)
     })
    
  })
  
  #Segunda ventana gráfica
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
    #Variables que se observan
    ficherocargando<-input$datafile
    APIAction<-input$API_Action
      
    #Segunda ventana gráfica
    output$atributoUnaVariableGrafica2 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica2", "Atributo:",items)
      
    })
  })
  
  #En caso de evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoUnaVariableGrafica2 <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica2", "Atributo:",items)
      
    })
    
  })
  
  #RESET de la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoUnaVariableGrafica2 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariableGrafica2", "Atributo:",items)
      
    })
  })
  
  
  #Evento que muestra la gráfica 2 sin esperar un ActionButton
  observe({
    
    output$explor1_grafica2 <- renderPlot({
      #Comprobamos si se ha factorizado antes o no
      if (ficheroFactorizado=="True"){
        df<-fileout_fact_dosVar
      }else{
        df<-filedata()
      }
      #Diferenciamos los casos según corresponda
      #Sólo en caso de que el atributo no sea nominal
      if(class(df[,input$atributoUnaVariableGrafica2])!="character"){
        switch(input$tipoExploracionGrafica2, 
               histograma={
                 if (class(df[,input$atributoUnaVariableGrafica2])=="numeric" || class(df[,input$atributoUnaVariableGrafica2])=="integer"){
                   
                   hist(df[,input$atributoUnaVariableGrafica2],main = "Histograma", xlab=input$atributoUnaVariableGrafica2)
                   
                   #Mensaje positivo
                   output$mensajes_exploracionGrafica2 <- renderText(
                     print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
                   )
                   
                 }else{
                   #Mensaje de error
                   output$mensajes_exploracionGrafica2 <- renderText(
                     print(paste("Error: el atributo <",input$atributoUnaVariableGrafica2,"> no es numérico.",sep=""))
                   )
                 }
               },
               caja={
                 
                 if (class(df[,input$atributoUnaVariableGrafica2])!="factor"){
                   boxplot(df[,input$atributoUnaVariableGrafica2],main = "Diagrama de Caja", xlab=input$atributoUnaVariableGrafica2)
                   
                   #Mensaje positivo 
                   output$mensajes_exploracionGrafic2a <- renderText(
                     print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
                   )
                   
                 }else{
                   #Mensaje error factor
                   output$mensajes_exploracionGrafica2 <- renderText(
                     print(paste("Error: el atributo <",input$atributoUnaVariableGrafica2,"> no es numérico.",sep=""))
                   )
                   
                 }
                 
               },
               plot={
                 plot(df[,input$atributoUnaVariableGrafica2],main = "Plot", xlab=input$atributoUnaVariableGrafica2, ylab="Valores") 
                 
                 #Mensaje positivo
                 output$mensajes_exploracionGrafica2 <- renderText(
                   print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
                 )
               },
               
             pie_chart={
               pie(table(df[,input$atributoUnaVariableGrafica2]))
               
               #Mensaje positivo
               output$mensajes_exploracionGrafica2 <- renderText(
                 print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
               )
             }
        )
      }else{
        #Mensaje de error
        output$mensajes_exploracionGrafica2 <- renderText(
          print(paste("Error: el atributo: ",input$atributoUnaVariableGrafica2," no es numérico o necesita ser factorizado."))
        )
      }
      
    })
    
  })
  
  #-------EXPLORACIÓN DE DOS VARIABLES---------------
  
  
  #--Relacion tabular entre dos variables--
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
    #Variables que se observan
    ficherocargando<-input$datafile
    APIAction<-input$API_Action

    #Renderizamos los atributos en 'dosvariables_Ui_rela_at1'
    output$dosvariables_Ui_rela_at1 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at1", "Atributo 1:",items)
    })
    
    
    #Renderizamos los atributos en 'dosvariables_Ui_rela_at2'
    output$dosvariables_Ui_rela_at2 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at2", "Atributo 2:",items)
      
    })
  
 })
  
  #Evento que cambia los valores de los combos dosvariables_Ui_rela_at1 y dosvariables_Ui_rela_at2, si se ha factorizado algún atributo
  observeEvent(input$dosvariables_Action_factorizar,{

    #Renderizamos los atributos en 'dosvariables_Ui_rela_at1'
      output$dosvariables_Ui_rela_at1 <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at1", "Atributo 1:",items)
    })


    #Renderizamos los atributos en 'dosvariables_Ui_rela_at2'
    output$dosvariables_Ui_rela_at2 <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at2", "Atributo 2:",items)

    })

  })

  #Evento que hace RESET de los combos respecto a la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    #Renderizamos los atributos en 'dosvariables_Ui_rela_at1'
    output$dosvariables_Ui_rela_at1 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at1", "Atributo 1:",items)
    })
    
    
    #Renderizamos los atributos en 'dosvariables_Ui_rela_at2'
    output$dosvariables_Ui_rela_at2 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_rela_at2", "Atributo 2:",items)
      
    })
    
  })

  
  #En este caso no la función se encuentra incorporada dentro del código
  observeEvent(input$dosvar_Action_relaTab, {
    at1=input$dosvariables_Ui_rela_at1
    at2=input$dosvariables_Ui_rela_at2
    if (ficheroFactorizado=="False"){
          df <-filedata()
         }else{
          #El fichero que deja la función de factorización en intervalos
          df<-fileout_fact_dosVar
         }
         if (is.null(df)) return(NULL)
         fileout<-table(df[,at1],df[,at2])
        
       output$dosvar_Print_relaTab <- renderPrint({
          fileout
       })
     })
    
  #Para limpiar el cuadro de texto generado por la ejecución de 'dosvar_Action_relaTab' 
  observeEvent(input$dosvar_Action_resetTab, {
    output$dosvar_Print_relaTab <- renderPrint({})
  })
  
 
  
  #---Realizamos la relación gráfica de dos variables conjuntamente
  
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
    #Variables que se observan
    ficherocargando<-input$datafile
    APIAction<-input$API_Action
    
    #Inicializamos los combos
    output$atributoDosVariablesGraficas1 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("atributoDosVariablesGraficas1", "Atributo 1:",items)
      
    })
    
    output$atributoDosVariablesGraficas2 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("atributoDosVariablesGraficas2", "Atributo 2:",items)
      
    })
  })
  
  #Cambiamos los valores de los combos si se produce el evento de factorizacion
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoDosVariablesGraficas1 <- renderUI({
      df<-fileout_fact_dosVar
      items=names(df)
      selectInput("atributoDosVariablesGraficas1", "Atributo 1:",items)
  
    })
  
    output$atributoDosVariablesGraficas2 <- renderUI({
      df<-fileout_fact_dosVar
      items=names(df)
      selectInput("atributoDosVariablesGraficas2", "Atributo 2:",items)
  
    })
  })
  
  #Hacemos RESET de los combos respecto a la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoDosVariablesGraficas1 <- renderUI({
      df<-filedata()
      items=names(df)
      selectInput("atributoDosVariablesGraficas1", "Atributo 1:",items)
      
    })
    
    output$atributoDosVariablesGraficas2 <- renderUI({
      df<-filedata()
      items=names(df)
      selectInput("atributoDosVariablesGraficas2", "Atributo 2:",items)
      
    })
  })
  
  #Evento que escucha el cambio de variables en los combos y actualiza las gráficas
  observe({

    #Renderizamos los atributos en 'dosvariables_Ui_rela_at1'
    output$explor1_dosvar_grafica1 <- renderPlot({
      if (ficheroFactorizado=="False"){
        df <-filedata()
        if (is.null(df)) return(NULL)
      }else{
        df<-fileout_fact_dosVar
      }
      at1<-input$atributoDosVariablesGraficas1
      at2<-input$atributoDosVariablesGraficas2
      
      if( (class(df[,at1][[1]])=="factor" || class(df[,at1][[1]])=="ordered")   && (class(df[,at2][[1]])=="factor" || class(df[,at2][[1]])=="ordered")){
        
        output$mensajes_dosvar_exploracionGrafica <- renderText({
          print("Gráfica Factor/ Factor.")
        })
        
        mosaicplot(table(df[,at1],df[,at2]), col=c("gray","black"), main="Factor / Factor", xlab=at1,ylab=at2)
        
      }else if((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")) {
        
        output$mensajes_dosvar_exploracionGrafica <- renderText({
          print("Gráfica Numeral / Numeral.")
        })
        
        plot(df[,at1],df[,at2],main="Numeral / Numeral", xlab=at1,ylab=at2)
        
      }else if (class(df[,at1])!="character" && (class(df[,at2][[1]])=="factor" || class(df[,at2][[1]])=="ordered")){
        
        output$mensajes_dosvar_exploracionGrafica <- renderText({
          print("Gráfica Numeral / Factor.")
        })
        
        boxplot(df[,at1] ~  df[,at2], main="Numeral / Factor",xlab=at2,ylab=at1)
        
      }else if ((class(df[,at1][[1]])=="factor" || class(df[,at1][[1]])=="ordered") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")){
         
         output$mensajes_dosvar_exploracionGrafica <- renderText({
            print("Error: El atributo tipo factor debe asignarse a la variable Atributo 2, para poder mostrar el diagrama de caja.")
          })
          
        }else{
          output$mensajes_dosvar_exploracionGrafica <- renderText({
          print("Error: Alguno de los atributos es de tipo caracter y no puede ser relacionado gráficamente.")
            })
        }
      
      output$explor1_dosvar_info <- renderText({
         paste0("x=", input$plot1_dosvar_click$x, "\ny=", input$plot1_dosvar_click$y)
       })

    })

  }) #Fin del Observe
  
  #----CORRELACIONES ENTRE DOS VARIABLES-----#####
  
  #Para que se refresque cada vez que se carga un dataset nuevo
  observe({
    
    #Variables que se observan
    ficherocargando<-input$datafile
    APIAction<-input$API_Action
    
    #Inicializamos los combos
    output$dosvariables_Ui_correla_at1 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at1", "Atributo 1:",items)
      
    })
    
    output$dosvariables_Ui_correla_at2 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at2", "Atributo 2:",items)
      
    })
  
  })
  
  #En caso de evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$dosvariables_Ui_correla_at1 <- renderUI({
      df<-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at1", "Atributo 1:",items)
      
    })
    
    output$dosvariables_Ui_correla_at2 <- renderUI({
      df<-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at2", "Atributo 2:",items)
      
    })
    
  })
  
  #RESET de la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$dosvariables_Ui_correla_at1 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at1", "Atributo 1:",items)
      
    })
    
    output$dosvariables_Ui_correla_at2 <- renderUI({
      df<-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      selectInput("dosvariables_Ui_correla_at2", "Atributo 2:",items)
      
    })
  })

  #Evento que ejecuta el tets de correlación
  observeEvent(input$dosvar_Action_correlacion,{
    
      at1<-input$dosvariables_Ui_correla_at1
      at2<-input$dosvariables_Ui_correla_at2
      if (ficheroFactorizado=="False"){
        df<-filedata()
        if (is.null(df)) return(NULL)
        
      }else{
        df<-fileout_fact_dosVar
      }
      
      if ((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")){
        #Mostramos el resultado del test de correlación
        output$dosvar_Print_correlacion <- renderPrint({
          #Devolvemos la correlación como variable global
          correlacion<<-cor(df[,at1],df[,at2])
          cor.test(df[,at1],df[,at2])
        })
        
        #Dibujamos el gráfico de correlación
        output$graf_correla_dosVariables <- renderPlot({
          plot(df[,at1],df[,at2], main=paste("Cor:",correlacion),xlab=at1,ylab=at2)
          
        })
        
        output$dosvar_msj_correlacion <- renderText({
          print("Se muestran los resultados del test de correlación.")
        })
        
      }else{
        output$dosvar_msj_correlacion <- renderText({
          print("Error: Alguno de los atributos no es numérico, no se puede ejecutar el test de correlación.")
        })
        
      }
  })
  
#---ANÁLISIS MULTIVARIABLE--------
  
 
  #--Código que muestra las relaciones gráficas si se pulsa el botón action
  observeEvent(input$multivar_Action_gra,{
    if (ficheroFactorizado=="False"){
      df<-filedata()
      if (is.null(df)) return(NULL)
      
    }else{
      df<-fileout_fact_dosVar
    }
    
    colcharacter<-"False"
    
    #Dibujamos los gráfico 
    output$multivar_graf_plot <- renderPlot({
      #Mostramos la relación de todas la variables numéricas
      #Buscamos las variables no-numéricas
      for (i in ncol(df)){
        if (class(df[,i])=="character"){
          colcharacter<<-"True"
          df[,i]<-NULL
        }
      }
      pairs(df)
      
    })
    
    output$multivar_msj_graf <- renderText({
      if (colcharacter=="False"){
        print("Gráficas de relación de todos los atributos del dataset.")
      }else{
        print("Se muestran únicamente las gráficas de relación entre atributos numéricos o factores.")
      }
    })
    
  })
    
  #------Correlación multivariable
  
  #--Código que muestra las relaciones gráficas si se pulsa el botón action
  observeEvent(input$multivar_Action_correlacion,{
    if (ficheroFactorizado=="False"){
      df<-filedata()
      if (is.null(df)) return(NULL)
      
    }else{
      df<-fileout_fact_dosVar
    }
    
    colcharactermulti<-"False"
    
    
    #Mostramos la relación de todas la variables numéricas
    #Buscamos las variables no-numéricas y las eliminamos
    e<-0
    for (i in 1:ncol(df)){
      as<-"22"
      i<-i+e
      if (i<(ncol(df)+1)){
        if (class(df[,i])=="character" || class(df[,i])=="factor"){
          colcharactermulti<<-"True"
          df[,i]<-NULL
          e<- e-1
        }
      }
    }
    
    #Mostramos los resultados
    output$multivar_print_correlacion <- renderPrint({
      
      corr.test(df)
      
    })
    
    output$multivar_msj_correlacion <- renderText({
      if (colcharactermulti!="False"){
        print("Se muestran los valores de correlación y p-values de todos los atributos.")
      }else{
        print("Se muestran únicamente los valores de correlación y p-values de las variables de caracter numérico.")
      }
    })
    
  })
  
  #Dibujamos las gráficas
  observeEvent(input$multivar_Action_correlacion,{
    if (ficheroFactorizado=="False"){
      df<-filedata()
      if (is.null(df)) return(NULL)
      
    }else{
      df<-fileout_fact_dosVar
    }
    
  colcharactermulti<-"False"
  
  output$multivar_graf_correla <- renderPlot({
    #Mostramos la relación de todas la variables numéricas
    #Buscamos las variables no-numéricas
    for (i in ncol(df)){
      if (class(df[,i])=="character"){
        colcharactermulti<<-"True"
        df[,i]<-NULL
      }
    }
   corrgram(df,order=FALSE,
            main="Correlograma del dataset",
            lower.panel=panel.conf, upper.panel=panel.ellipse,
            diag.panel=panel.minmax, text.panel=panel.txt)
    
   })
  })
  
  #-----REGRESIONES LINEALES-----####
  
  #---SLR----
  output$reglinealsimple_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("reglinealsimple_at1", "Atributo X:",items)
    
  })
  
  output$reglinealsimple_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("reglinealsimple_at2", "Atributo Y:",items)
    
  })
  
  #Ejecutamos el modelo SLR
  observeEvent(input$reglinealsimple_Action,{
      df<-filedata()
      if (is.null(df)) return(NULL)
      at1<-input$reglinealsimple_at1
      X<-df[,at1]
      at2<-input$reglinealsimple_at2
      Y<-df[,at2]
      
      
      if ((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")){
          
        #Exportamos el modelo SLR como variable global
         modelo<<-lm( Y ~ X, data=df )
         
         
         #Código para mostrar valores predichos vs reales -Evaluación del modelo
         index <- sample(1:nrow(df),round(0.75*nrow(df)))
         train <- df[index,]
         test <- df[-index,]
         
         #Creamos la formula
         formulacion <- as.formula(paste(at2," ~ ", at1, collapse ="" ))
         
         #Se genera el modelo de SLR en base a los datos de entrenamiento
         lm.fit <- glm(formulacion, data=train)
         
         #valores predichos sobre los datos de test xon in CI del 95%
         pr.lm <- predict(lm.fit,test,level= 95)
         
         #Se calcula el error cuadrático medio que se mostrará por pantalla
         MSE.lm <- sum((pr.lm - test[,at2])^2)/nrow(test)
      
         
         
         #Mostramos los resultados
         output$reglienalsimple_print <- renderPrint({ summary(modelo) })
         
         #Mostramos el MSE (Error cuadrático)
         output$reglienalsimple_MSE <- renderText({ print(paste("MSE = ",MSE.lm,sep="")) })
         
         
         #Mostramos el modelo generado
         output$reglienalsimple_plotSLR<-renderPlot({
                    plot(X, Y,  ylab = at2, xlab = at1, main = "Modelo SLR generado") 
          abline(modelo) 
          
          #Leyenda con la formula del modelo
          legend("topleft",paste(at2," = ",round(modelo$coefficients[1],3)," + ",at1,"*",round(modelo$coefficients[2],3),sep=""),bty="n")
    
         })
         
         #Permitimos el uso de coordenadas en la gráfica anterior
         output$reglienalsimple_plotSLR_info <- renderText({
           paste0("x=", input$reglienalsimple_plotSLR_click$x, "\ny=", input$reglienalsimple_plotSLR_click$y)
         })
         
         #Histograma de los residuos
         output$reglienalsimple_plot2 <- renderPlot({ 
                hist(modelo$residuals, xlab = "Residuos", col = "gray", 
                main = "Distribución de los residuos") 
           })
         
         #Q-Q de los residuos
         output$reglienalsimple_plot3 <- renderPlot({ 
               qqnorm(modelo$residuals, main = "Q-Q Plot de los residuos") 
               qqline(modelo$residuals) 
           })
         
         #Variación ecuanime plot
         output$reglienalsimple_plot4 <- renderPlot({
              plot(modelo$fitted.values, modelo$residuals, ylab = "Residuos", 
                xlab = "Valores ajustados", main="Distribución de residuos") 
               abline(0, 0, lwd = 3)
         })
         
         #Mostramos el mensaje
         output$reglienalsimple_msj <- renderText({ print(paste("Se muestran los datos del modelo de regresión lineal (SLR) generado.", sep="")) })
     
         
         #Plot de evaluación SLR
         output$reglienalsimple_plot1 <- renderPlot({
           
           #Aislamos de los movimientos del combo de la variable Y (at2)
           isolate({
             plot(test[,input$reglinealsimple_at2],pr.lm,col='blue',main='Real vs Predicho (95%)',pch=18, cex=0.7,xlab=isolate(paste(input$reglinealsimple_at2,"_real",sep="")), ylab=isolate(paste(input$reglinealsimple_at2,"_predicho",sep="")),asp=1)
             abline(0,1,lwd=2)
             legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
           })
         })
         
         #Guardamos una salida out 
         output$salidaOkMostrarVentanasSLR <- reactive({valorDevuelto<-"TRUE"})
         
         
         #Devolvemos la condición a ui.R
         outputOptions(output, "salidaOkMostrarVentanasSLR", suspendWhenHidden = FALSE)
         
         
      }else{
        
        #Mostramos el mensaje
        output$reglienalsimple_msj <- renderText({ print("Error: alguno de los atributos (X o Y) no es numérico.") })
        
        #Guardamos una salida out 
        output$salidaOkMostrarVentanasSLR <- reactive({valorDevuelto<-"FALSE"})

        #Devolvemos la condición a ui.R para ocultar paneles
        outputOptions(output, "salidaOkMostrarVentanasSLR", suspendWhenHidden = FALSE)
        
        
      }
        
  
      
  })
  
  
  #Predicción de valores de Y en función de X
  observeEvent(input$SLR_prediccion_Action,{
    df<-filedata()
    if (is.null(df)) return(NULL)

    #Obtenemos la lista de posibles valores de X
    valoresX<-input$valorX 
    arrayList<-strsplit(valoresX,";")[[1]]
    #Cuidado con definir X=, pues el modelo se construyó con lm(Y ~ X), hay que seguir la misma estructura
    prediccion<-predict.lm(modelo, data.frame(X=as.numeric(arrayList)), level= as.numeric(input$intervaloConfianza), interval = "prediction",na.action = na.pass) 
    
    #Mostramos los resultados
    output$SLR_prediccion_print <- renderPrint({ prediccion })
    
  })
  
  #-------Regresión lineal Múltiple  MLR--------
  
  
  
  output$reglinealmulti_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("reglinealmulti_at2", "Atributo Y:",items)
    
  })
  
  #Ejecutamos la correlación
  observeEvent(input$reglinealmulti_Action,{
    df<-filedata()
    if (is.null(df)) return(NULL)
    
    #Comprobamos si algún valor no es numérico
    valorCaracter<-"False"
    
    if(input$reglinealmulti_at1!=""){
      #Obtenemos la lista de columnas
      valoresX<-input$reglinealmulti_at1 
      arrayList<-strsplit(valoresX,";")[[1]]
      
      #Verificamos si los campos insertados se encuentran en el dataset
      outTry1<-tryCatch(
        {
          X<-df[,arrayList]
        },
        warning = function(w) {
          malaSalida<-"True"
        },
        error = function(e) {
          malaSalida<-"True"
        }
        
      )
      
      #Creamos la fórmula sólo en el caso de que los valores de entrada sean correctos
      if(outTry1!="True"){
        #at2<-input$reglinealmulti_at2
        #Y<-df[,at2]
        Y<-input$reglinealmulti_at2
        
        #Artefacto para construir la fórmula
        xnom<-c("")
        
        for (i in 1:length(arrayList)){
          if (class(df[,arrayList[i]])!="integer" && class(df[,arrayList[i]])!="numeric" || class(df[,input$reglinealmulti_at2])=="factor"){
            valorCaracter<-"True"
          }
          assign(paste("X",arrayList[i],sep=""),df[,arrayList[i]])
          
        }
        #xnom <- paste("X",arrayList,sep="")
        #formu<-paste(xnom,collapse="+")
        formu<-paste(arrayList,collapse="+")
        
      }
      
     
          
    }else{
      
      #Liberamos outTry1, en caso de que se haya producido un error anterior
      outTry1<-"False"
      
      #Comprobamos si hay columnas tipo caracter o factor para generar error
      columnasCaracter<-sapply(df,is.numeric)
      if(FALSE %in% columnasCaracter || class(df[,input$reglinealmulti_at2])=="factor" ){
        valorCaracter<-"True"
      }else{
        #Obtenemos la lista de columnas
        Y<-input$reglinealmulti_at2
        formu<-"."
      }
   
    }
    
    if (valorCaracter=="False" && outTry1!="True"){
      
      #Exportamos el modelo como variable global
      if(input$reglinealmulti_at1!=""){
        modelo<<-lm( as.formula(paste(Y,"~", formu)), data=df )
        #modelo<<-lm( as.formula(paste("Y ~", formu)), data=df )
      }else{
        modelo<<-lm( as.formula(paste(Y,"~", formu)), data=df )
      }
      
      #Mostramos los resultados
      output$reglienalmulti_print <- renderPrint({ summary(modelo) })
      
      #Código para mostrar valores predichos vs reales -Evaluación del modelo
      index <- sample(1:nrow(df),round(0.75*nrow(df)))
      train <- df[index,]
      test <- df[-index,]
      
      
      pr.lm <- predict(modelo,test,level= 95)
      MSE.lm <- sum((pr.lm - test[,input$reglinealmulti_at2])^2)/nrow(test)
      
      
      #Mostramos el MSE (Error cuadrático)
      output$reglienalmulti_MSE <- renderText({ print(paste("MSE = ",MSE.lm,sep="")) })
      
      
      #Mostramos la gráfica de evaluación del modelo vs real
      output$reglienalmulti_plot1 <- renderPlot({
        #Aislamos de los movimientos del comb de la variable Y (at2)
        isolate({
          #Plot de evaluación
          plot(test[,input$reglinealmulti_at2],pr.lm,col='blue',main='Real vs Predicho (95%)',pch=18, cex=0.7,xlab=isolate(paste(input$reglinealmulti_at2,"_real",sep="")),ylab=isolate(paste(input$reglinealmulti_at2,"_predicho",sep="")),asp=1)
          abline(0,1,lwd=2)
          legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
        })
      })
      
      #Histograma de los residuos
      output$reglienalmulti_plot2 <- renderPlot({
        hist(modelo$residuals, xlab = "Residuos", col = "gray",
             main = "Distribución de los residuos")
      })
      
    
       #Variación ecuanime plot
       output$reglienalmulti_plot3 <- renderPlot({
         #Dividimos la pantalla para mostrar las gráficas del modelo
         layout(matrix(c(1,2,3,4),2,2))
         plot(modelo)
       })
      
       
       #Mostramos el mensaje
       output$reglienalmulti_msj <- renderText({ print(paste("Se muestran los datos del modelo de regresión lineal múltiple (MLR) generado.",sep="")) })
   
       #Guardamos una salida out 
       output$salidaOkMostrarVentanasMLR <- reactive({valorDevueltoMLR<-"TRUE"})
         
       
       #Devolvemos la condición a ui.R
       outputOptions(output, "salidaOkMostrarVentanasMLR", suspendWhenHidden = FALSE)
       
       
       
        }else{
      
      #Mostramos el mensaje
      output$reglienalmulti_msj <- renderText({ print("Error: alguno de los atributos (X o Y) no es numérico, o no existe en el dataset.") })
      
      #Guardamos una salida out 
      output$salidaOkMostrarVentanasMLR <- reactive({valorDevueltoMLR<-"FALSE"})

      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOkMostrarVentanasMLR", suspendWhenHidden = FALSE)
      
      #Reset de mensajes
      output$reglienalmulti_print <- renderPrint({invisible()})
      output$reglienalmulti_MSE <- renderPrint({invisible()})
    }
          
    
  })
  

  
  #Guardamos una salida out para consulta cada vez que se carge un archivo para ser pronosticada la salida del modelo MLR
  output$fileMLRdatacargado <- eventReactive(input$regreMultiEvaluation,{
    output$regreMultiEvaluation_msj<-renderText({
      print("Se ha cargado un archivo para realizar pronósticos según el modelo MLR.")
    })
  })
  
  #Devolvemos la condición de que el fichero (NN) se ha cargado a la variable para consultar desde ui.R <conditionalPanel>
  outputOptions(output, "fileMLRdatacargado", suspendWhenHidden = FALSE)
  
  
  #Mostramos el fichero cargado para pronosticar en base al modelo MLR
  observe({
    
    fileMRLCarga<-input$regreMultiEvaluation
    
    if(is.null(fileMRLCarga)){ return(NULL)}
    
    #Leemos el archivo y tratamos los '?' como NA
    fileEvalCargado<-read.csv(fileMRLCarga$datapath,sep=",",na.strings=c("?",""),stringsAsFactors = TRUE)
    
    #mostramos los resultados obtenidos
    output$regreMultiEvaluation <- renderTable({
      fileEvalCargado
    })
    
  })
  
  
  #Comprobamos si el fichero para ejecutar los pronosticos está bien construido
  Function_ficheroMLRConstruido<-function(df){
    
    AComparar<-filedata()
    if(is.null(AComparar)){ return(NULL)}
    
    if(input$reglinealmulti_at1==""){
      indice<-grep(input$reglinealmulti_at2,colnames(AComparar))
      ficheroResultante<-AComparar[,-indice]
      arrayNamesList<-names(ficheroResultante)
    }else{
      valoresX<-input$reglinealmulti_at1 
      arrayNamesList<-strsplit(valoresX,";")[[1]]
    }

    
    
    condicion<-(arrayNamesList==names(df))
    if (is.element(FALSE,condicion)){
      return(FALSE)
    }else{
      return(TRUE)
    }
    
  }
  
  
  observeEvent(input$evalCSVmultiRegre_Action,{
    
    #Recogemos el nombre del fichero 
    fileMLREval <- input$regreMultiEvaluation 
    
    if(is.null(fileMLREval)){ return(NULL)}
    
    #Leemos el archivo y tratamos los '?' como NA
    fileMLRPred<-read.csv(fileMLREval$datapath,sep=",",na.strings=c("?",""),stringsAsFactors = TRUE)
    
    #Guardamos el fichero inicial sin cambios para generar la tabla en caso de erro de formato del archivo de entrada para pronóstico
    fileMLRPredInicial<-fileMLRPred
    
    #Debemos comprobar sin el fichero está bien construido de acuerdo al modelo de red
    estBien<-Function_ficheroMLRConstruido(fileMLRPred)
    
    if (estBien==TRUE){
      
      prediccion<-predict.lm(modelo, fileMLRPred, level= 0.95, interval = "prediction",na.action = na.pass) 
      
      #añadimos al dataframe de salida los campos predichos
      fileMLRPred[,paste("fit_",input$reglinealmulti_at2,sep="")]<-as.data.frame(prediccion)$fit
      fileMLRPred[,paste("lwr_",input$reglinealmulti_at2,sep="")]<-as.data.frame(prediccion)$lwr
      fileMLRPred[,paste("upr_",input$reglinealmulti_at2,sep="")]<-as.data.frame(prediccion)$upr
      
      #mostramos los resultados obtenidos
      output$regreMultiEvaluation <- renderTable({
        fileMLRPred
      })
      
      output$regreMultiEvaluation_msj<-renderText({
        print("Se ha verificado la validez del dataset importado / Se muestran los resultados del modelo.")
      })
      
    }else{
      
      output$regreMultiEvaluation_msj<-renderText({
        print("Error: alguno de los valores de entrada del dataset no se corresponde con los utilizados por el modelo MLR.")
      })
      
      #mostramos los resultados obtenidos
      output$regreMultiEvaluation <- renderTable({
        fileMLRPredInicial
      })
      
    }
  
  })
  
  

  
  
  #----REDES NEURONALES--------#
  

  
  output$redneuronal_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("redneuronal_at2", "NN_OUT",items)
    
  })
  
  
  observeEvent(input$redneuronal_Action,{
    df<-filedata()
    if (is.null(df)) return(NULL)
    hayCaracter<-FALSE

    
    #Barra de progreso mostrada
    withProgress(message = 'Generando red...',detail = 'Puede tardar un poco...', value = 0, {
      
    
    #Ordenamos el data.frame para que la variable salida OUT de la red quede en la última posición
    col_idx <- grep(input$redneuronal_at2, names(df)) 
    df <- df[, c((1:ncol(df))[-col_idx],col_idx)]
    
    #Guardamos df en una variable global para posteriorres predicciones
    df_paraEvalRed <<-df
    incProgress(1/4)
    
    #Comprobamos que no haya ni caracteres ni factores en los atributos de entrada
      columnasCaracter<-sapply(df,is.character)
      columnasFactor<-sapply(df,is.factor)
   
    
      if(TRUE %in% columnasCaracter || TRUE %in% columnasFactor){
        hayCaracter<-TRUE
        
      }else{
        maxs <- apply(df, 2, max) 
        mins <- apply(df, 2, min)

        df_escalado <- as.data.frame(scale(df, center = mins, scale = maxs - mins))
        
        n <- names(df)
        #Exportamos a global para poder ser usada luego en comprobación de datos de pronósticos
        formulacion <- as.formula(paste(input$redneuronal_at2,"~", paste(n[!n %in% input$redneuronal_at2], collapse = " + ")))
        if(input$hidenLayers==2){
           nn <<- neuralnet(formulacion,data=df_escalado,hidden=c(as.numeric(input$neurLayer1),as.numeric(input$neurLayer2)),linear.output=T)

        }else{
          nn <<-neuralnet(formulacion,data=df_escalado,hidden=c(as.numeric(input$neurLayer1)),linear.output=T)
        }
      } 
    incProgress(2/4)
    
    
    if(hayCaracter==FALSE){

      
      #Ordenamos el data.frame para que la variable salida OUT de la red quede en la última posición
      col_idx <- grep(input$redneuronal_at2, names(df)) 
      df <- df[, c((1:ncol(df))[-col_idx],col_idx)]
      
      
        #recogemos un índice para los datos de entrenamiento y eveluación
        index <- sample(1:nrow(df),round(0.75*nrow(df)))
        maxs <<- apply(df, 2, max) 
        mins <<- apply(df, 2, min)
        
        #Valores sin escalar
        train <- df[index,]
        test <- df[-index,]
        
        #Se escalan los datos
        scaled <- as.data.frame(scale(df, center = mins, scale = maxs - mins))
        train_ <- scaled[index,]
        test_ <- scaled[-index,]
        n <- names(train_)
        
        #Formamos la fórmula para obtener el modelo
        formul <- as.formula(paste(input$redneuronal_at2,"~", paste(n[!n %in% input$redneuronal_at2], collapse = " + ")))
        #formul <- as.formula(paste(input$redneuronal_at2," ~",input$redneuronal_at1))
        
        #Creamos el modelo en función del número de capas
        if(input$hidenLayers==2){
          nn_eval <- neuralnet(formul,data=train_,hidden=c(as.numeric(input$neurLayer1),as.numeric(input$neurLayer2)),linear.output=T)
        }else{
          nn_eval <- neuralnet(formul,data=train_,hidden=c(as.numeric(input$neurLayer1)),linear.output=T)
        }
        
        incProgress(3/4)
        
        #Computamos los datos de test de entrada, todas las columnas menos la última que es donde está la salida de la red
        aComputar<-as.numeric(ncol(test_)-1)
         pr.nn <- compute(nn_eval,test_[,1:aComputar])
         pr.nn_ <- pr.nn$net.result*(max(df[,input$redneuronal_at2])-min(df[,input$redneuronal_at2]))+min(df[,input$redneuronal_at2])
         test.r <- (test_[,input$redneuronal_at2])*(max(df[,input$redneuronal_at2])-min(df[,input$redneuronal_at2]))+min(df[,input$redneuronal_at2])
         
         #Obtenemos el error cuadrático medio
         MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
       
      incProgress(4/4)  
        
      #Se muestran las salidas correspondientes
      output$redneuronal_msj<-renderText({
        print("Se muestra el modelo de red neuronal generado a partir de los datos.")
      })
      
      output$redneural_plot1 <- renderPlot({
       plot(nn,rep="best")
      })
      
      #Se muestran las salidas correspondientes
      output$redneuronal_msj2<-renderText({
        print(paste("MSE = ",MSE.nn,sep=""))
      })
      
      #Mostramos la gráfica del error cuadrático MSE
      output$redneural_plot2 <- renderPlot({
        par(mfrow=c(1,2))
        
        plot(test[,input$redneuronal_at2],pr.nn_,col='red',main='Reales vs Predichos (NN)',pch=18,cex=0.7,xlab="Valores Reales",ylab="Valores Predichos")
        abline(0,1,lwd=2)
        legend('bottomright',legend='NN',pch=18,col='red', bty='n')
        par(mfrow=c(1,1))
      })
      
      
      #Guardamos una salida out 
      output$salidaOKNN <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKNN", suspendWhenHidden = FALSE)
      
      
    }else{
      output$redneuronal_msj<-renderText({
        print("Error: alguno de los valores de entrada no es numérico")
      })
      
      #Guardamos una salida out 
      output$salidaOKNN <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKNN", suspendWhenHidden = FALSE)
      
    }
    
    })#withprogress
  })
  
  
  #Guardamos una salida out para consulta cada vez que se carge un archivo para ser pronosticada la salida de la NN
  output$fileNeuraldatacargado <- eventReactive(input$neuralFileEvaluation,{
    output$redneuronal_msj3<-renderText({
      print("Se ha cargado un archivo para realizar pronósticos según el modelo de red generado.")
    })
  })
  
  #Devolvemos la condición de que el fichero (NN) se ha cargado a la variable para consultar desde ui.R <conditionalPanel>
  outputOptions(output, "fileNeuraldatacargado", suspendWhenHidden = FALSE)
  
  
  #Mostramos el fichero cargado para pronosticar en base a la red neuronal generada
  observe({
    
    fileNeuCarga<-input$neuralFileEvaluation
    
    if(is.null(fileNeuCarga)){ return(NULL)}
    
    #Leemos el archivo y tratamos los '?' como NA
    fileEvalCargado<-read.csv(fileNeuCarga$datapath,sep=",",na.strings=c("?",""),stringsAsFactors = TRUE)
    
    #mostramos los resultados obtenidos
    output$tableEvalNeuronal <- renderTable({
      fileEvalCargado
    })
  
  })
  
  
  #Comprobamos si el fichero para ejecutar los pronosticos está bien construido
  Function_ficheroNNConstruido<-function(df){
    #Eliminamos del dataset la variable a pronosticar
    indice<-grep(input$redneuronal_at2,colnames(df_paraEvalRed))
    nombresOrigen<-df_paraEvalRed[,-indice]
    condicion<-names(nombresOrigen)==names(df)
    if (is.element(FALSE,condicion)){
      return(FALSE)
    }else{
      return(TRUE)
    }
    
  }
  
  
  observeEvent(input$evalCSVNeuronal_Action,{
  
  #Recogemos el nombre del fichero 
  fileNeuralEval <- input$neuralFileEvaluation 
  
  if(is.null(fileNeuralEval)){ return(NULL)}
  
  #Leemos el archivo y tratamos los '?' como NA
  fileNeuralPred<-read.csv(fileNeuralEval$datapath,sep=",",na.strings=c("?",""),stringsAsFactors = TRUE)
  #Guardamos el fichero inicial sin cambios para generar la tabla en caso de erro de formato del archivo de entrada para pronóstico
  fileNeuralPredInicial<-fileNeuralPred
  
  #Debemos comprobar sin el fichero está bien construido de acuerdo al modelo de red
  estBien<-Function_ficheroNNConstruido(fileNeuralPred)
  
  if (estBien==TRUE){
  
  #Los valores mins y maxs han sido declarados como globales durante la generación del modelo.
  #Tenemos que quitar la última columna de mins y maxs que sería la predicción
  valoresEscalados <- as.data.frame(scale(fileNeuralPred, center = mins[1:length(mins)-1], scale = maxs[1:length(maxs)-1] - mins[1:length(mins)-1]))
  
  pr.nn <- compute(nn,valoresEscalados)
  #Des-escalamos los valores
  pr.nn_ <- pr.nn$net.result*(max(df_paraEvalRed[,input$redneuronal_at2])-min(df_paraEvalRed[,input$redneuronal_at2]))+min(df_paraEvalRed[,input$redneuronal_at2])
  
  #Construimos el csv final
  fileNeuralPred$NN_OUT<-as.data.frame(pr.nn_)
  
  #mostramos los resultados obtenidos
  output$tableEvalNeuronal <- renderTable({
    fileNeuralPred
  })
  
  output$redneuronal_msj3<-renderText({
    print("Se ha verificado la validez del dataset importado / Se muestran los resultados del modelo.")
  })
  
  }else{
    
    output$redneuronal_msj3<-renderText({
      print("Error: alguno de los valores de entrada del dataset no se corresponde con los utilizados por el modelo de NN.")
    })
   
    #mostramos los resultados obtenidos
    output$tableEvalNeuronal <- renderTable({
      fileNeuralPredInicial
    })
    
  }
  
  
  
  })
  
  #--ANÁLISIS DE CLUSTERS--#
  
  output$cluster_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("cluster_at1", "Atributo X:",items)
    
  })
  
  output$cluster_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("cluster_at2", "Atributo Y:",items)
    
  })
  
  
  #Ejecutamos la función de clustering
  observeEvent(input$cluster_Action,{
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Random
    set.seed(123)
    
    #Comprobamos que todos los atributos de entrada son numéricos
    if (is.numeric(df[,input$cluster_at1]) &&  is.numeric(df[,input$cluster_at2]) && !is.na(as.numeric(input$cluster_n1)) && !is.na(as.numeric(input$cluster_n2))){

      #EL dataset debe contener las 2 columnas seleccionadas únicamente
      df<-df[,c(input$cluster_at2,input$cluster_at1)]
      
      
      #Hacemos globales los modelos
      dos<<-kmeans(df,as.numeric(input$cluster_n1))
      tres<<-kmeans(df,as.numeric(input$cluster_n2))
      
      #Pintamos las salidas tabulares de los clusters genrados
      output$cluster_print1 <- renderPrint({ dos })
      output$cluster_print2 <- renderPrint({ tres })
      
      
      
      #Creamos un fichero con dos nuevas columnas con el cluster al que pertenece el registro
      clus<-cbind(df,clus2=dos$cluster,clus3=tres$cluster)
      at1<<-clus[,input$cluster_at1]
      at2<<-clus[,input$cluster_at2]
      
      
      #Dibujamos cluster 1
      output$cluster_plot1 <- renderPlot({
        plot(at1, at2, col=dos$cluster, #asp=1
             pch=dos$cluster, main="Dos Clusters",
             xlab=isolate(input$cluster_at1), ylab=isolate(input$cluster_at2),
             xlim=c(min(at1),max(at1)), ylim=c(min(at2),max(at2)))
        points(dos$centers[,2], dos$centers[,1], pch=23,
               col="maroon", bg="lightblue", cex=3)
        text(dos$centers[,2], dos$centers[,1], cex=1.1,
              col="black", attributes(dos$centers)$dimnames[[1]])
      })
      
      #Dibujamos cluster 2
      output$cluster_plot2 <- renderPlot({
        plot(at1, at2, col=tres$cluster, #asp=1, 
             pch=tres$cluster, main="Tres Clusters",
             xlab=isolate(input$cluster_at1), ylab=isolate(input$cluster_at2))
        points(tres$centers[,2], tres$centers[,1], pch=23,
               col="maroon", bg="lightblue", cex=3)
        text(tres$centers[,2], tres$centers[,1], cex=1.1,
             col="black", attributes(tres$centers)$dimnames[[1]])
      })
      
  
      output$cluster_msj <- renderText({
        print("Se muestran los clusters generados.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClusters <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClusters", suspendWhenHidden = FALSE)
    
    }else{
      
      #Mensaje de error
      output$cluster_msj <- renderText({
        print("Error: algunos de los atributos de entrada no es numérico.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClusters <- reactive({valorDevuelto<-"False"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClusters", suspendWhenHidden = FALSE)
      
    }
    
  })
 


   #Actualizamos la salida en función de los valores de los combos
  observe({
    clus1<-input$cluster_explo1
    output$cluster_print1 <- renderPrint({

      switch(clus1,
             Sumario={
               dos
             },
             Clusters={
               dos$cluster
             },
             Centers={
               dos$centers
             },
             Totss={
               dos$totss
             },
             Withinss={
               dos$withinss
             },
             Tot.Withinss={
               dos$tot.withinss
             },
             Betweens={
               dos$betweens
             },
             Size={
               dos$size
             },
             Iter={
               dos$iter
             },
             Ifault={
               dos$ifault
             }
      )
    })
  })

  observe({
    clus2<-input$cluster_explo2
    output$cluster_print2 <- renderPrint({

      switch(clus2,
             Sumario={
               tres
             },
             Clusters={
               tres$cluster
             },
             Centers={
               tres$centers
             },
             Totss={
               tres$totss
             },
             Withinss={
               tres$withinss
             },
             Tot.Withinss={
               tres$tot.withinss
             },
             Betweens={
               tres$betweens
             },
             Size={
               tres$size
             },
             Iter={
               tres$iter
             },
             Ifault={
               tres$ifault
             }

      )
    })
  })
  
  
  
  #-------Clustering Jerarquico----
  
  output$clusterj_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("clusterj_at1", "Atributo X:",items)
    
  })
  
  output$clusterj_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("clusterj_at2", "Atributo Y:",items)
    
  })
  
  #Funciones que se ejecutan al pulsar el botón de acción
  observeEvent(input$clusterj_Action,{
    
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Guardamos las variables
    at1<-input$clusterj_at1
    at2<-input$clusterj_at2
    
    
    #Barra de progreso mostrada
    withProgress(message = 'Generando dendograma...',detail = 'Puede tardar un poco...', value = 0, {
    
    
    if(is.numeric(df[,input$clusterj_at1]) &&  is.numeric(df[,input$clusterj_at2]) && !is.na(as.numeric(input$clusterj_nclusters))){
    
    #Generamos un nuevo fichero a partir de las dos columnas
    df<-df[,c(at1,at2)]
    
   #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
    df[,paste(at1,"scale1",sep="_")]<-as.numeric(scale(df[,at1]))
    df[,paste(at2,"scale2",sep="_")]<-as.numeric(scale(df[,at2]))
    
    #Lo globalizamos para que sea accesible
    fileClusterNorm<<-df
    incProgress(1/4)
    
   #Creamos la semilla y el modelo jerarquico
    set.seed(456)
    hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
    incProgress(2/4)
    
    #Visualizacion del modelo y exportamos a variable global
    dendro<<-stats::as.dendrogram(hc_model)
    incProgress(3/4)
    
     #Exportamos como variable global para que pueda ser recogido por el siguiente evento
     dendro_six_color<<-color_branches(dendro, k=as.numeric(input$clusterj_nclusters))
     incProgress(4/4)
     
      output$clusterj_plot1 <- renderPlot({
        plot(dendro_six_color,leaflab="none", horiz=TRUE,
             main="Dendrograma de los atributos seleccionados", xlab="Altura")
      #abline(v=37.5, lty="dashed", col="blue")
      })
    
      output$clusterj_msj <- renderText({
        print("Se muestran los clusters generados mediante técnicas jerárquicas.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClustersJe <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClustersJe", suspendWhenHidden = FALSE)
      
    
    }else{
      
      output$clusterj_msj <- renderText({
        print("Error: alguno de los atributos de entrada no es numérico.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClustersJe <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClustersJe", suspendWhenHidden = FALSE)
      
    }
      
    })#With progress
    
  })
  
  observeEvent(input$clusterj_AddValorCorte,{
    valor<-input$clusterj_corte

    
     output$clusterj_plot1 <- renderPlot({
       
       #Barra de progreso mostrada
       withProgress(message = 'Generando corte...',detail = 'Puede tardar un poco...', value = 0, {
         incProgress(1/3)
         
          plot(dendro_six_color,leaflab="none", horiz=TRUE,
            main="Dendrograma de los atributos seleccionados", xlab="Altura")
          abline(v=valor, lty="dashed", col="blue")
          incProgress(2/3)
          incProgress(3/3)
       })
       
     })

    output$clusterj_print <- renderPrint({
      str(cut(dendro,h=valor)$upper)
    })
    
    #Guardamos una salida out 
    output$salidaOKClustersJeCorte <- reactive({valorDevuelto<-"TRUE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKClustersJeCorte", suspendWhenHidden = FALSE)
    
    output$clusterj_plotFinal <- renderPlot({
      
      #Barra de progreso mostrada
      withProgress(message = 'Generando modelo...',detail = 'Puede tardar un poco...', value = 0, {
        incProgress(1/4)
        
        
      #Incluimos la gráfica de análisis para jerarquía
      #Preparando los resultados
      df<-fileClusterNorm
      
      #Guardamos las variables
      at1<-isolate(input$clusterj_at1)
      at2<-isolate(input$clusterj_at2)
      
      #Creamos la semilla y el modelo jerarquico
      set.seed(456)
      hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
     
      #Visualizacion del modelo y exportamos a variable global
      
      dendrog<<-as.dendrogram(hc_model)
      
      modelo <- kmeans(df[, 3:4], as.numeric(input$clusterj_nclusters)) 
      incProgress(2/4)
      
      #Continuamos mostrando las graficas comparadas
      df$clusModelo <- modelo$cluster 
      dend_modelo <- dendextend::cutree(dendrog, k = as.numeric(input$clusterj_nclusters)) 
      df$dendModelo <- dend_modelo 
      incProgress(2/4)
      
      if(!require("dplyr")) install.packages("dplyr") 
      suppressMessages(suppressWarnings(library(dplyr))) 
      
      #Tenemos que cambiar el valor de age e incoming para poder 
      #usarlos como simbolos en la agrupación siguiente de labels
      colnames(df)[1] <- "var1"
      colnames(df)[2] <- "var2"
      
      #Esportamos para que lo recojan las funciones de print más abajo
      dfmodelado<<-df
      
      #Creamos un nuevo dataframe agrupando
      labels <- as.data.frame(df %>%  
                                group_by(dendModelo) %>%  
                                summarise(avg_age = median(var1), avg_inc = median(var2))) 
      incProgress(3/4)
      
      #Dibujamos la gráfica teniendo en cuenta que hemocs cambiado el nombre de las 2 primeras columnas
      plot(df$var1, df$var2, col = df$dendModelo, 
           pch = df$dendModelo - 1, xlab = isolate(input$clusterj_at1), ylab = isolate(input$clusterj_at2),
           main = paste0("Clusters jerárquicos para K = ",input$clusterj_nclusters,sep="")) 
      #Añadimos los centros
      points(labels[ ,2], labels[ ,3], pch = 21, col = 'maroon', bg = 'white', cex = 3) 
      #Texto de los centros
      text(labels[, 2], labels[, 3], cex = 1.1, col = 'black', labels[, 1]) 
      incProgress(4/4)
      
      })#withProgress
      
    })
    
    
    
    output$clusterj_print1 <- renderPrint({
      dfmodelado %>% group_by(dendModelo) %>% summarise(ClusterSize = n()) 
    })
    
    output$clusterj_print2 <- renderPrint({ 
      dfmodelado %>% group_by(dendModelo) %>%  
        summarise(min_age = min(var1), med_age = median(var1), 
                  max_age = max(var1), med_inc = median(var2), 
                  min_inc = min(var2), max_inc = max(var2)) 
    })
    
    

      
  })
  
  
  
  
  #-----------Evaluación de los modelos de clustering
  output$clustereva_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("clustereva_at1", "Atributo X:",items)
    
  })
  
  output$clustereva_at2 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("clustereva_at2", "Atributo Y:",items)
    
  })
  
  observeEvent(input$clustereva_Action,{
    
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Guardamos las variables
    at1<-input$clustereva_at1
    at2<-input$clustereva_at2
    
    
    #Barra de progreso mostrada
    withProgress(message = 'Generando modelos...',detail = 'Puede tardar un poco...', value = 0, {
    incProgress(1/4)
    
    #Comprobamos que todos los atributos de entrada son numéricos
    if (is.numeric(df[,input$clustereva_at1]) &&  is.numeric(df[,input$clustereva_at2])){
      
    
      #Generamos un nuevo fichero a partir de las dos columnas
      df<-df[,c(at1,at2)]
      
      #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
      df[,paste(at1,"scale1",sep="_")]<-as.numeric(scale(df[,at1]))
      df[,paste(at2,"scale2",sep="_")]<-as.numeric(scale(df[,at2]))
      incProgress(2/4)
      
      set.seed(456) 
      dos <- kmeans(df[, 3:4], 2) 
      tres <- kmeans(df[, 3:4], 3) 
      cuatro <- kmeans(df[, 3:4], 4) 
      cinco <- kmeans(df[, 3:4], 5) 
      seis <- kmeans(df[, 3:4], 6) 
      siete <- kmeans(df[, 3:4], 7) 
      ocho <- kmeans(df[, 3:4], 8) 
      nueve <- kmeans(df[, 3:4], 9) 
      diez <- kmeans(df[, 3:4], 10) 
      incProgress(3/4)
      
    # Evaluamos los modelos 
      optimizado <- data.frame(clusters = c(2:10), wss = rep(0, 9)) 
      optimizado[1, 2] <- as.numeric(dos$tot.withinss) 
      optimizado[2, 2] <- as.numeric(tres$tot.withinss) 
      optimizado[3, 2] <- as.numeric(cuatro$tot.withinss) 
      optimizado[4, 2] <- as.numeric(cinco$tot.withinss) 
      optimizado[5, 2] <- as.numeric(seis$tot.withinss) 
      optimizado[6, 2] <- as.numeric(siete$tot.withinss) 
      optimizado[7, 2] <- as.numeric(ocho$tot.withinss) 
      optimizado[8, 2] <- as.numeric(nueve$tot.withinss) 
      optimizado[9, 2] <- as.numeric(diez$tot.withinss) 
      incProgress(4/4)
      
      output$clusterelbow_plot1 <- renderPlot({
        
        plot(optimizado$wss ~ optimizado$clusters, type = "b",  
                    ylim = c(0, 12000), ylab = 'Suma del Error Cuadrático', 
                    main = 'Número óptimo de clusters basado en el MSE', 
                    xlab = 'Número de clusters', pch = 17, col = 'black') 
         })
      
      output$clustereva_msj <- renderText({
        print("Método del codo para elegir el número de clusters.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClustersEva <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClustersEva", suspendWhenHidden = FALSE)
      
    }else{
      
      output$clustereva_msj <- renderText({
        print("Error: alguno de los atributos de entrada no es numérico.")
      })
      
      #Guardamos una salida out 
      output$salidaOKClustersEva <- reactive({valorDevuelto<-"False"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKClustersEva", suspendWhenHidden = FALSE)
      
    }
      
    })#withProgress
  })
  
  observeEvent(input$clustereva_CompaAction,{
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Guardamos las variables
    at1<-input$clustereva_at1
    at2<-input$clustereva_at2
    
    #Barra de progreso mostrada
    withProgress(message = 'Generando modelos...',detail = 'Puede tardar un poco...', value = 0, {
      incProgress(1/4)
      
    #Generamos un nuevo fichero a partir de las dos columnas
    df<-df[,c(at1,at2)]
    
    #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
    df[,paste(at1,"scale1",sep="_")]<-as.numeric(scale(df[,at1]))
    df[,paste(at2,"scale2",sep="_")]<-as.numeric(scale(df[,at2]))
    
    #Creamos la semilla y el modelo jerarquico
    set.seed(456)
    hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
    incProgress(2/4)
    
    #Visualizacion del modelo y exportamos a variable global
    dendro<<-as.dendrogram(hc_model)
    
    cinco <- kmeans(df[, 3:4], as.numeric(input$cluster_eval1)) 
    seis <- kmeans(df[, 3:4], as.numeric(input$cluster_eval2)) 
    
    #Continuamos mostrando las graficas comparadas
    df$clus5 <- cinco$cluster 
    dend_five <- dendextend::cutree(dendro, k = as.numeric(input$cluster_eval1)) 
    df$dend5 <- dend_five 
    incProgress(3/4)
    
    df$clus6 <- seis$cluster 
    dend_six <- dendextend::cutree(dendro, k = as.numeric(input$cluster_eval2)) 
    df$dend6 <- dend_six
    incProgress(4/4)
    
    # Choosing a Model 
    output$clustereva_plot1 <- renderPlot({
       
      plot(df[,input$clustereva_at1], df[,input$clustereva_at2], col = cinco$cluster, 
        pch = cinco$cluster, xlab = isolate(input$clustereva_at1), ylab=isolate(input$clustereva_at2), main = '5-means Clustering') 
    })
    
    output$clustereva_plot3 <- renderPlot({
      
      plot(df[,input$clustereva_at1], df[,input$clustereva_at2], col = seis$cluster, xlab = isolate(input$clustereva_at1), 
        ylab = isolate(input$clustereva_at2), pch = seis$cluster, main = '6-means Clustering') 
    
    })
    
    output$clustereva_plot2 <- renderPlot({
      
      plot(df[,input$clustereva_at1], df[,input$clustereva_at2], col = df$dend5, 
             pch = df$dend5, xlab=isolate(input$clustereva_at1), ylab=isolate(input$clustereva_at2), main = 'k = 5 Jerárquico') 
    })
    
    output$clustereva_plot4 <- renderPlot({
    
      plot(df[,input$clustereva_at1], df[,input$clustereva_at2], col = df$dend6, xlab=isolate(input$clustereva_at1), ylab = isolate(input$clustereva_at2),  
            pch = df$dend6, main = 'k = 6 Jerárquico') 
    })
  
    })#withProgress
  })
  
 
  
  #-------FILTRADO COLABORATIVO----------
  #Iniciamos variables de control
  SeHaAfinado<<-FALSE
  
  #------Consulta de datos de la matriz principal------------
  
  #Geenramos el slidebar de reducción de la dispersión de la matriz dinámicamente
  observe({
    
    #Lo que se observa
    cambio1<-input$RedDisp_Guardar
    cambio2<-input$RedDisp_Reset
    
    
    #El fichero que cogemos para renderizar los combos
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }
    if (is.null(df)) return(NULL)
    
    
    output$datosRecomendaciones_at1 <- renderUI({
   
      items=names(df)
      selectInput("datosRecomendaciones_at1", "Usuarios:",items)
      
    })
    
    output$datosRecomendaciones_at2 <- renderUI({
  
      items=names(df)
      selectInput("datosRecomendaciones_at2", "Items:",items)
      
    })
    
    output$datosRecomendaciones_at3 <- renderUI({
  
      items=names(df)
      selectInput("datosRecomendaciones_at3", "Valoraciones:",items)
      
    })
  
  }) 
  
  #Estos combos además deven renderizarse dinámicamente cada vez que se carga la matriz inicial
  observe({
    
    #Escucha dinámica
    espera<-input$datosRecomendaciones_Action
    
        #Slider dinámico en función de la dispersión de la matriz de valores
        output$RedDisperMatrix <- renderUI({
          
          sliderInput("RedDisperMatrix", "Nº mínimo de valoraciones por Item:", 
                      min = 1, max = valorDispMax, value = 1, step= 1)
          
        })
        
        #Slider dinámico en función de la dispersión de la matriz de valores
        output$RedDisperMatrix2 <- renderUI({
          
          sliderInput("RedDisperMatrix2", "Nº mínimo de valoraciones por Usuario:", 
                      min = 1, max = valorDispMaxRows, value = 1, step= 1)
          
        })
    })

  #Función que es llamada cada vez que hay que ocultar los paneles de filtrado colaboratico (Evalaluación y Recomendaciones)
  Funcion_OcultaPanelesEvaluacRecomen<-function(){
    
    #Ocultamos los paneles en de Evaluaciones y Recomendaciones
    
    #Guardamos una salida out para los paneles de la evaluación de modelos
    output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
    
    #Guardamos una salida out para los paneles las recomendaciones
    output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"FALSE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKrecomendarAction", suspendWhenHidden = FALSE)
    
    #Reiniciamos mensajes de Evaluaciones y Recomendaciones
    output$modelEval_msj <- renderPrint({invisible()})
    output$colaborativo_msj <- renderPrint({invisible()})
  }
  
  #Mostramos los resultados de la dispersión de la matriz
  observeEvent(input$datosRecomendaciones_Action,{
    
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }
    
    if (is.null(df)) return(NULL)
    
    if (is.numeric(df[,input$datosRecomendaciones_at3])) {
      
      #Se genera la matriz a partir del dataframe
      matriz<-acast(df, df[,input$datosRecomendaciones_at1]~df[,input$datosRecomendaciones_at2], value.var=input$datosRecomendaciones_at3)
      
      #Convertimos la matriz y hacemos de r una variable global para qe pueda ser usada por el resto de funciones
      #La devolvemos globalmente para ejecutar reducciones de dispersión
      r <<- as(matriz,"realRatingMatrix")
      
      output$datosRecomendaciones_msj<-renderText({
        print("Dispersión de la matriz de datos para realizar recomendaciones.")
      })
      
      output$dispersionMatriz_msj<-renderText({
        print(paste("Los items tienen como mínimo: ",min(colCounts(r))," valoraciones, y como máximo: ",max(colCounts(r))," valoraciones.",sep=""))
      })
      
      output$dispersionMatriz_msj2<-renderText({
        print(paste("Los usuarios tienen como mínimo: ",min(rowCounts(r))," valoraciones, y como máximo: ",max(rowCounts(r))," valoraciones.",sep=""))
      })
      
      #Reiniciamos el dialogo de dispersión inferior
      output$dispersionReduccionHecha_msj<-renderPrint({invisible()})

      
    #Devolvemos los valores de dispersión para generar el slider de reducción dinámicamente  
    valorDispMin<<-min(colCounts(r))
    valorDispMax<<-max(colCounts(r))
    
    valorDispMinRows<<-min(rowCounts(r))
    valorDispMaxRows<<-max(rowCounts(r))
    
      
      output$datosRecomendaciones_plot1<-renderPlot({
       image(r, main="Matriz de calificaciones")
      })
      
      #Hacemos zoom en la matriz
      if(nrow(r)>20 && ncol(r)>50){
        output$datosRecomendaciones_plot2<-renderPlot({
          image(r[1:20,1:50], main="Dispersión de la matriz")
        })
      }else{
        #No hace falta el detalle
        output$datosRecomendaciones_plot2<-renderPlot({})
      }
      
      
      #Guardamos una salida out 
      output$salidaOKDispersion <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKDispersion", suspendWhenHidden = FALSE)
      
      #Guardamos una salida out 
      output$salidaOKSlidersDisp <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKSlidersDisp", suspendWhenHidden = FALSE)
      
    }else{
      output$datosRecomendaciones_msj<-renderText({
        print("Error: el atributo <Valoraciones> no es de tipo numérico.")
      })
      
      output$datosRecomendaciones_plot1<-renderPlot({})
      output$datosRecomendaciones_plot2<-renderPlot({})
      
      #Guardamos una salida out 
      output$salidaOKDispersion <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKDispersion", suspendWhenHidden = FALSE)
      
      #Guardamos una salida out 
      output$salidaOKSlidersDisp <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKSlidersDisp", suspendWhenHidden = FALSE)
    }
    
    
    #Ocultamos paneles
    Funcion_OcultaPanelesEvaluacRecomen()
    
    
  })
  
  
  
  #Visualizamos los resultados de la reducción de la dispersión por nº de evaluacines de los Items
  observeEvent(input$RedDisp_Action,{
    
    out<-tryCatch({
      
    if(SeHaAfinado==FALSE){
      
    matriz_afi<-r
    
      
      while (min(rowCounts(matriz_afi))< input$RedDisperMatrix2 || min(colCounts(matriz_afi)) < input$RedDisperMatrix) {
        matriz_afi <- matriz_afi[rowCounts(matriz_afi)>input$RedDisperMatrix2,colCounts(matriz_afi)>input$RedDisperMatrix]
      }
       
    
    
    output$datosRecomendaciones_plot1<-renderPlot({
      image(matriz_afi, main="Matriz de calificaciones afinada")
    })
    
    #Dibujamos el zoom de la matriz
    if(nrow(matriz_afi)>20 && ncol(matriz_afi)>50){
        output$datosRecomendaciones_plot2<-renderPlot({
          image(matriz_afi[1:20,1:50], main="Dispersión de la matriz afinada")
        })
    }else{
        #No dibujamos nada porque habría suficiente información con una gráfica
        output$datosRecomendaciones_plot2<-renderPlot({})
    }
    
    output$dispersionMatriz_msj<-renderText({
      print(paste("Los items de la matriz afinada tienen como mínimo: ",min(colCounts(matriz_afi))," valoraciones, y como máximo: ",max(colCounts(matriz_afi))," valoraciones.",sep=""))
    })
    
    output$dispersionMatriz_msj2<-renderText({
      print(paste("Los usuarios de la matriz afinada tienen como mínimo: ",min(rowCounts(matriz_afi))," valoraciones, y como máximo: ",max(rowCounts(matriz_afi))," valoraciones.",sep=""))
    })
    
    output$dispersionReduccionHecha_msj<-renderText({
      print("Se ha reducido la dispersión de la matriz y actualizado los valores de los paneles superiores.")
    })
    
    #Devolvemos la matriz afinada recompuesta como un dataframe para que pueda ser usado en la generación de modelos y emisión de recomendaciones
    df_Reco_afinado<<-as(matriz_afi,"data.frame")
    
    #Devolvemos globalmente la matriz afinada
    matriz_afinada<<-matriz_afi
    
    }else{
      
      output$dispersionReduccionHecha_msj<-renderText({
        print("Error: la matriz ya se encuentra afinada. <Reset> para volver a la situación inicial.")
      })
    }
    
    }, #Fin del TryCatch
    warning=function(w){
      output$dispersionReduccionHecha_msj<-renderText({
        print("Warning: la matriz no cumple los criterios para poder realizar la operación. Se pierde mucha información.")
      })
      return(NULL)
    },
    error=function(e){
      output$dispersionReduccionHecha_msj<-renderText({
        print("Error: la matriz no cumple los criterios para poder realizar la operación. Se pierde mucha información.")
      })
      return(NULL)
    }
    
    )#Fin TryCatch
    
    
  })
  
  observeEvent(input$RedDisp_Guardar,{
    
    if(SeHaAfinado==FALSE){
      
    #Con la bandera, ya se puede recoger matriz_afinada del paso anterior
    SeHaAfinado<<-TRUE
    
    #Devolvemos los valores de dispersión de la nueva matriz
    valorDispMin<<-min(colCounts(matriz_afinada))
    valorDispMax<<-max(colCounts(matriz_afinada))
    
    output$dispersionReduccionHecha_msj<-renderText({
      print("Se guarda la nueva configuración matricial de cara la generación de modelos y recomendaciones.")
    })
    
    output$datosRecomendaciones_msj<-renderText({
      print("Dispersión de la matriz AFINADA de datos para realizar recomendaciones. <Reset> para volver a la situación inicial.")
    })
    
    #Guardamos una salida out 
    output$salidaOKSlidersDisp <- reactive({valorDevuelto<-"FALSE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKSlidersDisp", suspendWhenHidden = FALSE)
    
    #Ocultamos paneles
    Funcion_OcultaPanelesEvaluacRecomen()
    
    }else{
      
      output$dispersionReduccionHecha_msj<-renderText({
        print("Error: la matriz ya se encuentra afinada. <Reset> para volver a la situación inicial.")
      })
      
    }
    
  })
  
  observeEvent(input$RedDisp_Reset,{
    
    SeHaAfinado<<-FALSE
    
    output$dispersionReduccionHecha_msj<-renderText({
      print("Se vuelto a la situación matricial inicial.")
    })
    
    output$datosRecomendaciones_msj<-renderText({
      print("Dispersión de la matriz de datos para realizar recomendaciones.")
    })
    
    output$dispersionMatriz_msj<-renderText({
      print(paste("Los items tienen como mínimo: ",min(colCounts(r))," valoraciones, y como máximo: ",max(colCounts(r))," valoraciones.",sep=""))
    })
    
    output$dispersionMatriz_msj2<-renderText({
      print(paste("Los usuarios tienen como mínimo: ",min(rowCounts(r))," valoraciones, y como máximo: ",max(rowCounts(r))," valoraciones.",sep=""))
    })
    
    #Devolvemos los valores de dispersión para generar el slider de reducción dinámicamente  
    valorDispMin<<-min(colCounts(r))
    valorDispMax<<-max(colCounts(r))
    
    
    output$datosRecomendaciones_plot1<-renderPlot({
      image(r, main="Matriz de calificaciones")
    })
    
    #Hacemos zoom en la matriz
    if(nrow(r)>20 && ncol(r)>50){
      output$datosRecomendaciones_plot2<-renderPlot({
        image(r[1:20,1:50], main="Dispersión de la matriz")
      })
    }else{
      #No hace falta el detalle
      output$datosRecomendaciones_plot2<-renderPlot({})
    }
    
    
    #Guardamos una salida out 
    output$salidaOKDispersion <- reactive({valorDevuelto<-"TRUE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKDispersion", suspendWhenHidden = FALSE)
    
    #Guardamos una salida out 
    output$salidaOKSlidersDisp <- reactive({valorDevuelto<-"TRUE"})
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKSlidersDisp", suspendWhenHidden = FALSE)
    
    #Ocultamos paneles
    Funcion_OcultaPanelesEvaluacRecomen()
  })
  
  
  
  #-----Evaluación de modelos-------------

  observe({
    
    #Lo que se observa
    cambio1<-input$RedDisp_Guardar
    cambio2<-input$RedDisp_Reset
    
    #El fichero que cogemos para renderizar los combos
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }
    if (is.null(df)) return(NULL)
    
  output$modelEval_at1 <- renderUI({

    items=names(df)
    selectInput("modelEval_at1", "Usuarios:",items)
    
  })
  
  output$modelEval_at2 <- renderUI({

    items=names(df)
    selectInput("modelEval_at2", "Items:",items)
    
  })
  
  output$modelEval_at3 <- renderUI({

    items=names(df)
    selectInput("modelEval_at3", "Valoraciones:",items)
    
  })
  
  
  })
  
  #Generamos la función de evalaución de los modelos
  Funcion_evaluacionModelosFiltrado<-function(df){

  #Comprobamos que el atributo valoraciones es numérico
  if (is.numeric(df[,input$modelEval_at3])){
    
    #Barra de progreso mostrada
    withProgress(message = 'Evaluando modelos...',detail = 'Puede tardar un poco...', value = 0, {
      incProgress(1/4)
      
    #Se genera la matriz a partir del dataframe
    matriz<-acast(df, df[,input$modelEval_at1]~df[,input$modelEval_at2], value.var=input$modelEval_at3)
    
    
    #Convertimos la matriz y hacemos de r una variable global para qe pueda ser usada por el resto de funciones
    r <- as(matriz,"realRatingMatrix")
    incProgress(2/4)
    
  #Creamos una lista con los algoritmos a evaluar para la matriz real.
  models_to_evaluate <- list(  
    random = list(name = "RANDOM", param=NULL),
    PUPULAR = list(name = "POPULAR", param = NULL),
    IBCF_jac = list(name = "IBCF", param = list(method = "jaccard", k=30)),
    IBCF_cos = list(name = "IBCF", param = list(method = "cosine", k=30)),  
    UBCF_jac = list(name = "UBCF", param = list(method= "jaccard", nn=25)),
    UBCF_cos = list(name = "UBCF", param = list(method = "cosine", nn=25))
  )
 
  
  #Número de Items que vamos a usar para evaluar el esquema, lo ideal es que sea menor que el mínimo dispuesto por un usuario
  if(min(rowCounts(r))>2){
     minItems<-min(rowCounts(r)) 
    items_dados<- minItems-1
  }else{
    items_dados<-1
  }
  
  eval_sets <- evaluationScheme(data = r, method = "cross-validation", k = input$modelEval_sliderEval, given = items_dados, goodRating=3)

  
  n_recommendations <- c(1, 3, 5, 10)
  
  #Generamos los resultados de la evaluación sobre el modelo
  list_results <- evaluate(x = eval_sets, method = models_to_evaluate, 
                           n = n_recommendations)
  incProgress(3/4)
  incProgress(4/4)
  
  salidaDeLaFuncion<-list_results
  
    })#Withprogess
    
  }else{
    return(NULL)
  }
  
    
  }
  
  
  
  #Observamos el botón de acción para evaluar los modelos de filtrado colaborativo
  observeEvent(input$modelEval_Action,{
    
    #Recogemos el dataframe en función de si la matriz ha sido afinada o no
    if(SeHaAfinado==TRUE){
      df<-df_Reco_afinado
    }else{
      df<-filedata()
    }
    if (is.null(df)) return(NULL)
    
    out<-tryCatch(
      
      {
     
        list_results<-Funcion_evaluacionModelosFiltrado(df)
        
        if (!is.null(list_results)){
          
        par(mfrow = c(1, 2))
          
        #Dibujamos la curva ROC
        output$modelEval_plot1<-renderPlot({
          
          #Sentencia de control de errores a la hora de generar la gráfica curva ROC
          devuelve1<-tryCatch(
            {
              plot(list_results, annotate = 1, legend = "bottomright")
              title("Curva ROC")
            },warning=function(w){
              output$modelEval_msj<-renderText({
                print("Error: se ha producido un warning al tratar de representar la curva ROC. Es posible que los datos de entrada no cumplan los criterios mínimos de calidad.")
              })
              output$modelEval_plot1<-renderPlot({})
              output$modelEval_plot2<-renderPlot({})
              
              #Guardamos una salida out 
              output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
              
              #Devolvemos la condición a ui.R para ocultar los paneles 
              outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
            },
            error=function(e){
              output$modelEval_msj<-renderText({
                print("Error: se ha producido un error al tratar de representar la curva ROC. Es posible que los datos de entrada no cumplan los criterios mínimos de calidad.")
              })
              output$modelEval_plot1<-renderPlot({})
              output$modelEval_plot2<-renderPlot({})
              
              #Guardamos una salida out 
              output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
              
              #Devolvemos la condición a ui.R para ocultar los paneles 
              outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
            }
          ) #Fin de la rentencia de control de errores a la hora de generar la gráfica curva ROC
          
          #Devolvemos el resultado del control de errores anterior
          return(devuelve1)
          
        })#output$modelEval_plot1<-renderPlot
        
        output$modelEval_msj<-renderText(
          print("Se muestras las gráficas de evaluación de los modelos.")
        )
        
        #Dibujamos la curva precision/recall
        output$modelEval_plot2<-renderPlot({
          
          #Sentencia de control de errores a la hora de generar la gráfica Precision/Recall
          devuelve<-tryCatch(
            {
              plot(list_results, "prec/rec", ylim = c(0,1), annotate = 1, legend = "topright")
              title("Precision - Recall")
            },warning=function(w){
              
              output$modelEval_msj<-renderText({
               "Error: se ha producido un warning al tratar de representar las curvas del modelo. Es posible que los datos de entrada no cumplan los criterios mínimos de calidad."
              })
              output$modelEval_plot1<-renderPlot({})
              output$modelEval_plot2<-renderPlot({})
              
              #Guardamos una salida out 
              output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
              
              #Devolvemos la condición a ui.R para ocultar los paneles 
              outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
            },
            error=function(e){
              
                output$modelEval_msj<-renderText({
                 print("Error: se ha producido un error al tratar de representar las curvas del modelo. Es posible que los datos de entrada no cumplan los criterios mínimos de calidad.")
                })              
              output$modelEval_plot1<-renderPlot({})
              output$modelEval_plot2<-renderPlot({})
              
              #Guardamos una salida out 
              output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
              
              #Devolvemos la condición a ui.R para ocultar los paneles 
              outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
            }
          ) # Fin de la sentencia de control de errores a la hora de generar la gráfica Precision/Recall
          
          #Devolvemos el resultado del control de errores anterior
          return(devuelve)
  
          
        })#output$modelEval_plot2<-renderPlot
        
        par(mfrow = c(1, 1))
        
        #Guardamos una salida out 
        output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"TRUE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
        
        
        }else{ #En caso de que el atributo de valoraciones no sea numérico.
          output$modelEval_msj<-renderText({
           print("Error: el atributo <Valoraciones> no es de tipo numérico.")
          })
          
          output$modelEval_plot1<-renderPlot({})
          output$modelEval_plot2<-renderPlot({})
          
          #Guardamos una salida out 
          output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
          
          #Devolvemos la condición a ui.R para ocultar los paneles 
          outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
        }
        
    },
    warning=function(w){
      output$modelEval_msj<-renderText({
        print("Error: se ha producido un warning al generar los modelos, es posible que los datos de entrada no cumplan con los criterios de calidad necesarios para generar un modelo de recomendaciones.")
      })
      
    },
    error=function(e){
      
      output$modelEval_msj<-renderText({
        print("Error: No se han generado los modelos correctamente, es posible que los datos de entrada no cumplan con los criterios de calidad necesarios para generar un modelo de recomendaciones.")
      })
      output$modelEval_plot1<-renderPlot({})
      output$modelEval_plot2<-renderPlot({})
      
      #Guardamos una salida out 
      output$salidaOKEvalRecomen <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKEvalRecomen", suspendWhenHidden = FALSE)
    }
  )#out<-tryCatch
    
    #devolvemos la salida general
    return(out)
    
  })
  
  
  #--------Recomendaciones
  
  observe({
    
    #Lo que se observa
    cambio1<-input$RedDisp_Guardar
    cambio2<-input$RedDisp_Reset
    
    
    #El fichero que cogemos para renderizar los combos
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }
    if (is.null(df)) return(NULL)
      
    output$colaborativo_at1 <- renderUI({
  
      items=names(df)
      selectInput("colaborativo_at1", "Usuarios:",items)
      
    })
    
    output$colaborativo_at2 <- renderUI({
  
      items=names(df)
      selectInput("colaborativo_at2", "Items:",items)
      
    })
    
    output$colaborativo_at3 <- renderUI({
  
      items=names(df)
      selectInput("colaborativo_at3", "Valoraciones:",items)
      
    })

  })
  
  #Combo donde se renderizan los usuarios a escoger
  observe({
    
    #Variable que se observa (usuario)
    cambio<-input$colaborativo_at1
  
    #El fichero que cogemos para renderizar los combos
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }
    if (is.null(df)) return(NULL)
    
    output$colaborativo_at4 <- renderUI({
 
      items=df[,input$colaborativo_at1]
      selectInput("colaborativo_at4", "Usuario a recomendar:",items)
      
    })
  
  })
  
  Funcion_emiteRecomendaciones<-function(df){
    
    if (class(df[,input$colaborativo_at3])=="numeric" || class(df[,input$colaborativo_at3])=="integer"){
      
    #Se genera la matriz a partir del dataframe
    matriz<-acast(df, df[,input$colaborativo_at1]~df[,input$colaborativo_at2], value.var=input$colaborativo_at3)
    
    #Convertimos la matriz y hacemos de r una variable global para qe pueda ser usada por el resto de funciones
    r <- as(matriz,"realRatingMatrix")
    
    #Obtenemos la posición en la matriz, del usuario de análisis
    pos<-which(rownames(r) ==input$colaborativo_at4, arr.ind = T)
    
    #Almacenamos el número de filas de la matriz
    lim<-nrow(r)
    
    #Definimos lel modelo de recomendación
    if(pos>1){
      
      modelo <- Recommender(r[c(1:(pos-1),(pos+1):lim)], method = input$colaborativo_metodo, param = list(method = input$colaborativo_distancia))
    
    }else{
      
      modelo <- Recommender(r[c(2:lim)], method = input$colaborativo_metodo, param = list(method = input$colaborativo_distancia))
      
    }
    #Predecimos las Top-N recomendaciones
    recomendaciones <- predict(modelo, r[pos], n=input$colaborativo_slider)
    
    #Forecasting de valoraciones sobre los items
    valoraciones<- predict(modelo, r[pos], n=input$colaborativo_slider,type="ratings")
    
    resultado<-list(recomendaciones,valoraciones)
    
    }else{
      return(NULL)
      }
  }
  

  
  #Observamos el evento de acción y mostramos los resultados
  observeEvent(input$colaborativo_Recomen, {
    
    if(SeHaAfinado==FALSE){
      df<-filedata()
    }else{
      df<-df_Reco_afinado
    }

    if (is.null(df)) return(NULL)
    
    #Nos asegurados que se ha indicado un usuario a recomendar
    if (input$colaborativo_at4!=""){

  
      datos<-Funcion_emiteRecomendaciones(df)
      
      if (!is.null(datos)){
        
      
      #Devolvemos las recomendaciones en una tabla
      recomendaciones<-as.data.frame(as(datos[[1]],"list"))
      valoraciones<-as.data.frame(as(datos[[2]],"matrix"))
      
      if (nrow(recomendaciones)==0){
        
        output$colaborativo_msj = renderText({print("Error: la matriz de valoraciones no cumple los criterios adecuados para generar un modelo de recomendaciones. Chequear los datos de entrada.")})
        output$colaborativo_table = renderTable({})
        
        #Guardamos una salida out 
        output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"FALSE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKrecomendarAction", suspendWhenHidden = FALSE)
        
        #Nos salimos de l ejecución actual
        return(NULL)
        
      }
      
      #Buscamos los valores predichos en la matriz
      salida<-recomendaciones
      for(i in 1:nrow(recomendaciones)){
        
        posicionCol<-which(colnames(valoraciones) == recomendaciones[i,1])
        posicionFil<-which(rownames(valoraciones) == input$colaborativo_at4)
        prediccion<-valoraciones[posicionFil,posicionCol]
        salida[i,2]<-prediccion
      }
      #Renombramos las columnas de la tabla
      colnames(salida)<-c("Items Recomendados","Valoraciones Predichas")
      
      output$colaborativo_msj = renderText({print(paste("Se muestran ",isolate(input$colaborativo_slider)," recomendaciones para el usuario ",isolate(input$colaborativo_at4),".",sep=""))})
      
      output$colaborativo_table = renderTable({salida})
      
      #Guardamos una salida out 
      output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKrecomendarAction", suspendWhenHidden = FALSE)
      
      }else{
        output$colaborativo_msj = renderText({print("Error: el atributo Valoraciones no es de tipo numérico.")})
        output$colaborativo_table = renderTable({})
        
        #Guardamos una salida out 
        output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"FALSE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKrecomendarAction", suspendWhenHidden = FALSE)
        
        
      }
    
    }else{
      
      output$colaborativo_msj = renderText({print("Error: se debe seleccionar un usuario a recomendar.")})
      output$colaborativo_table = renderTable({})
      
      #Guardamos una salida out 
      output$salidaOKrecomendarAction <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKrecomendarAction", suspendWhenHidden = FALSE)
      
    }
  })
  
  
  #-------SERIES TEMPORALES---------
  #------------ARIMA----------------
  
  #Renderizar combos
  output$arima_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("arima_at1", "Atributo contable:",items)
    
  })
  
  #Renderizar combos
  output$tbats_at1 <- renderUI({
    df<-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    selectInput("tbats_at1", "Atributo contable:",items)
    
  })

  #Función que se ejecuta al pulsar el botón de acción
  Funcion_arimaModelCondicion<-function(df) {
    
    if (is.null(df)) return(NULL)
    
    #Comprobamos si cumplen la condición para ser as.Date
    listado<-lapply(df[,1], function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d"))))
    nFalsos<-length(grep("FALSE",listado))
    if(nFalsos==0){
      condicion<-TRUE
    }else{
      listado2<-lapply(df[,1], function(x) !all(is.na(as.Date(as.character(x),format="%d-%m-%Y"))))
      nFalsos2<-length(grep("FALSE",listado2))
      if(nFalsos2==0){
        condicion<-TRUE
      }else{
        condicion<-FALSE
      }
    }
  }
  
  
  Funcion_arimaTabla<-function(condicion,df, atributo){
    #Si los valores de la primera columna del dataset son aptos (condicion<-TRUE), continuamos. Si no mostramos msj de error
    if (condicion==TRUE){
      #obtenemos el primer año del dataset
      annoComienzo<-year(df[1,1])
      
      #Renombramos el atributo de cuenta para agrupar
      indice<-grep(atributo,colnames(df))
      colnames(df)[indice]<-"Cuenta_Mensual"
      
      #Agrupamos mensualmente
      usuarios_mensuales <- as.data.frame(df %>%
                                            group_by(anno = year(df[,1]),
                                                     mes = month(df[,1])) %>%
                                            summarise(usuarios = sum(Cuenta_Mensual)))
      
      #Generamos la serie temporal con la librería ts
      usuarios <- usuarios_mensuales[, 3]
      mensual <- ts(usuarios, frequency = 12, start = c(annoComienzo, 1))
      
    }
  }
  
  
  #Evento que ejecuta la función anterior
  observeEvent(input$arima_predAction, {
    df<-filedata()
    if (is.null(df)) return(NULL)
    
    #Comprobamos si el atributo de agrupamiento es numérico
    if (!is.numeric(df[,input$arima_at1])){
      
      output$arima_msj <- renderText({
        print("Error: El atributo contable escogido no es numérico.")
      })
      
      #REiniciamos la tabla gráfica temporal
      output$arima_print1 <- renderPrint({invisible()})
      
      #Guardamos una salida out 
      output$salidaOKARIMA <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKARIMA", suspendWhenHidden = FALSE)
      
      return(NULL)
    }
    
    #Verificamos si el campo fecha cumple la condición para generar una serie temporal
    condicion<-Funcion_arimaModelCondicion(df)
    
    if (condicion=="TRUE"){
      
      mensual<<-Funcion_arimaTabla(condicion,df,input$arima_at1)
      #Print de la representación tabular de la tabla temporal generada
      output$arima_print1 <- renderPrint({
        mensual
      })
      
      #Mensaje de ejecución
      output$arima_msj <- renderText({
        print("Se muestran los resultados de la conversión de la serie temporal.")
      })
      
      #Plot de las gráficas de descomposición en componentes
      output$arima_plot1 <- renderPlot(height = 600,{
        plot(decompose(mensual))
                                      
      })
      
      #Plot de las gráficas diff
      output$arima_plot2 <- renderPlot({
        par(mfrow = c(1, 3))
        plot(mensual, ylim = c(-30000, max(mensual)))
        plot(diff(mensual), ylim = c(-30000, max(mensual)))
        plot(diff(diff(mensual), lag = 12), ylim = c(-30000, max(mensual)))
        par(mfrow = c(1, 1))
      })
      
      #Plot de las gráficas ACF y PACF
      output$arima_plot3 <- renderPlot({
        par(mfrow = c(1, 2))
        acf(mensual, xlim = c(0, 2))
        pacf(mensual, xlim = c(0, 2))
      })
      
      #Guardamos una salida out 
      output$salidaOKARIMA <- reactive({valorDevuelto<-"TRUE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKARIMA", suspendWhenHidden = FALSE)
      
    }else{
      
      output$arima_msj <- renderText({
        print("Error: el dataset no cumple las condiciones para ser transformado en una serie temporal.")
      })
      
      #Guardamos una salida out 
      output$salidaOKARIMA <- reactive({valorDevuelto<-"FALSE"})
      
      #Devolvemos la condición a ui.R para ocultar los paneles 
      outputOptions(output, "salidaOKARIMA", suspendWhenHidden = FALSE)
    }
    
  })
  
  #Evento que genera el modelo
  observeEvent(input$arima_generModelArima, {
    df<-filedata()
    if (is.null(df)) return(NULL)

    modeloNoEsta=as.numeric(c(input$modeloNoEsta_p,input$modeloNoEsta_d,input$modeloNoEsta_q))
    modeloEsta=as.numeric(c(input$modeloEsta_P,input$modeloEsta_D,input$modeloEsta_Q))
    
    modeloARIMA <<- arima(mensual, modeloNoEsta,
                  seasonal = list(order = modeloEsta))
    
    #Mensaje de salida
    output$arima_msj2 <- renderText({
      print("Se muestran las gráficas de modelo ARIMA generado.")
    })
    
    #Plot del modelo ARIMA generado
    output$arima_plot4 <- renderPlot({
      tsdiag(modeloARIMA)
    })

  })
  
  #Plot de la predicción 
  observeEvent(input$arima_predicAction,{

    #Predicción gráfica
    output$arima_plot5<-renderPlot({
      prediccion<-forecast(modeloARIMA,h=input$slider_predarima)
      plot(prediccion)
      
    })
  })
  
  
    #Usando el método TBATS
    observeEvent(input$tbats_predAction,{
      
      df<-filedata()
      if (is.null(df)) return(NULL)
      
      
      #Comprobamos si el atributo de agrupamiento es numérico
      if (!is.numeric(df[,input$tbats_at1])){
        
        output$tbats_msj <- renderText({
          print("Error: El atributo contable escogido no es numérico.")
        })
        
        #Guardamos una salida out 
        output$salidaOKTBATS <- reactive({valorDevuelto<-"FALSE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKTBATS", suspendWhenHidden = FALSE)
        
        return(NULL)
      }
      
      #Verificamos si el campo fecha cumple la condición para generar una serie temporal
      condicion<-Funcion_arimaModelCondicion(df)
      
      if (condicion=="TRUE"){
      
      #obtenemos el primer año del dataset
      annoComienzo<-year(df[1,1])
      
      #Recogemos los años que hay en el dataset
      numAnnos<-unique(year(df[,1]))
      
      #Renombramos el atributo de cuenta para agrupar
      indice<-grep(input$tbats_at1,colnames(df))
      colnames(df)[indice]<-"Cuenta_Mensual"
      
      #Agrupamos mensualmente
      usuarios_mensuales <- as.data.frame(df %>%
                                            group_by(anno = year(df[,1]),
                                                     mes = month(df[,1])) %>%
                                            summarise(usuarios = sum(Cuenta_Mensual)))
      
      usuarios <- usuarios_mensuales[, 3]
      
        mensual<<-Funcion_arimaTabla(condicion,df,input$tbats_at1)
        

        output$tbats_plot1<-renderPlot({
          

          #Modelo avanzado
          modeloAvd <- tbats(mensual)
          predAvan<-forecast(modeloAvd,h=input$slider_predtbats)
          plot(predAvan)
          
          summary(predAvan$mean)
          summary(predAvan$upper)
          summary(predAvan$lower)
          
          #Recorremos todos los años del dataset para pintar la gráfica
          for (annoi in numAnnos){
            assign(paste("media_",annoi,sep=""),round(as.numeric(
                 filter(usuarios_mensuales, anno == annoi) %>%
                   summarise(mean = mean(usuarios))), 0))
            
            mediaAnno<-eval(as.name(paste("media_",annoi,sep="")))
            
            segments(as.numeric(annoi), mediaAnno, x1 = as.numeric(annoi)+1, y1 = mediaAnno,
                              col = "darkgray", lty = 2, lwd = 2)
            
            text(as.numeric(annoi), mediaAnno + mediaAnno*0.05, mediaAnno)
            
          }
          
          #Dibujamos la línea media del pronóstico
          abline(h = max(predAvan$mean), lty = 2, col = "blue")
          
          #Obtenemos el último año de los datos del dataset
          ultiAnno<-tail(numAnnos,n=1)
          
          #Definimos el proximo año a pronosticar
          annoPronos<-as.numeric(ultiAnno)+1
          annoPronosString<-as.character(annoPronos)
          
          #Le asignamos la media del pronóstico TBATS
          assign(paste("media",annoPronosString,sep=""),round(mean(predAvan$mean), 0))
          assign(paste("max_media",annoPronosString,sep=""),round(max(predAvan$mean), 0))
          
          #Dibujamos el segmento del pronóstico
          segments(annoPronos, eval(as.name(paste("media",annoPronosString,sep=""))), x1 = annoPronos+1, y1 = eval(as.name(paste("media",annoPronosString,sep=""))),
                             col = "blue", lty = 2, lwd = 2)
          
          #Pintamos el texto del año pronosticado
          text(annoPronos, eval(as.name(paste("media",annoPronosString,sep=""))) + eval(as.name(paste("media",annoPronosString,sep="")))*0.05, eval(as.name(paste("media",annoPronosString,sep=""))))
          text(annoPronos, eval(as.name(paste("max_media",annoPronosString,sep=""))) + eval(as.name(paste("max_media",annoPronosString,sep="")))*0.05, eval(as.name(paste("max_media",annoPronosString,sep=""))))

        })
        
    
      
        #Escribimos el mensaje por pantalla
        output$tbats_msj<-renderText({
          print("Se muestra el pronóstico proyectado mediante el uso la función TBATS.")
        })
        
        #Guardamos una salida out 
        output$salidaOKTBATS <- reactive({valorDevuelto<-"TRUE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKTBATS", suspendWhenHidden = FALSE)
        
        
      }else{
        
        #Escribimos el mensaje por pantalla
        output$tbats_msj<-renderText({
          print("Error: el dataset no cumple las condiciones para ser transformado en una serie temporal.")
        })
        
        #Guardamos una salida out 
        output$salidaOKTBATS <- reactive({valorDevuelto<-"FALSE"})
        
        #Devolvemos la condición a ui.R para ocultar los paneles 
        outputOptions(output, "salidaOKTBATS", suspendWhenHidden = FALSE)
      }
    })
    
  
  
  #--------------------VISUALIZACIONES-----------------------
  
  #Renderiza los combos en funcion de los datos del archivo
  output$visual_at1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("visual_at1", "Valor X:",choices=c("",items))
    
  })
  
  output$visual_at2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("visual_at2", "Valor Y:",choices=c("",items))
    
  })
  
  output$visual_factor <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("visual_factor", "Atributo de Agrupación",choices=c("",items))
    
  })
  
  output$visual_color <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("visual_color", "Atributo de Densidad",choices=c("",items))
    
  })
  
  
  esfactorvisual<-"NULL"
  
  
  #En caso de que no sea de tipo caracter la variable seleccionada para la densidad del color
  #La factorizamos
  
 output$variableColor<-reactive({
    
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Se observa sólo en caso de que contenga datos el combo
    if (typeof(input$visual_color)!="NULL" && input$visual_color!=""){
      if (class(df[,input$visual_color])!="character" && class(df[,input$visual_color])!="factor" && input$visual_color!="NULL"){
        esfactorvisual<<-"TRUE" #TRUE
        }else{
          esfactorvisual<<-"FALSE"
        }
    }
  })
  outputOptions(output, 'variableColor', suspendWhenHidden = FALSE)
  
#Visualizaciones gráficas
  visuales<-reactive({
    df<-filedata()
    if (is.null(df)) return(NULL)
    
    tipovisualizacion<-input$tipoVisual
    ngrupos<-input$visual_grupos
    
    if (input$visual_at1!="" && input$visual_at2!="" && input$visual_grupos>1 && input$visual_colorIntervalos>1){
    #if(tipovisualizacion=="puntoscolores"){
      
    indiceAt1<-grep(input$visual_at1,colnames(df))
    indiceAt2<-grep(input$visual_at2,colnames(df))
    
    #Generamos una nueva columna con los grupos a mostrar, de acuerdo a la variable a agrupar (nº degraficas)
    if (class(df[,input$visual_factor])!="factor"){
      df$emp_size <- cut(df[,input$visual_factor],breaks = ngrupos)
      
      #Guardamos una salida out 
      output$salidaOKFactorAgrupacion <- reactive({valorDevuelto<-"TRUE"})

    }else{
      df$emp_size<-df[,input$visual_factor]
      
      #Guardamos una salida out 
      output$salidaOKFactorAgrupacion <- reactive({valorDevuelto<-"FALSE"})
    }
    
    #Devolvemos la condición a ui.R para ocultar los paneles 
    outputOptions(output, "salidaOKFactorAgrupacion", suspendWhenHidden = FALSE)                

    #En caso de que sea necesario factorizar la variable continua en grupos
    if(esfactorvisual=="TRUE"){
      df[,paste(input$visual_color,"_factor_",input$visual_colorIntervalos,sep="")]<-cut(df[,input$visual_color],as.numeric(input$visual_colorIntervalos))
      
    
    
        plot <- ggplot(data = df, aes(x = df[,input$visual_at1],
                                            y = df[,input$visual_at2]))
        plot <- plot + facet_grid(. ~ emp_size) + 
          geom_point(aes(color = df[,paste(input$visual_color,"_factor_",input$visual_colorIntervalos,sep="")]), shape = 18, size = 4)
       
        plot + 
            scale_color_discrete(guide = guide_legend(title = paste0(input$visual_color,"\nDensity"))) +
            xlab(input$visual_at1) + ylab(input$visual_at2) 
    }else{
      plot <- ggplot(data = df, aes_string(colnames(df)[indiceAt1], colnames(df)[indiceAt2]))
      
      plot <- plot + facet_grid(. ~ emp_size) + 
      geom_point(aes(color = df[,input$visual_color]), shape = 18, size = 4)
      
       plot + 
         scale_color_discrete(guide = guide_legend(title = paste0(input$visual_color,"\nDensity"))) +
         xlab(input$visual_at1) + ylab(input$visual_at2) 
        

        }
     
    }
  })  
    
    output$visual_plot1<-renderPlot({
      visuales()
    })

    
    #-----Incorporamos la GEOlOCALIZACIÓN------------
    
    
    observeEvent(input$geo_action,{
      df<-filedata()
      if (is.null(df)) return(NULL)
      
      #Comprobamos si hay longitud y latitud en el datset
      if (length(grep("lati.*",names(df)))!=0 && length(grep("long.*",names(df)))!=0){
        esgeolocalizable<-TRUE
        latitud<-grep("lati.*",names(df),value=TRUE)
        longitud<-grep("long.*",names(df),value=TRUE)
      }else{
        esgeolocalizable<-FALSE
        }
      
      if(esgeolocalizable==TRUE){
        
       
          
          #Añadimos el pop-up
          df$popup <- paste0("Localización #",
                                 seq(1, nrow(df))," ","Lon:",df[,longitud]," ","Lat:",df[,latitud])
          #Dibujamos el mapa
          output$geo_plot<-renderLeaflet({
          leaflet() %>%
                      addTiles() %>%
                      addMarkers(data = df, ~as.numeric(df[,longitud]), ~as.numeric(df[,latitud]),
                                 popup = ~popup)
          })
        
        output$geo_msj<-renderText({
          print("Se muestra la localización de los datos.")
        })
 
      }else{
        
        output$geo_msj<-renderText({
          print("Error: no hay datos de geolocalización en el dataset.")
        })
        
        #Reiniciamos el mapa de cooordenadas
        output$geo_plot<-renderLeaflet({ })
        
      }
      
    })
    
    #--Ruta Optima---------
    
    observeEvent(input$ruta_action,{
      df<-filedata()
      if (is.null(df)) return(NULL)
      
      #Comprobamos si hay longitud y latitud en el datset
      if (length(grep("lati.*",names(df)))!=0 && length(grep("long.*",names(df)))!=0){
        esgeolocalizable<-TRUE
        latitud<-grep("lati.*",names(df),value=TRUE)
        longitud<-grep("long.*",names(df),value=TRUE)
      }else{
        esgeolocalizable<-FALSE
      }
      
      if(esgeolocalizable==TRUE){
        
        #Recogemos del dataset las columnas que necesitamos únicamente
        df<-df[,c(latitud,longitud)]
        
        #Por si acaso son caracteres transformamos en tipo numeric
        df[, 1:2] <- sapply(df[, 1:2], as.numeric)
        
        #Optenemos el TSP a través de la distancia
        tsp <- TSP(dist(df))
        tsp <- insert_dummy(tsp, label = "cut")
        tour <- solve_TSP(tsp, method="2-opt", control=list(rep=10))
        path.tsp <- unname(cut_tour(tour, "cut"))
        
        
        #Ordenamos df de acuerdo a la ruta TSP
        df_ordenado<-df[path.tsp,] 
        
        
        #Añadimos el pop-up 
        df_ordenado$popup <- paste0("Localización #",
                           seq(1, nrow(df))," ","Lon:",df[,longitud]," ","Lat:",df[,latitud])
        
        #Dibujamos el mapa
        output$ruta_plot<-renderLeaflet({
        
          leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(data = df_ordenado, lat = ~df_ordenado[,latitud], 
                             lng = ~df_ordenado[,longitud], radius = 3, 
                             popup = ~popup) %>% 
            addPolylines(data =df_ordenado, lat = ~df_ordenado[,latitud], 
                         lng = ~df_ordenado[,longitud], color = "#C94339", 
                         opacity = .7) 
          
        })
        
        output$ruta_msj<-renderText({
          print("Se muestra la ruta optima entre las coordenadas.")
        })
        
      }else{
        
        output$ruta_msj<-renderText({
          print("Error: No hay datos de geolocalización en el dataset.")
        })
        
        #Reiniciamos el mapa de cooordenadas
        output$ruta_plot<-renderLeaflet({ })
        
      }
      
    })
  
    
    #--------BBDD MONGODB----------
    
    #variable de control pàra importar un fichero desde BBDD
    df_imported<-NULL
    
    #Informamos sobre la situación de carga actual en la herramienta
    observe({

      ficherocargando<-input$datafile

      if (is.null(filesalida)){

      #Informamos del archivo que se ha cargado
      output$mongo_msj_archivo<-renderText({
        print("Error: Actualmente no hay cargado ningún archivo.")
      })

    }else{

      #Informamos del archivo que se ha cargado
      output$mongo_msj_archivo<-renderText({
        print(paste("Actualmente se encuentra cargado el archivo: " ,input$datafile[[1]], sep=""))
      })

    }

    })
    
    #En caso de importación del documento
    observeEvent(input$Importar_DocFromBBDD,{
      
      out<-tryCatch(  
        {
        #Iniciamos la conexión con MongoDB
        m <- mongo(collection = input$coleccion, db=input$baseDeDatos,url=input$url_mongo)
        
        #Obtenemos el dataframe de datos
        df_imported <<- m$find()
        
        if (nrow(df_imported)>0){
          
          #Renderizamos la tabla
          output$mongo_table<-renderTable({
            df_imported[1:100,]
          })
          
          output$mongo_msj_action<-renderText({
            print(paste("Se ha importado la colección ",isolate(input$coleccion)," desde la base de datos ",isolate(input$baseDeDatos),".",sep=""))
            
          })
          
          output$mongo_msj_table<-renderText({
            print("Se muestran los 100 primeros registros de la tabla.")
            
          })
        }else{
          
          output$mongo_msj_action<-renderText({
            print("Error: No existe la colección en la base de datos especificada.")
            
          })
        }
        },
        
          warning = function(w) {
            output$mongo_msj_action<-renderText({
            print(paste("Warning: ", "La cadena de conexión especificada no devuelve resultados."))
          })
        
          },
        error = function(e) {
            output$mongo_msj_action<-renderText({
              print(paste("Error: ", "La cadena de conexión especificada no devuelve resultados."))
            })
          }
        )
      return(out)

    
  })
    
    output$importadaColeccionDeMongo<-eventReactive(input$Importar_DocFromBBDD,{
      df_impor <-df_imported
      if (is.null(df_impor)) return(NULL)
      if(nrow(df_impor)==0){retorna<-"FALSE"}else{retorna<-"TRUE"}
    })

    outputOptions(output, 'importadaColeccionDeMongo', suspendWhenHidden = FALSE)


    #En caso de exportación del documento
    observeEvent(input$Exportar_DocToBBDD,{
      
      if (is.null(filesalida)){
        
        output$mongo_msj_action<-renderText({
          print("Error: No se puede realizar esta operación sin haber cargado previamente un archivo.")
        })
        
      }else{ #En el caso de que haya cargado un archivo podemos ejecutar la operación.

        out<-tryCatch(
          {
          
          df<-filedata()
          if (is.null(df)) return(NULL)
          #Iniciamos la conexión con MongoDB
          m <- mongo(collection = input$coleccion, db=input$baseDeDatos,url=input$url_mongo)
          
          #Exportamos el dataframe de datos
          m$insert(df)
          
            output$mongo_msj_action<-renderText({
              print(paste("Se ha exportado la colección ",isolate(input$coleccion)," a la base de datos ",isolate(input$baseDeDatos),".",sep=""))
            })
          
          },
          
          warning = function(w) {
            output$mongo_msj_action<-renderText({
              print(paste("Warning: ", "La cadena de conexión especificada no devuelve resultados."))
            })
          },
          
          error = function(e) {
            output$mongo_msj_action<-renderText({
              print(paste("Error: ", "La cadena de conexión especificada no devuelve resultados."))
            })
           }
        )
          }#else
    })
    
    #Para guardar el archivo Importado desde MongoDB en un CSV
    observe({
      
      volumes <- c("UserFolder"=getwd())
      shinyFileSave(input, "guardarImportFromMongo", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$guardarImportFromMongo)
      data <- df_imported
      if (is.null(data)) return(NULL)
      
      if (nrow(df_imported) > 0) {
        write.csv(data, as.character(fileinfo$datapath),row.names=FALSE)
      }
    })

    
})



