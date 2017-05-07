
shinyServer(function(input, output, session) {

#Cargamos el archivo CSV
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      #Aún no se ha introducido ningún archivo en el editable
      return(NULL)
    }
    #Leemos el archivo y tratamos los '?' como NA
    file<-read.csv(infile$datapath,sep=",",na.strings=c("?",""),stringsAsFactors = FALSE)
    
    output$mensajes_carga <- renderText({
      print("Se muestran los 100 primeros registros del archivo.")
    })
      
    fileout<-file
  })
  
  #Guardamos una salida out para consulta cada vez que se llama a la función filedata()
  output$filedatacargado <- reactive({
    filedata()
  })
  
  #Devolvemos la condición de que el fichero se ha cargado a la variable para consultar desde ui.R <conditionalPanel>
  outputOptions(output, "filedatacargado", suspendWhenHidden = FALSE) 
  
  ####Mensajes de necesario cargar fichero en todas las secciones#####
  
  #Comprobamos si se ha cargado algún fichero en Edición
  output$controlDeCarga_Edicion <- renderText({ 
    df <-filedata()
    if (is.null(df)) return("Para EDITAR es necesario haber cargado un archivo previamente.")
    })
  
  #Comprobamos si se ha cargado algún fichero en Limpieza
  output$controlDeCarga_Limpieza <- renderText({ 
    df <-filedata()
    if (is.null(df)) return("Para LIMPIAR es necesario haber cargado un archivo previamente.")
  })
  
  #Comprobamos si se ha cargado algún fichero en Consulta
  output$controlDeCarga_Consulta <- renderText({ 
    df <-filedata()
    if (is.null(df)) return("Para CONSULTAR es necesario haber cargado un archivo previamente.")
  })
  
  #Comprobamos si se ha cargado algún fichero en Consulta
  output$controlDeCarga_Exploracion1 <- renderText({ 
    df <-filedata()
    if (is.null(df)) return("Para EXPLORAR es necesario haber cargado un archivo previamente.")
  })
  
  ###FIN de los mensajes en las secciones###
  
  #------------------MENÚ DE CONSULTA DE DATOS--------------------------
  
  #Renderizamos los combos en funcion de los datos del archivo
  output$var1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("Variable1", "Variable 1:",items)
    
  })
  

  output$val1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fr=input$Variable1
    items=df[,fr]
    
    selectInput("Valor1", "Valor:",items,selected = NULL)
    
  })
  

  output$var2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
    selectInput("Variable2", "Variable 2:",items)
    
  })
  

  output$val2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fr=input$Variable2
    items=df[,fr]
    
    selectInput("Valor2", "Valor:",items,selected = NULL)
    
  })
  
  
  output$str <- renderPrint({
   df <-filedata()
    if (is.null(df)) return(NULL)

    str(df)
  })
  
  
  #Se visualiza el contenido delarchivo
  output$filetable <- renderTable({
    file<-filedata()
    fileReduced<-file[1:100,]
  })
  
  
  #Renderiza los combos en funcion de los datos del archivo
  output$variables <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    checkboxGroupInput("variablesLista", "Variables del archivo:",items,selected=items,inline=TRUE,width="100%")
    
  })
  
  output$TextoSTR <- renderPrint({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    str(df)
  })
  
  FuncionFiltroColumnas <-eventReactive(input$SeleccionarVariables,{
    file=filedata()
    if (is.null(file)) return(NULL)
    
    file.subset <- file[, input$variablesLista]
    
    var1=input$Variable1
    val1=input$Valor1
    var2=input$Variable2
    val2=input$Valor2

   if(val1!="" && val2=="" ){
      fileout <- file.subset[file.subset[,var1]==val1,]
    }else if(val2!="" && val1!="" ){
      fileout_pre<-file.subset[file.subset[,var1]==val1,]
      fileout<- fileout_pre[fileout_pre[,var2]==val2,]
    }else{
      fileout<-file.subset
    }
    
  })
  
  observeEvent(input$SeleccionarVariables, {
    output$filetablecolumnas <- renderTable({
      FuncionFiltroColumnas()
    })
  })
  
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardarFiltro", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardarFiltro)
    data <- FuncionFiltroColumnas()
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath))
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
      print("Se ejecuta la operación")
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
        print("Registros con valores NA eliminados")
      }else{
        print("Registros con valores NA habian sido previamente eliminados")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$resultados_limpiezaNA <- renderTable({})
  })
  
  #Para guardar el nuevo archivo con la limpieza de vlores NA generado
  # observe({
  #   volumes <- c("UserFolder"=getwd())
  #   shinyFileSave(input, "guardar_limpieza", roots=volumes, session=session)
  #   fileinfo <- parseSavePath(volumes, input$guardar_limpieza)
  #   #Revisamos esta parte, ya que ejecuta si o si...
  #   data <- Funcion_eliminarValoresNA()
  #   if (nrow(fileinfo) > 0) {
  #     write.csv(data, as.character(fileinfo$datapath))
  #   }
  # })
  
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
        print("Aún no se ha realizado ninguna operación sobre el dataset")
      }else{
        #Restauramos para que se pueda volver a eliminar
        eliminadosNA<<-"False"
        print("Se ha restaurado el dataset original")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$resultados_limpiezaNA <- renderTable({})
  })
  
  
  #---Pasamos a la búsqueda de valores anómalos---------
  buscadosvaloreserror<-"False"
  borradosvaloreserror<<-"False"

  output$atributosLimpieza <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)

    items=names(df)
    selectInput("atributosLimpieza", "Atributo a analizar:",items)

  })

  #Función que restaura los valores NA eliminados previamente
  Funcion_buscaValoresErroneos<-eventReactive(input$valoresAnomalos_Buscar,{
    if (eliminadosNA=="True"){
      df<-Funcion_eliminarValoresNA()
    }else{
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
    buscadosvaloreserror<<-"True"
    fileout<-df[location, ]

  })

  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$valoresAnomalos_Buscar, {
    #Evento que dibuja el resultado
    output$resultados_limpiezaAnomalos <- renderTable({Funcion_buscaValoresErroneos()})
    output$mensajes_limpiezaAnomalos <- renderText({

        print("Ejecutada búsqueda de valores anómalos")

    })

  })
  #Control de eliminaciones de registros anómalos
  eliminadosValoresAnomalos<-"False"
  segundaeliminacionAnomala<-"False"
  
  #Función que elimina los valores Anómalos
  Funcion_eliminarValoresAnomalos<-eventReactive(input$valoresAnomalos_Limpiar,{
    #Comprobamos si se han eliminado los valores NA
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
  
  # 
  # #Borramos los valores anómalos ya buscados
  # Funcion_eliminaValoresErroneos<-eventReactive(input$eliminarvaloresAnomalos_Buscar,{
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  #   atributo<-input$atributosLimpieza
  #   if(input$tipoDato_Limpieza=="string"){
  #     subset <- str_subset(as.vector(df[,atributo]), "[a-z A-Z]") 
  #   }else{
  #     subset <- str_subset(as.vector(df[,atributo]), "[0-9]")
  #   }
  #   location <- str_detect(as.vector(df[,atributo]), subset)
  #   buscadosvaloreserror<<-"True"
  #   file1<<-df[-location, ]
  #   file2<-df[location, ] 
  #   fileout<-list(file1,file2)
  # 
  # })
  # 
  # #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  # observeEvent(input$eliminarvaloresAnomalos_Buscar, {
  #   lista<-Funcion_eliminaValoresErroneos()
  #   #Evento que dibuja el resultado
  #   output$resultados_limpieza <- renderTable({lista[2]})
  #   output$mensajes_limpieza <- renderText({
  #     borradosvaloreserror<<-"True"
  #     print("Se han borrado los valores mostrados.")
  #   })
  #   
  # })
  
  
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
      write.csv(data, as.character(fileinfo$datapath))
    }
  })
  #------------------FIN DE TRASFORMACIONES--------------------------
  
  #------------------MENU DE EDICION DE DATOS--------------------------
  
  output$atributosEdicion <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("atributosEdicion", "Atributo Origen:",items)
    
  })
  
  output$otroAtributo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("otroAtributo", "Otro Atributo:",items)
    
  })
  
  
  FuncionAddAtributo <-eventReactive(input$ejecutarAtributo,{
    df=filedata()
    if (is.null(file)) return(NULL)
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
    if (class(df[[input$atributosEdicion]]) == "numeric" && input$factorNumerico!=""){
      #Añadimos la nueva columna con el cálculo
      fileout<-mutate_(df, .dots=setNames(paste0(input$atributosEdicion,oper,input$factorNumerico), input$nuevoAtributo))

    }else if (class(df[[input$atributosEdicion]]) == "numeric" && input$factorNumerico=="" && input$otroAtributo!="") {
      if (class(df[[input$otroAtributo]])=="numeric") {
        fileout<-mutate_(df, .dots=setNames(paste0(input$atributosEdicion,oper,input$otroAtributo), input$nuevoAtributo))
      }else{
      fileout<-paste0("El atributo <",input$otroAtributo,">, no es numérico")
         }
    } else{
      fileout<-paste0("El atributo <",input$atributosEdicion,">, no es numérico")
        }
      })
  
  observeEvent(input$ejecutarAtributo, {
    output$filetabledicion <- renderTable({
      FuncionAddAtributo()
    })

  })
  
  #Botón de guardado
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardar_edicion", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardar_edicion)
    data <- FuncionAddAtributo
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath),row.names=F)
    }
  })

  
####-------EXPLORACIONES---------#########
  
  ####-------Factorización---------#########
  
  ficheroFactorizado<-"False"
  
  #Mostramos el sumario general del dataset
  output$dosvariables_sumarioGeneral <- renderPrint({
    file<-filedata()
    summary(file)
  })
  
  
  #Renderizamos los atributos en 'dosvariables_Ui_atributos'
  output$dosvariables_Ui_atributos <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("dosvariables_Ui_atributos", "Atributo:",items)
    
  })
  
  #Función que factoriza un atributo en un número de intervalos
  Funcion_FactorizarConDosVariables<-eventReactive(input$dosvariables_Action_factorizar,{
    if (ficheroFactorizado=="True"){
      df<-fileout_fact_dosVar
    }else{
      df <-filedata()
      if (is.null(df)) return(NULL)
    }
    atributo<-input$dosvariables_Ui_atributos
    if (class(df[,atributo])=="numeric" || class(df[,atributo])=="integer"){
      df[,paste(atributo,"_factor_",input$dosvariables_TextInput_intervalos,sep="")]<-cut(df[,atributo],as.numeric(input$dosvariables_TextInput_intervalos))
      
      #Mostramos el mensaje
        output$dosvariables_mensajes_factorizar <- renderPrint({
        #Mensaje de ejecución
        print(paste("Se ha factorizado el atributo",isolate(input$dosvariables_Ui_atributos)," en ",input$dosvariables_TextInput_intervalos," intervalos"))
        
      })
      
      #Mostramos los nuevos intervalos
      output$dosvariables_mensajes_print <- renderPrint({
        atributo<-isolate(input$dosvariables_Ui_atributos)
        summary(df[,paste(atributo,"_factor_",isolate(input$dosvariables_TextInput_intervalos),sep="")])
      })
      
    }else{
      #En este caso es que alguna variable es de tipo caracter con lo que hay que usar la estrategia names de columna
      factoresArray<-unique(df[,atributo])
      #arrayList<-strsplit(factoresArray,";")[[1]]
      df[,atributo]<-factor(df[,atributo], ordered=TRUE, levels=factoresArray)
      
      output$dosvariables_mensajes_factorizar <- renderPrint({
        #Mensaje de ejecución
        print(paste("Se ha factorizado el atributo caracter(mejorar descripcion) intervalos"))
        
      })
      
      #Reiniciamos la salida 
      output$dosvariables_mensajes_print <- renderText({ })
      
    }
    
    ficheroFactorizado<<-"True"
    fileout_fact_dosVar<<-df
    
  })
  
  #Ejecutamos la función anterior por evento
  observeEvent(input$dosvariables_Action_factorizar, {
    
    fileout<-Funcion_FactorizarConDosVariables()
    
    if (ficheroFactorizado=="True"){
      
      #Actualizamos el sumario general
      output$dosvariables_sumarioGeneral <- renderPrint({
        summary(fileout)
      })
      
    }else{
      #Mostramos el mensaje de que el atributo no es numérico
      output$dosvariables_mensajes_factorizar <- renderPrint({
        #Mensaje de ejecución
        print(paste("El atributo ",isolate(input$dosvariables_Ui_atributos)," no es numérico",sep=""))
        
      })
      #Reiniciamos la salida 
      output$dosvariables_mensajes_print <- renderPrint({ })
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
  
  # #Variables y datos iniciales
  # factorizadoSiNo<-"False"
  # 
  # #Factorización
  # output$atributosCambioDeTipos <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  #   
  #   items=names(df)
  #   selectInput("atributosCambioDeTipos", "Atributo:",items)
  #   
  # })
  # 
  # #Realizar la factorización
  # Funcion_Factorizar<-eventReactive(input$ejecutarFactorizacion,{
  #   #Comprobamos si es la primera factorización o no.
  #   if (factorizadoSiNo=="False"){
  #   df <-filedata()
  #   }else{
  #     df<-fileout_factorizado
  #   }
  #   if (is.null(df)) return(NULL)
  #   factoresArray<-input$factores 
  #   arrayList<-strsplit(factoresArray,";")[[1]]
  #   df[,input$atributosCambioDeTipos]<-factor(df[,input$atributosCambioDeTipos], ordered=TRUE, levels=arrayList)
  #   #df$pop_density<-factor(df$pop_density, ordered=TRUE, levels=c("Low","Medium","High"))
  #   
  #   factorizadoSiNo<<-"True"
  #   fileout_factorizado<<-df
  # })
  # 
  # #Ejecutamos la función anterior por evento
  # observeEvent(input$ejecutarFactorizacion, {
  #   output$edicion_print <- renderPrint({
  #     str(Funcion_Factorizar())
  #   })
  #   
  #   output$mensajes_factorizar <- renderPrint({
  #     print(paste("Se ha factorizado el atributo",isolate(input$atributosCambioDeTipos)))
  #   })
  #     
  # })
  
  #------EXPLORACIÓN DE UNA VARIABLE---------
  
  #Variables a explorar
  output$atributoUnaVariable <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("atributoUnaVariable", "Atributo:",items)
    
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
           sumario={
              fileout<-summary(df[,input$atributoUnaVariable])
           },
           media={
             fileout<-mean(df[,input$atributoUnaVariable])
           },
           desviacion={
             fileout<-sd(df[,input$atributoUnaVariable])   
           },
           varianza={
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
      print(paste("Se muestra: ",isolate(input$tipoExploracion1)," del atributo ",isolate(input$atributoUnaVariable)))
    })

  })


  
  #--Exploraciones Gráficas---------
  
  #inicializamos el combo
  output$atributoUnaVariableGrafica <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("atributoUnaVariableGrafica", "Atributo:",items)
    
  })
  
  #En caso de evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoUnaVariableGrafica <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
    })
  
  })
  
  #RESET de la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoUnaVariableGrafica <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
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
               if (class(df[,input$atributoUnaVariableGrafica])=="numeric"){
                      hist(df[,input$atributoUnaVariableGrafica],main = "Histograma", xlab=input$atributoUnaVariableGrafica)
                 #Mensaje positivo
                      output$mensajes_exploracionGrafica <- renderText(
                      print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
                      )
                 
                 }else{
                   #Mensaje de error
                   output$mensajes_exploracionGrafica <- renderText(
                     print(paste("El atributo: ",input$atributoUnaVariableGrafica," no es numérico o necesita ser factorizado."))
                   )
                  }
             },
             caja={
                     boxplot(df[,input$atributoUnaVariableGrafica],main = "Diagrama de Caja", xlab=input$atributoUnaVariableGrafica)
                #Mensaje positivo 
                     output$mensajes_exploracionGrafica <- renderText(
                      print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
               )
               },
             plot={
                     plot(df[,input$atributoUnaVariableGrafica],main = "Plot", xlab=input$atributoUnaVariableGrafica, ylab="Valores")   
               #Mensaje positivo
                      output$mensajes_exploracionGrafica <- renderText(
                      print(paste("Gráfica 1: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica))
               )
               }
          )
      }else{
        #Mensaje de error
        output$mensajes_exploracionGrafica <- renderText(
          print(paste("El atributo: ",input$atributoUnaVariableGrafica," no es numérico o necesita ser factorizado."))
        )
      }
      
    })
    
  })
  
  #Segunda ventana gráfica
  output$atributoUnaVariableGrafica2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    selectInput("atributoUnaVariableGrafica2", "Atributo:",items)
    
  })
  
  #En caso de evento de factorización
  observeEvent(input$dosvariables_Action_factorizar,{
    
    output$atributoUnaVariableGrafica2 <- renderUI({
      df <-fileout_fact_dosVar
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
    })
    
  })
  
  #RESET de la factorización
  observeEvent(input$dosvariables_Action_factoReset,{
    
    output$atributoUnaVariableGrafica2 <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("atributoUnaVariable", "Atributo:",items)
      
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
                 if (class(df[,input$atributoUnaVariableGrafica2])=="numeric"){
                   hist(df[,input$atributoUnaVariableGrafica2],main = "Histograma", xlab=input$atributoUnaVariableGrafica2)
                   #Mensaje positivo
                   output$mensajes_exploracionGrafica2 <- renderText(
                     print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
                   )
                   
                 }else{
                   #Mensaje de error
                   output$mensajes_exploracionGrafica2 <- renderText(
                     print(paste("El atributo: ",input$atributoUnaVariableGrafica2," no es numérico o necesita ser factorizado."))
                   )
                 }
               },
               caja={
                 boxplot(df[,input$atributoUnaVariableGrafica2],main = "Diagrama de Caja", xlab=input$atributoUnaVariableGrafica2)
                 #Mensaje positivo 
                 output$mensajes_exploracionGrafic2a <- renderText(
                   print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
                 )
               },
               plot={
                 plot(df[,input$atributoUnaVariableGrafica2],main = "Plot", xlab=input$atributoUnaVariableGrafica2, ylab="Valores")   
                 #Mensaje positivo
                 output$mensajes_exploracionGrafica2 <- renderText(
                   print(paste("Gráfica 2: ",input$tipoExploracionGrafica1," del atributo ",input$atributoUnaVariableGrafica2))
                 )
               }
        )
      }else{
        #Mensaje de error
        output$mensajes_exploracionGrafica2 <- renderText(
          print(paste("El atributo: ",input$atributoUnaVariableGrafica2," no es numérico o necesita ser factorizado."))
        )
      }
      
    })
    
  })
  
  #-------EXPLORACIÓN DE DOS VARIABLES---------------
  
  
  #--Relacion tabular entre dos variables--
  
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
    
    
  # Funcion_relacionTabDosVariables<-eventReactive(input$dosvariables_Action_relacionTab,{
  #   at1=input$dosvariables_Ui_rela_at1
  #   at2=input$dosvariables_Ui_rela_at2
  #   
  #   if (ficheroFactorizado=="False"){
  #     df <-filedata()
  #   }else{
  #     #El fichero que deja la función de factorización en intervalos
  #     df<-Funcion_FactorizarConDosVariables()
  #   }
  #   if (is.null(df)) return(NULL)
  #   table(df[,at1],df[,at2])
  #   
  #     
  # })
  # 
  # observeEvent(input$dosvariables_Action_relacionTab, {
  #   output$dosvariables_Print_relacionTab <- renderPrint({
  #     fileout<-Funcion_relacionTabDosVariables()
  #   })
  # })
  
  #---Realizamos la relación gráfica de dos variables conjuntamente
  
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
  
  #Cambiamos los valores de los combos si se produce el evento
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
      
      if(class(df[,at1])=="factor" && class(df[,at2])=="factor"){
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Gráfica Factor/ Factor")
        })
        
        mosaicplot(table(df[,at1],df[,at2]), col=c("gray","black"))
        
      }else if((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")) {
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Gráfica Numeral / Numeral")
        })
        
        plot(df[,at1],df[,at2])
        
      }else if (class(df[,at1])!="character" && class(df[,at2])=="factor" ){
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Se muestra el gráfico de Numeral / Factor")
        })
        
        boxplot(df[,at1] ~  df[,at2], main="Factor/Numeral")
        
      }else if (class(df[,at1])=="factor" && class(df[,at2])=="numeric"){
         
         output$mensajes_dosvar_exploracionGrafica <- renderPrint({
            print("El atributo tipo factor debe asignarse a la variable Atributo2 para poder mostrar el diagrama de caja")
          })
          
        }else{
          output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Alguno de los atributos es de tipo caracter y no puede ser relacionado gráficamente")
            })
        }

    })

  }) #Fin del Observe
  
  #----CORRELACIONES ENTRE DOS VARIABLES-----#####
  
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
        
        output$dosvar_msj_correlacion <- renderPrint({
          print("Se muestran los resultados del test de correlación")
        })
        
      }else{
        output$dosvar_msj_correlacion <- renderPrint({
          print("Alguno de los atributos no es numérico, no se puede ejecutar el test de correlación")
        })
        
      }
  })
  
  
  
  
})
