
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
  
  # #Renderizamos los combos en funcion de los datos del archivo
  # output$var1 <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  # 
  #   items=names(df)
  #   selectInput("Variable1", "Variable 1:",items)
  # 
  # })
  # 
  # 
  # output$val1 <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  #   fr=input$Variable1
  #   items=df[,fr]
  # 
  #   selectInput("Valor1", "Valor:",choices=c("",items))
  # 
  # })
  # 
  # 
  # output$var2 <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  # 
  #   items=names(df)
  #   #names(items)=items
  #   selectInput("Variable2", "Variable 2:",items)
  # 
  # })
  # 
  # 
  # output$val2 <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  #   fr=input$Variable2
  #   items=df[,fr]
  # 
  #   selectInput("Valor2", "Valor:",choices=c("",items))
  # 
  # })
  # 
  #Los combos los atributos deben actualizarse de acuerdo a los checkbox seleccionados
  observeEvent(input$variablesLista,{
    file<- filedata()
    file.subset <- file[, input$variablesLista]
    
    output$var1 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      
      items=names(df)
      selectInput("Variable1", "Variable 1:",items)
      
    })
    
    
    output$val1 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      fr=input$Variable1
      items=df[,fr]
      
      selectInput("Valor1", "Valor:",choices=c("",items))
      
    })
    
    output$var2 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      
      items=names(df)
      #names(items)=items
      selectInput("Variable2", "Variable 2:",items)
      
    })
    
    
    output$val2 <- renderUI({
      df <-file.subset
      if (is.null(df)) return(NULL)
      fr=input$Variable2
      items=df[,fr]
      
      selectInput("Valor2", "Valor:",choices=c("",items))
      
    })
    
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
    checkboxGroupInput("variablesLista", "Atributos del Dataset:",items,selected=items, inline = TRUE)
    
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
    
    #Necesitamos incorporar un mensaje que diga que se necesitan al menos dos columnas seleccionadas
   if (length(input$variablesLista)>1){
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
     
     
   }else{
     return(NULL)
   }
   
    
  })
  
  observeEvent(input$SeleccionarVariables, {
    
    filtroCol<-FuncionFiltroColumnas()
    if(is.null(filtroCol)){
      output$consulta_msj <- renderText({
        print("Se debe seleccionar más de un atributo para consultar.")
      })
      #Reiniciamos la tabla
      output$filetablecolumnas <- renderTable({})
      
    }else{
      output$filetablecolumnas <- renderTable({
        filtroCol
      })
      output$consulta_msj <- renderText({
        print("Se muestran los resultados de la consulta.")
      })
    }
    
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
  

  #---------------Para dibujar en UI.R el cuadro de intervalos---------

    output$intervalos<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      atributo<-input$dosvariables_Ui_atributos
      if(class(df[,atributo])=="numeric"){interv<-"TRUE"}else{inter<-"FALSE"}
    })
    outputOptions(output, 'intervalos', suspendWhenHidden = FALSE)

  #---------------Fin para dibujar en UI.R el cuadro de intervalos---------
  
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
      #df[,atributo]<-factor(df[,atributo], ordered=TRUE, levels=factoresArray)
      df[,atributo]<-factor(df[,atributo], levels=sort(factoresArray))
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
               if (class(df[,input$atributoUnaVariableGrafica])=="numeric" || class(df[,input$atributoUnaVariableGrafica])=="integer"){
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
                   print(paste("Gráfica 2: ",input$tipoExploracionGrafica2," del atributo ",input$atributoUnaVariableGrafica2))
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
      
      if( (class(df[,at1][[1]])=="factor" || class(df[,at1][[1]])=="ordered")   && (class(df[,at2][[1]])=="factor" || class(df[,at2][[1]])=="ordered")){
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Gráfica Factor/ Factor")
        })
        
        mosaicplot(table(df[,at1],df[,at2]), col=c("gray","black"))
        
      }else if((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")) {
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Gráfica Numeral / Numeral")
        })
        
        plot(df[,at1],df[,at2])
        
      }else if (class(df[,at1])!="character" && (class(df[,at2][[1]])=="factor" || class(df[,at2][[1]])=="ordered")){
        
        output$mensajes_dosvar_exploracionGrafica <- renderPrint({
          print("Se muestra el gráfico de Numeral / Factor")
        })
        
        boxplot(df[,at1] ~  df[,at2], main="Factor/Numeral")
        
      }else if ((class(df[,at1][[1]])=="factor" || class(df[,at1][[1]])=="ordered") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")){
         
         output$mensajes_dosvar_exploracionGrafica <- renderPrint({
            print("El atributo tipo factor debe asignarse a la variable Atributo 2, para poder mostrar el diagrama de caja")
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
  
#---ANÁLISIS MULTIVARIABLE--------
  
  #--Análisis gráfico-----
  
  # #Inicializamos los combos
  # output$multivariable_Ui_gra_at1 <- renderUI({
  #   df<-filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   selectInput("multivariable_Ui_gra_at1", "Atributo 1:",items)
  #   
  # })
  # 
  # 
  # #En caso de evento de factorización
  # observeEvent(input$dosvariables_Action_factorizar,{
  #   
  #   output$multivariable_Ui_gra_at1 <- renderUI({
  #     df<-fileout_fact_dosVar
  #     if (is.null(df)) return(NULL)
  #     items=names(df)
  #     selectInput("multivariable_Ui_gra_at1", "Atributo 1:",items)
  #     
  #   })
  #   
  # })
  # 
  # #RESET de la factorización
  # observeEvent(input$dosvariables_Action_factoReset,{
  #   
  #   output$multivariable_Ui_gra_at1 <- renderUI({
  #     df<-filedata()
  #     if (is.null(df)) return(NULL)
  #     items=names(df)
  #     selectInput("multivariable_Ui_gra_at1", "Atributo 1:",items)
  #     
  #   })
  #   
  # })
  # 
  #--Código que muestra las relaciones gráficas si se pulsa el botónn action
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
    
    output$multivar_msj_graf <- renderPrint({
      if (colcharacter=="False"){
        print("Gráficas de exploración multivariable de todos los atributos")
      }else{
        print("Se muestran únicamente las variables de caracter numérico o factor")
      }
    })
    
  })
    
  #------Correlación multivariable
  
  #--Código que muestra las relaciones gráficas si se pulsa el botónn action
  observeEvent(input$multivar_Action_correlacion,{
    if (ficheroFactorizado=="False"){
      df<-filedata()
      if (is.null(df)) return(NULL)
      
    }else{
      df<-fileout_fact_dosVar
    }
    
    colcharactermulti<-"False"
    
    #Mostramos los resultados
    output$multivar_print_correlacion <- renderPrint({
      #Mostramos la relación de todas la variables numéricas
      #Buscamos las variables no-numéricas
      for (i in ncol(df)){
        if (class(df[,i])=="character"){
          colcharactermulti<<-"True"
          df[,i]<-NULL
        }
      }
      corr.test(df)
      
    })
    
    output$multivar_msj_correlacion <- renderPrint({
      if (colcharactermulti=="False"){
        print("Se muestran los valores de correlación y p-values de todos los atributos")
      }else{
        print("Se muestran únicamente los valores de correlación y p-values de las variables de caracter numérico o factor")
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
  
  #---RLS----
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
  
  #Ejecutamos la correlación
  observeEvent(input$reglinealsimple_Action,{
      df<-filedata()
      if (is.null(df)) return(NULL)
      at1<-input$reglinealsimple_at1
      X<-df[,at1]
      at2<-input$reglinealsimple_at2
      Y<-df[,at2]
      
      if ((class(df[,at1])=="numeric" || class(df[,at1])=="integer") && (class(df[,at2])=="numeric" || class(df[,at2])=="integer")){
          
        #Exportamos el modelo como variable global
         modelo<<-lm( Y ~ X, data=df )
      
         #Mostramos los resultados
         output$reglienalsimple_print <- renderPrint({ summary(modelo) })
         
         #Histograma de los residuos
         output$reglienalsimple_plot1 <- renderPlot({ 
                hist(modelo$residuals, xlab = "Residuos", col = "gray", 
                main = "Distribución de los residuos") 
           })
         
         #Q-Q de los residuos
         output$reglienalsimple_plot2 <- renderPlot({ 
               qqnorm(modelo$residuals, main = "Q-Q Plot de los residuos") 
               qqline(modelo$residuals) 
           })
         
         #Variación ecuanime plot
         output$reglienalsimple_plot3 <- renderPlot({
              plot(modelo$fitted.values, modelo$residuals, ylab = "Residuos", 
                xlab = "Valores ajustados", main="Distribución de residuos") 
               abline(0, 0, lwd = 3)
         })
         
         #Mostramos el mensaje
         output$reglienalsimple_msj <- renderPrint({ print("Se muestran los datos del modelo lineal generado.") })
      }else{
        
        #Mostramos el mensaje
        output$reglienalsimple_msj <- renderPrint({ print("Alguno de los atributos (X o Y) no es numérico.") })
        
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
  
  #-------REgresión lineal Múltiple--------
  
  
  
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
    
    
    #Obtenemos la lista de columnas
    valoresX<-input$reglinealmulti_at1 
    arrayList<-strsplit(valoresX,";")[[1]]
    X<-df[,arrayList]
    
    at2<-input$reglinealmulti_at2
    Y<-df[,at2]
    
    #Comprobamos si algún valor no es numérico
    valorCaracter<-"False"
    xnom<-c("")
    
    for (i in 1:length(arrayList)){
      if (class(df[,arrayList[i]])!="integer" && class(df[,arrayList[i]])!="numeric"){
        valorCaracter<-"True"
      }
      assign(paste("X",arrayList[i],sep=""),df[,arrayList[i]])
        
    }
        xnom <- paste("X",arrayList,sep="")
        formu<-paste(xnom,collapse="+")

    if (valorCaracter=="False"){
      
      #Exportamos el modelo como variable global
      modelo<<-lm( as.formula(paste("Y ~", formu)), data=df )
      
      #xnam <- paste("x", 1:3, sep="")
      #fmla <- as.formula(paste("y ~ ", paste(formula, collapse= "+")))
      #lm(fmla, data = myData).
      
      #formula=paste(df[,as.numeric(arrayList[1])],df[,as.numeric(arrayList[2])],df[,as.numeric(arrayList[3])],sep="+")
     
      #Mostramos los resultados
      output$reglienalmulti_print <- renderPrint({ summary(modelo) })
      
      #Histograma de los residuos
      output$reglienalmulti_plot1 <- renderPlot({
        hist(modelo$residuals, xlab = "Residuos", col = "gray",
             main = "Distribución de los residuos")
      })

      #Q-Q de los residuos
      output$reglienalmulti_plot2 <- renderPlot({
        qqnorm(modelo$residuals, main = "Q-Q Plot de los residuos")
        qqline(modelo$residuals)
      })

       #Variación ecuanime plot
       output$reglienalmulti_plot3 <- renderPlot({
         #Dividimos la pantalla para mostrar las gráficas del modelo
         layout(matrix(c(1,2,3,4),2,2))
         plot(modelo)
       })
      
      #Mostramos el mensaje
      output$reglienalmulti_msj <- renderPrint({ print("Se muestran los datos del modelo lineal múltiple generado.") })
    }else{
      
      #Mostramos el mensaje
      output$reglienalmulti_msj <- renderPrint({ print("Alguno de los atributos (X o Y) no es numérico.") })
      
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
    
    #EL dataset debe contener las 2 columnas seleccionadas únicamente
    df<-df[,c(input$cluster_at2,input$cluster_at1)]
    
    
    #Hacemos globales los modelos
    dos<<-kmeans(df,as.numeric(input$cluster_n1))
    tres<<-kmeans(df,as.numeric(input$cluster_n2))
    
    #Pintamos las salidas tabulares de los clusters genrados
    output$cluster_print1 <- renderPrint({ dos })
    output$cluster_print2 <- renderPrint({ tres })
    
    # #Generramos el modelo hibrido
    # ncolsDF<-ncol(df)
    # clusTotal<-cbind (df, clus1<-dos$cluster, clus2<-tres$cluster)
    # hibrido<-cbind(clusTotal, forma_hibrida=rep(0, dim(clusTotal)[1]))
    # for (e in 1:dim(hibrido[1])[1]){
    #   if (hibrido[e,ncolsDF+1] == hibrido[e,ncolsDF+2]){
    #     hibrido[e,ncolsDF+3]<-hibrido[e,ncolsDF+3]
    #   }
    #   if (hibrido[e,ncolsDF+1] != hibrido[e,ncolsDF+2]){
    #     hibrido[e,ncolsDF+3]<-hibrido[e,ncolsDF+3]+15
    #   }
    # }
    # hibridoCluster<<-hibrido
    
    #Creamos un fichero con dos nuevas columnas con el cluster al que pertenece el registro
    clus<-cbind(df,clus2=dos$cluster,clus3=tres$cluster)
    at1<<-clus[,input$cluster_at1]
    at2<<-clus[,input$cluster_at2]
    
    
    #Dibujamos cluster 1
    output$cluster_plot1 <- renderPlot({
      plot(at1, at2, col=dos$cluster, #asp=1
           pch=dos$cluster, main="Dos Clusters",
           xlab="Atributo X", ylab="Atributo Y",
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
           xlab="Atributo X", ylab="Atributo Y")
      points(tres$centers[,2], tres$centers[,1], pch=23,
             col="maroon", bg="lightblue", cex=3)
      text(tres$centers[,2], tres$centers[,1], cex=1.1,
           col="black", attributes(tres$centers)$dimnames[[1]])
    })
    
    # #Dibujamos el modelo hibrido
    # output$cluster_plot3 <- renderPlot({
    #   plot(at1, at2, col=dos$cluster,
    #        main="Modelo Hibrido",
    #        pch=hibrido$forma_hibrida, cex=1.1,
    #        xlab="Atributo X", ylab="Atributo Y", asp=1)
    #   points(tres$centers[1:2,2], tres$centers[1:2,1], pch=23,
    #          col="maroon", bg="lightblue", cex=3)
    #   text(tres$centers[1:2,2], tres$centers[1:2,1], cex=1.1,
    #        col="black", attributes(dos$centers)$dimnames[[1]])
    # })
    
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
  
  # #Actualizamos la salida del modelo hibrido
  # observe({
  #   clus3<-input$cluster_explo3
  #   output$cluster_print3 <- renderPrint({
  #     
  #     switch(clus3, 
  #            Sumario={
  #              hibridoCluster
  #            },
  #            Clusters={
  #              hibridoCluster$cluster
  #            },
  #            Centers={
  #              hibridoCluster$centers
  #            },
  #            Totss={
  #              hibridoCluster$totss   
  #            },
  #            Withinss={
  #              hibridoCluster$withinss
  #            },
  #            Tot.Withinss={
  #              hibridoCluster$tot.withinss
  #            },
  #            Betweens={
  #              hibridoCluster$betweens
  #            },
  #            Size={
  #              hibridoCluster$size
  #            },
  #            Iter={
  #              hibridoCluster$iter
  #            },
  #            Ifault={
  #              hibridoCluster$ifault
  #            }
  #     )
  #   })
  # })
  
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
    
    #Generamos un nuevo fichero a partir de las dos columnas
    df<-df[,c(at1,at2)]
    
   #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
    df[,paste(at1,"scale",sep="_")]<-as.numeric(scale(df[,at1]))
    df[,paste(at2,"scale",sep="_")]<-as.numeric(scale(df[,at2]))
    
    #Lo globalizamos para que sea accesible
    fileClusterNorm<<-df
    
   #Creamos la semilla y el modelo jerarquico
    set.seed(456)
    hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
    print("Voy a petar")
    #Visualizacion del modelo y exportamos a variable global
    dendro<<-stats::as.dendrogram(hc_model)
    print("dendr")
     #Exportamos como variable global para que pueda ser recogido por el siguiente evento
     dendro_six_color<<-color_branches(dendro, k=as.numeric(input$clusterj_nclusters))
     print("dendrsix")
      output$clusterj_plot1 <- renderPlot({
        plot(dendro_six_color,leaflab="none", horiz=TRUE,
             main="Dendrograma de los atributos seleccionados", xlab="Altura")
      #abline(v=37.5, lty="dashed", col="blue")
      })
    
   
    
  })
  
  observeEvent(input$clusterj_AddValorCorte,{
    valor<-input$clusterj_corte

     output$clusterj_plot1 <- renderPlot({
      plot(dendro_six_color,leaflab="none", horiz=TRUE,
            main="Dendrograma de los atributos seleccionados", xlab="Altura")
          abline(v=valor, lty="dashed", col="blue")
     })

    output$clusterj_print <- renderPrint({
      str(cut(dendro,h=valor)$upper)
    })
  
    output$clusterj_plotFinal <- renderPlot({
      #Incluimos la gráfica de análisis para jerarquía
      #Preparing the Results 
      df<-fileClusterNorm
      
      #Guardamos las variables
      at1<-input$clusterj_at1
      at2<-input$clusterj_at2
      
      #Creamos la semilla y el modelo jerarquico
      set.seed(456)
      hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
      
      #Visualizacion del modelo y exportamos a variable global
      
      dendrog<<-as.dendrogram(hc_model)
      
      modelo <- kmeans(df[, 3:4], as.numeric(input$clusterj_nclusters)) 
      
      
      #Continuamos mostrando las graficas comparadas
      df$clusModelo <- modelo$cluster 
      dend_modelo <- dendextend::cutree(dendrog, k = as.numeric(input$clusterj_nclusters)) 
      df$dendModelo <- dend_modelo 
      
      
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
      
      #Dibujamos la gráfica teniendo en cuenta que hemocs cambiado el nombre de las 2 primeras columnas
      plot(df$var1, df$var2, col = df$dendModelo, 
           pch = df$dendModelo - 1, xlab = at1, ylab = at2,
           main = "Marketing Clusters from Hierarchical Clustering \n (Labels show medians of age and income for cluster)") 
      #Añadimos los centros
      points(labels[ ,2], labels[ ,3], pch = 21, col = 'maroon', bg = 'white', cex = 3) 
      #Texto de los centros
      text(labels[, 2], labels[, 3], cex = 1.1, col = 'black', labels[, 1]) 
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
    
    #Generamos un nuevo fichero a partir de las dos columnas
    df<-df[,c(at1,at2)]
    
    #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
    df[,paste(at1,"scale",sep="_")]<-as.numeric(scale(df[,at1]))
    df[,paste(at2,"scale",sep="_")]<-as.numeric(scale(df[,at2]))
    
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
    
    output$clusterelbow_plot1 <- renderPlot({
      
      plot(optimizado$wss ~ optimizado$clusters, type = "b",  
                  ylim = c(0, 12000), ylab = 'Suma del Error Cuadrático', 
                  main = 'Número óptimo del cluster basado en el error', 
                  xlab = 'Número de clusters', pch = 17, col = 'black') 
       })
    
   
  })
  
  observeEvent(input$clustereva_CompaAction,{
    df<-filedata()
    if (is.null(df)) return(NULL)
    #Guardamos las variables
    at1<-input$clustereva_at1
    at2<-input$clustereva_at2
    
    #Generamos un nuevo fichero a partir de las dos columnas
    df<-df[,c(at1,at2)]
    
    #Nomalizamos las escalas añadiendo dos nuevas columnas al dataset
    df[,paste(at1,"scale",sep="_")]<-as.numeric(scale(df[,at1]))
    df[,paste(at2,"scale",sep="_")]<-as.numeric(scale(df[,at2]))
    
    #Creamos la semilla y el modelo jerarquico
    set.seed(456)
    hc_model<-hclust(dist(df[,3:4]),method="ward.D2")
    
    #Visualizacion del modelo y exportamos a variable global
    dendro<<-as.dendrogram(hc_model)
    
    cinco <- kmeans(df[, 3:4], as.numeric(input$cluster_eval1)) 
    seis <- kmeans(df[, 3:4], as.numeric(input$cluster_eval2)) 
    
    #Continuamos mostrando las graficas comparadas
    df$clus5 <- cinco$cluster 
    dend_five <- dendextend::cutree(dendro, k = as.numeric(input$cluster_eval1)) 
    df$dend5 <- dend_five 
    
    
    df$clus6 <- seis$cluster 
    dend_six <- dendextend::cutree(dendro, k = as.numeric(input$cluster_eval2)) 
    df$dend6 <- dend_six 
    
    # Choosing a Model 
    output$clustereva_plot1 <- renderPlot({
      #par(mfrow = c(2, 2), mar = c(3, 4, 4, 2) + 0.1) 
      plot(df$age, df$income, col = cinco$cluster, 
        pch = cinco$cluster, xlab = '', main = '5-means Clustering') 
    })
    
    output$clustereva_plot3 <- renderPlot({
      #par(mfrow = c(2, 2), mar = c(3, 4, 4, 2) + 0.1) 
      plot(df$age, df$income, col = seis$cluster, xlab = '', 
        ylab = '', pch = seis$cluster, main = '6-means Clustering') 
    
    })
    
    output$clustereva_plot2 <- renderPlot({
      #par(mar = c(5, 4, 2, 2) + 0.1) 
      plot(df$age, df$income, col = df$dend5, 
             pch = df$dend5, main = 'k = 5 Hierarchical') 
    })
    
    output$clustereva_plot4 <- renderPlot({
      #par(mar = c(5, 4, 2, 2) + 0.1) 
      plot(df$age, df$income, col = df$dend6, ylab = '',  
            pch = df$dend6, main = 'k = 6 Hierarchical') 
    })
   # par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) 
   
    # output$cluster_EvalFinal <- renderUI({
    #   # var1=input$cluster_eval1
    #   # var2=paste(input$cluster_eval1
    #   # var3=paste(input$cluster_eval2,"_K-means")
    #   # var4=paste(input$cluster_eval2,"_K-means")
    #   
    #   items=names(df)
    #   # selectInput("cluster_EvalFinal", "Selec. Modelo",
    #   #             c(paste0(input$cluster_eval1,"-means")="modelo1",
    #   #               # paste(input$cluster_eval1,"K-means")="modelo2",
    #   #               # paste(input$cluster_eval2,"K-means")="modelo3",
    #   #               var2="modelo4"),
    #   #             selected="modelo1")
    #   selectInput("cluster_EvalFinal", "Selec. Modelo",
    #               list(`K-means` = c(paste0(input$cluster_eval1,"-means"), paste0(input$cluster_eval2,"-means")),
    #                    `Jerarquía` = c(paste0(input$cluster_eval1,"-Jerarquico"), paste0(input$cluster_eval2,"-Jerarquico"))
    #                    )
    #   )
    #})
    
    #Esta parte finalmente se incluye en el apartado de clustering jerarquico.
    
  })
  
  # observeEvent(input$clustereva_SelectAction,{
  # 
  #   print("Hola")
  #   print("q")
  # })
  #   
  
  
  #-------SERIES TEMPORALES---------
  #------------ARIMA----------------
  

  #Evento que ejecuta la función anterior
  observeEvent(input$arima_predAction, {
    df<-filedata()
    if (is.null(df)) return(NULL)
    condicion<-Funcion_arimaModelCondicion(df)
    if (condicion=="TRUE"){
      
      mensual<<-Funcion_arimaTabla(condicion,df)
      #Print de la representación tabular de la tabla temporal generada
      output$arima_print1 <- renderPrint({
        mensual
      })
      
      #Mensaje de ejecución
      output$arima_msj <- renderPrint({
        print("Se muestran los resultados de la conversión de la serie temporal")
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
      
    }else{
      
      output$arima_msj <- renderPrint({
        print("El dataset no cumple las condiciones para ser transformado en una serie temporal.")
      })
    }
    
  })
  
  #Evento que genera el modelo
  observeEvent(input$arima_generModelArima, {
    df<-filedata()
    if (is.null(df)) return(NULL)
    # modeloNoEsta<-gsub("\\(","",input$modeloNoEsta)
    # modeloNoEsta2<-gsub("\\)","",modeloNoEsta)
    # modeloNoEsta3<-as.numeric(unlist(strsplit(modeloNoEsta2, split=",")))
    # 
    # modeloEsta<-gsub("\\(","",input$modeloEsta)
    # modeloEsta2<-gsub("\\)","",modeloEsta)
    # modeloEsta3<-as.numeric(unlist(strsplit(modeloEsta2, split=",")))
    modeloNoEsta=as.numeric(c(input$modeloNoEsta_p,input$modeloNoEsta_d,input$modeloNoEsta_q))
    modeloEsta=as.numeric(c(input$modeloEsta_P,input$modeloEsta_D,input$modeloEsta_Q))
    
    modeloARIMA <<- arima(mensual, modeloNoEsta,
                  seasonal = list(order = modeloEsta))
    
    #Mensaje de salida
    output$arima_msj2 <- renderPrint({
      print("Se muestran las gráficas de modelo ARIMA generado")
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
      #obtenemos el primer año del dataset
      annoComienzo<-year(df[1,1])
      #Agrupamos mensualmente
      
      
      usuarios_mensuales <- as.data.frame(df %>%
                                            group_by(anno = year(df[,1]),
                                                     mes = month(df[,1])) %>%
                                            summarise(usuarios = sum(count)))
      
      usuarios <- usuarios_mensuales[, 3]
      
      condicion<-Funcion_arimaModelCondicion(df)
      
     
      
      if (condicion=="TRUE"){
        
            mensual<<-Funcion_arimaTabla(condicion,df)
            
            output$tbats_plot1<-renderPlot({
              
              
              #Modelo avanzado
              modeloAvd <- tbats(mensual)
              predAvan<-forecast(modeloAvd,h=input$slider_predtbats)
              plot(predAvan)
              
              summary(predAvan$mean)
              summary(predAvan$upper)
              summary(predAvan$lower)
              
              mean_2011 <- round(as.numeric(
                filter(usuarios_mensuales, anno == 2011) %>%
                  summarise(mean = mean(usuarios))), 0)
              mean_2012 <- round(as.numeric(
                filter(usuarios_mensuales, anno == 2012) %>%
                  summarise(mean = mean(usuarios))), 0)
              mean_2013 <- round(mean(predAvan$mean), 0)
              max_mean_2013 <- round(max(predAvan$mean), 0)
              
              abline(h = max(predAvan$mean), lty = 2, col = "blue")
              segments(2011, mean_2011, x1 = 2012, y1 = mean_2011,
                       col = "darkgray", lty = 2, lwd = 2)
              segments(2012, mean_2012, x1 = 2013, y1 = mean_2012,
                       col = "darkgray", lty = 2, lwd = 2)
              segments(2013, mean_2013, x1 = 2014, y1 = mean_2013,
                       col = "blue", lty = 2, lwd = 2)
              
              text(2011.15, mean_2011 + 10000, mean_2011)
              text(2012, mean_2012 + 10000, mean_2012)
              text(2013, mean_2013 + 10000, mean_2013)
              text(2013.85, max_mean_2013 + 10000, max_mean_2013)
              
            })
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
    if (class(df[,input$visual_color])!="character" && input$visual_color!="NULL"){
      esfactorvisual<<-"TRUE"
      }else{
        esfactorvisual<<-"FALSE"
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
      
   
    df$emp_size <- cut(df[,input$visual_factor], breaks = ngrupos)
                             #labels = c("Employees: 3 - 6", "7 - 9", "10+"))
    
    #if(!require("ggplot2")) install.packages("ggplot2")
    #suppressMessages(suppressWarnings(library(ggplot2)))
    #if(!require("scales")) install.packages("scales")
    #suppressMessages(suppressWarnings(library(scales)))
    
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
      plot <- ggplot(data = df, aes(x = df[,input$visual_at1],
                                          y = df[,input$visual_at2]))
      
      plot <- plot + facet_grid(. ~ emp_size) + 
        geom_point(aes(color = df[,input$visual_color]), shape = 18, size = 4)
      
      plot + 
        scale_color_discrete(guide = guide_legend(title = paste0(input$visual_color,"\nDensity"))) +
        xlab(input$visual_at1) + ylab(input$visual_at2) 

        }
     #}#else if(df!=NULL)
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
                      addMarkers(data = df, ~df[,longitud], ~df[,latitud],
                                 popup = ~popup)
          })
        
        output$geo_msj<-renderPrint({
          print("Se muestra la localización de los datos")
        })
 
      }else{
        
        output$geo_msj<-renderPrint({
          print("No hay datos de geolocalización en el dataset.")
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
        
        output$ruta_msj<-renderPrint({
          print("Se muestra la localización de los datos")
        })
        
      }else{
        
        output$ruta_msj<-renderPrint({
          print("No hay datos de geolocalización en el dataset.")
        })
        
        #Reiniciamos el mapa de cooordenadas
        output$ruta_plot<-renderLeaflet({ })
        
      }
      
    })
  
})

