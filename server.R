shinyServer(function(input, output, session) {

#Cargamos el archivo CSV
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      #Aún no se ha introducido ningún archivo en el editable
      return(NULL)
    }
    #Leemos el archivo
    read.csv(infile$datapath)
  })
  
  #Guardamos una salida out para consulta cada vez que se llama a la función filedata()
  output$filedatacargado <- reactive({
    filedata()
  })
  
  #devolvemos la condición de que el fichero se ha cargado a la variable para consultar desde ui.R <conditionalPanel>
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
  
  ###FIN de los mensajes en las secciones###
  
  #------------------MENÚ DE CONSULTA DE DATOS--------------------------
  
  #Renderiza los combos en funcion de los datos del archivo
  output$var1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
    selectInput("Variable1", "Variable 1:",items)
    
  })
  
  #The following set of functions populate the column selectors
  output$val1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fr=input$Variable1
    items=df[,fr]
    
    selectInput("Valor1", "Valor:",items,selected = NULL)
    
  })
  
  #Renderiza los cobos en funcion de los datos del archivo
  output$var2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
    selectInput("Variable2", "Variable 2:",items)
    
  })
  
  #The following set of functions populate the column selectors
  output$val2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fr=input$Variable2
    items=df[,fr]
    
    selectInput("Valor2", "Valor:",items,selected = NULL)
    
  })
  
  #The checkbox selector is used to determine whether we want an optional column
  #output$amountflag <- renderUI({
  #  df <-filedata()
  #  if (is.null(df)) return(NULL)
  #  
  #  checkboxInput("amountflag", "Use values?", FALSE)
  #})
  
  #If we do want the optional column, this is where it gets created
  #output$amountCol <- renderUI({
  #  df <-filedata()
  #  if (is.null(df)) return(NULL)
  #  #Let's only show numeric columns
  # nums <- sapply(df, is.numeric)
  #  items=names(nums[nums])
  #  names(items)=items
  # selectInput("amount", "Amount:",items)
  #})
  
  output$str <- renderPrint({
   df <-filedata()
    if (is.null(df)) return(NULL)

    str(df)
  })
  
  #funcionFiltro <-eventReactive(input$Filtro,{
  # file=filedata()
  # if (is.null(file)) return(NULL)
    
  # #The function acts reactively when one of the variables it uses is changed
  #  #If we don't want to trigger when particular variables change, we need to isolate them 
  #  #isolate({
  #    #Get the CSV file data
  #    #file=filedata()
  #   #Which from/to columns did the user select?
  #  var1=input$Variable1
  #  val1=input$Valor1
  #   var2=input$Variable2
  #   val2=input$Valor2
      # locations are duplicated in from/to columns, dedupe so we don't geocode same location more than once
  #   if(val2==""){
  #      file %>% filter_(paste(var1, "==", val1))
  #   }else{
  #     fileout<-file %>% filter_(paste(var1, "==", val1))
  #     fileout %>% filter_(paste(var2, "==", val2))
  #    }
  # #filter(file,weather=="2")
  # #})
  # })
  
  #observeEvent(input$Filtro, {
  #  output$filetablecolumnas <- renderTable({
  #    funcionFiltro()
  #  })
  #})
  
  #Se visualiza el contenido delarchivo
  output$filetable <- renderTable({
    filedata()
  })
  
  
  #This previews the CSV data file
  #output$tablafiltro <- renderTable({
    #filedata()
  #})
  
  #output$strcarga <- renderPrint({
  # df <-filedata()
  # if (is.null(df)) return(NULL)
    
  # str(df)
  #})

  #MENU DE CONSULTA DE DATOS
  #Renderiza los combos en funcion de los datos del archivo
  output$variables <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
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
    
    #fileColumnas<-select(file,names(file) %in% contains(input$variablesLista))
    #fileColumnas<-select(df,datetime )
    file.subset <- file[, input$variablesLista]
    
    var1=input$Variable1
    val1=input$Valor1
    var2=input$Variable2
    val2=input$Valor2
    # locations are duplicated in from/to columns, dedupe so we don't geocode same location more than once
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

  #------------------MENÚ DE TRASFORMACIONES DE DATOS--------------------------
  
  #Inicializamos las varibales globales que indican segunda vuelta de las funciones
  eliminadosNA<-"False"
  segundaeliminacion<<-"False"
  
  #Función que devuelve los alores NA encontrados en el dataset
  Funcion_valoresNA<-eventReactive(input$valoresNA,{
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (eliminadosNA=="False") {
    fileout<-table(is.na(df)) 
    }else{
      fileout<-table(is.na(Funcion_eliminarValoresNA()))
    }
  })
  #Evento que ejecuta la función anterior
  observeEvent(input$valoresNA, {
    output$salidaNA <- renderTable({
      Funcion_valoresNA()
    })
  })
  
  #Función que elimina los valores NA
  Funcion_eliminarValoresNA<-eventReactive(input$eliminarNA_Limpiar,{
    df <-filedata()
    if (is.null(df)) return(NULL)
    #Cambia el valor de la varibale para saber que ya se ha ejecutado
    eliminadosNA<<-"True"
    na.omit(df)
  })
  
  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$eliminarNA_Limpiar, {
    Funcion_eliminarValoresNA()
    output$NAeliminados_Limpiar <- renderText({
      if (segundaeliminacion=="False"){
        segundaeliminacion<<-"True"
        print("Registros con valores NA eliminados")
      }else{
        print("Registros con valores NA habian sido previamente eliminados")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$salidaNA <- renderTable({})
  })
  
  #Para guardar el nuevo archivo con la limpieza de vlores NA generado
  observe({
    volumes <- c("UserFolder"=getwd())
    shinyFileSave(input, "guardarTransformaciones", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$guardarTransformaciones)
    data <- Funcion_eliminarValoresNA()
    if (nrow(fileinfo) > 0) {
      write.csv(data, as.character(fileinfo$datapath))
    }
  })
  
  #Función que restaura los valores NA eliminados previamente
  Funcion_restauraValoresNA<-eventReactive(input$restaurar_Limpiar,{
    df <-filedata()
    if (is.null(df)) return(NULL)
      fileout<-df
  })
  
  
  #Evento que ejecuta la función anterior y muestra el mensaje por pantalla
  observeEvent(input$restaurar_Limpiar, {
    Funcion_restauraValoresNA()
    output$NAeliminados_Limpiar <- renderText({
      if (eliminadosNA=="False"){
        print("Aún no se ha realizado ninguna operación sobre el dataset")
      }else{
        #Restauramos para que se pueda volver a eliminar
        eliminadosNA<<-"False"
        print("Se ha restaurado el dataset original")
      }
    })
    #Evento que reinicia los resultados de la tabla
    output$salidaNA <- renderTable({})
  })
  
  #------------------FIN DE TRASFORMACIONES--------------------------
  
  #------------------MENU DE EDICION DE DATOS--------------------------
  
  output$atributosEdicion <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
    selectInput("atributosEdicion", "Atributo Origen:",items)
    
  })
  
  output$otroAtributo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    #names(items)=items
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
  
})
