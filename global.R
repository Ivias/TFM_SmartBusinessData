




#-------Funciones ARIMA----------------------

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

Funcion_arimaTabla<-function(condicion,df){
  #Si los valores de la primera columna del dataset son aptos (condicion<-TRUE), continuamos. Si no mostramos msj de error
  if (condicion==TRUE){
    #obtenemos el primer año del dataset
    annoComienzo<-year(df[1,1])
    #Agrupamos mensualmente
    usuarios_mensuales <- as.data.frame(df %>%
                                    group_by(anno = year(df[,1]),
                                             mes = month(df[,1])) %>%
                                    summarise(usuarios = sum(count)))
    
    table(usuarios_mensuales$anno, usuarios_mensuales$mes)
    
    usuarios <- usuarios_mensuales[, 3]
    mensual <- ts(usuarios, frequency = 12, start = c(annoComienzo, 1))
    
  }
}

#----------Funciones de GEOLOCALIZACIÓN----------

