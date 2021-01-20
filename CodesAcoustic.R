#datos = dbtotal

#crear_sets <- function(datos, proporcion = .7) {
#  sets <- list()
#  
#  datosUS <- sample_frac(datos, proporcion)
#  nval = table(datosUS$Region_class)
#  
#  undersample_ds <- function(x, classCol, nsamples_class){
#    for (i in 1:length(unique(x[, classCol]))){
#      class.i <- unique(x[, classCol])[i]
#      if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
#        x <- x[-sample(which(x[, classCol] == class.i), 
#                       sum(x[, classCol] == class.i) - nsamples_class), ]
#      }
#    }
#    return(x)
#  }
#  
#  sets[["entrenamiento"]] <- undersample_ds(datosUS, "Region_class", nsamples_class=min(nval))
#  sets[["prueba"]] <- setdiff(datos, sets[["entrenamiento"]])
#  
#  sets
#}

###Nuevo codigo para crear_sets undersampling
crear_sets <- function(datos, proporcion = .8) {
  sets <- list()
  set.seed(123)
  n <- nrow(datos)
  n_train <- round(proporcion * n)
  train_ind <- sample(1:n, n_train)
  datosUS = datos[train_ind,]
  nval = table(datosUS$Region_class)
  
  undersample_ds <- function(x, classCol, nsamples_class){
    for (i in 1:length(unique(x[, classCol]))){
      class.i <- unique(x[, classCol])[i]
      if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
        x <- x[-sample(which(x[, classCol] == class.i), 
                       sum(x[, classCol] == class.i) - nsamples_class), ]
      }
    }
    return(x)
  }
  
  sets[["entrenamiento"]] <- undersample_ds(datosUS, "Region_class", nsamples_class=min(nval))
  datos_prueba_ind <- setdiff(rownames(datos), rownames(sets[["entrenamiento"]]))
  sets[["prueba"]] <- datos[datos_prueba_ind,]
  sets
}

###Nuevo codigo para crear_sets oversampling 29-07-2020
##MWMOTE: Majority Weighted Minority Oversampling TEchnique
#crear_sets <- function(datos, proporcion = .8) {
#  sets <- list()
#  set.seed(123)
#  n <- nrow(datos)
#  n_train <- round(proporcion * n)
#  train_ind <- sample(1:n, n_train)
#  datosOV = datos[train_ind,]
#  nval = table(datosOV$Region_class)
#  datosOV_Anch=datosOV[which(datosOV$Region_class=='Anch'),]
#  datosOV_Mun=datosOV[which(datosOV$Region_class!='Vin'),]
#  datosOV_Mun$Region_class=droplevels(datosOV_Mun$Region_class)
#  datosOV_Mun=mwmote(datosOV_Mun, numInstances = max(nval), classAttr = "Region_class")
#  datosOV_Vin=datosOV[which(datosOV$Region_class!='Mun'),]
#  datosOV_Vin$Region_class=droplevels(datosOV_Vin$Region_class)
#  datosOV_Vin=mwmote(datosOV_Vin, numInstances = max(nval), classAttr = "Region_class")
#  sets[["entrenamiento"]] <-rbind(datosOV_Anch,datosOV_Mun,datosOV_Vin)
#  
#  sets[["prueba"]]  = datos[-train_ind,]  
#  
#  sets
#}

###RWO: Random Walk Oversampling
#crear_sets <- function(datos, proporcion = .8) {
#  sets <- list()
#  set.seed(123)
#  n <- nrow(datos)
#  n_train <- round(proporcion * n)
#  train_ind <- sample(1:n, n_train)
#  datosOV = datos[train_ind,]
#  nval = table(datosOV$Region_class)
#  datosOV_Anch=datosOV[which(datosOV$Region_class=='Anch'),]
#  datosOV_Mun=datosOV[which(datosOV$Region_class!='Vin'),]
#  datosOV_Mun$Region_class=droplevels(datosOV_Mun$Region_class)
#  datosOV_Mun=rwo(datosOV_Mun, numInstances = max(nval), classAttr = "Region_class")
#  datosOV_Vin=datosOV[which(datosOV$Region_class!='Mun'),]
#  datosOV_Vin$Region_class=droplevels(datosOV_Vin$Region_class)
#  datosOV_Vin=rwo(datosOV_Vin, numInstances = max(nval), classAttr = "Region_class")
#  sets[["entrenamiento"]] <-rbind(datosOV_Anch,datosOV_Mun,datosOV_Vin)
#  
#  sets[["prueba"]]  = datos[-train_ind,]  
#  
#  sets
#}

entrenar_arbol <- function(sets, objetivo, predictores = ".", mi_cp = mi_cp) {
  if(length(predictores > 1)) {
    predictores <- paste0(predictores, collapse = "+")
  }


  mi_formula <- paste0(objetivo, " ~ ", predictores) %>% as.formula()
  
  arbol <- list()
  arbol[["modelo"]] <- 
    rpart(data = sets[["entrenamiento"]], formula = mi_formula, 
          control = rpart.control(cp = mi_cp, xval = 35, minsplit = 5))#, weights = pesos)
  
  arbol[["prediccion"]] <- predict(arbol[["modelo"]], sets[["prueba"]], type = "class")
  arbol[["referencia"]] <- sets[["prueba"]][[objetivo]]
  
  arbol

  # x11()
  # rpart.plot(arbol[["modelo"]])#arbol
}

obtener_diagnostico <- function(arbol, objetivo, mi_cp = 0.01) {
  diagnostico <- list()
  diagnostico[["matriz"]] <- confusionMatrix(data = arbol[["prediccion"]], 
                                             reference = arbol[["referencia"]])
  
  cp <- with(arbol[["modelo"]], cptable[which.min(cptable[, "xerror"]), "CP"])
  cp_original <- mi_cp
  podar <- if(cp < mi_cp) "SI" else "NO"
  diagnostico[["mincp"]] <- data.frame("CP mÃ?nimo" = cp, "CP original" = cp_original, "Podar" = podar)
  
  diagnostico
} 

crear_arbol <- function(datos, objetivo, predictores = ".", mi_cp = 0.01) {
  resultado <- list()
  resultado[["sets"]] <- crear_sets(datos)
  resultado[["arbol"]] <- entrenar_arbol(resultado[["sets"]], objetivo, predictores, mi_cp)
  resultado[["diagnostico"]] <- obtener_diagnostico(resultado[["arbol"]], objetivo, mi_cp)
  
  resultado
}




