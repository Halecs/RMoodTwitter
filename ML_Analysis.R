## CONFIGURACIóN MÉTODO VALIDACIÓN ##
# Descripción: Configura el método de validaciÓn a utilizar en el entrenamiento del conjunto de tweets.
# Parámetros de entrada: method: método de validación a utilizar
#                        subsets: número de subconjuntos a crear
#                        repeats: número de repeticiones
# Salida: Objeto tipo lista que contiene como atributos los parámetros recibidos.
####
configureValidationMethod <- function(method="repeatedcv", subsets=5, repeats=2)
{
  control <- trainControl(method= method, number = subsets, repeats=repeats)
  return(control)
}

## ENTRENAMIENTO DEL MODELO ##
# Descripción: Entra un modelo de datos con un método y unos parámetros de control suministrados
# Parámetros de entrada: trainingData: dataframe con el conjunto de tweets de entrenamiento a utilizar para el modelo
#                        method: método de entrenamiento
#                        trControl: lista de parámetros de validación
# Salida: Objeto tipo lista con los parámetros del modelo
####
trainModel <- function(trainingData, method="rf", trControl)
{
  model <- train(sentiment~., data=trainingData, method=method, trControl=trControl)
  return(model)
}

## CONSTRUCCIóN DE MATRIZ DE FRECUENCIAS DE TéRMINOS ##
# Descripción: Construye la matriz de frecuencias de términos para un conjunto de tweets dado
# Parámetros de entrada: subset: dataframe con el conjunto de tweets
# Salida: Dataframe representando la matriz de frecuencias
####
buildTermMatrix <- function(subset)
{
  #Preprocesamiento de Corpus previo
  corpus = Corpus(VectorSource(subset$content))
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeWords,stopwords("spanish"))
  corpus <- tm_map(corpus, stemDocument, "spanish")
  
  #Construcción matriz
  frequencies = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(frequencies, 0.995)
  
  #Reconvierte a dataframe
  tSparse = as.data.frame(as.matrix(sparse))
  colnames(tSparse) = make.names(colnames(tSparse))
  return(tSparse)
}

## ANÁLISIS DE SENTIMIENTOS BASADO EN APRENDIZAJE AUTOMÁTICO ##
# Descripción: Realiza un análisis de sentimientos con métodos basados en aprendizaje automático
# Parámetros de entrada: subset: dataframe con el conjunto de tweets a analizar, incluyendo su previa clasificación externa
#                        termMatrix: matriz de frecuencias de términos en formato dataframe
#                        method: método para el análisis de sentimientos
#                        control:lista con los parámetros de validación del modelo
# Salida: Dataframe de entrada con una columna agregada representado la predicción realizada por el algoritmo
####
ML_Analysis <- function(subset, termMatrix, method, control)
{
  #Separación de datos en test/train a proporción 20/80
  set.seed(2340)
  trainIndex <- createDataPartition(termMatrix$sentiment, p = 0.8, 
                                    list = FALSE, 
                                    times = 1)
  termMatrix$sentiment = subset$sentiment
  termMatrix$sentiment = as.factor(termMatrix$sentiment)
  tweets_train <- termMatrix[trainIndex, ]
  tweets_test <- termMatrix[-trainIndex, ]
  
  #Entrenamiento del modelo con los datos de training
  Model <- trainModel(trainingData = tweets_train, method = "rf", trControl = control)
  pred <- predict(Model, newdata = tweets_test)
  
  #Guardamos la predicción en una nueva columna del dataframe
  subset$predicted <- pred
  return(subset)
}


## CONSTRUCCIÓN MATRIZ DE CONFUSIÓN ##
# Descripción: Construye la matriz de confusión del conjunto de tweets analizados
# Parámetros de entrada: predict: lista con la predicción realizada del análisis de sentimientos basado en aprendizaje automático
#                        classified: lista con la clasificación real de los mismos tweets analizados
# Salida: objeto tipo lista que contiene la matriz de confusión del conjunto de tweets junto con métricas que miden el rendimiento
####
getConfusionMatrixML <- function(predict, classified)
{
  CM <- confusionMatrix(pred, classified)
  return(CM)
}


## COMPARACIÓN DE MODELOS ##
# Descripción: Compara con gráficas dos modelos creados a partir de un conjunto de datos
# Parámetros de entrada: model1: lista con los parámetros del primer modelo
#                        model2: lista con los parámetros del segundo modelo
# Salida: gráficas
####
compareModels <- function(model1, model2)
{
  results <- resamples(list(Model1=model1, Model2=model2))
  
  #################### CAJAS DE RANGO #####################
  summary(results)
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  bwplot(results, scales=scales)
  
  ################### GRÁFICAS DE DENSIDAD ##################
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  densityplot(results, scales=scales, pch = "|")
  
  #################### GRÁFICAS DE PUNTOS s#####################
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  dotplot(results, scales=scales)
}



