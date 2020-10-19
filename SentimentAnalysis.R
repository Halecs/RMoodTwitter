## ANÁLISIS DE SENTIMIENTOS DICCIONARIOS ##
# Descripción: Realiza análisis de sentimientos con diccionarios.
# Parámetros de entrada: subset: Dataframe que contiene el conjunto de tweets
#                        dictionary: nombre del archivo a usar como diccionario
#                        type: tipo de diccionario, a elegir entre txt o xml
# Salida: Dataframe de entrada con una columna adicional (ave_sentiment) con la predicción realizada
####
SentimentAnalysis <- function(subset, dictionary="elhPolar", type="txt")
{
  #Elige el tipo de diccionario
  if(type == "txt")
  {
    #Lectura del archivo .txt
    data <- read.table(file=paste(dictionary, ".txt"), sep=" " , quote="\n", comment.char="#")
    matrix <- str_split_fixed(data$V1, "\t", 2)
    diccionario <- as_tibble(matrix, preserve_row_names = TRUE)
    diccionario <- diccionario %>% mutate(V2 = if_else(V2 == "negative", -1, 1))
    diccionario$V1 <- str_replace_all(diccionario$V1,"Ã±", "ñ")
    diccionario$V1 <- tolower(diccionario$V1)
    diccionario <- diccionario[!duplicated(diccionario$V1), ]
    diccionario <- diccionario %>% rename(x = V1, y = V2)
    dictionary <- as_key(diccionario)
  }
  
  if(type == "xml")
  {
    #Lectura del archivo .xml
    wd <- dictionary
    source("leer_xml.R")
    info.basic <- as_tibble(info.basic, preserve_row_names = TRUE)
    info.basic <- info.basic %>% mutate(V2 = if_else(V2 == "negative", -1, 1))
    
    #Correción de tildes y espacios
    info.basic$V1 <- str_replace_all(info.basic$V1,"Ã³", "o")
    info.basic$V1 <- str_replace_all(info.basic$V1,"Ã±", "ñ")
    info.basic$V1 <- str_replace_all(info.basic$V1,"Ã©", "e")
    info.basic$V1 <- str_replace_all(info.basic$V1,"Ã", "i")
    info.basic$V1 <- str_replace_all(info.basic$V1,"iº", "u")
    info.basic$V1 <- str_replace_all(info.basic$V1,"i¡", "a")
    info.basic$V1 <- str_replace_all(info.basic$V1,"_", " ")
    dictionary <- as_key(info.basic)
  }
  
  result <- sentiment_by(subset$content, polarity_dt = dictionary)
  return(result)
}

## MATRIZ DE CONFUSIÓN ##
# Descripción: Construye la matriz de confusión posterior a la realización de un análisis de sentimientos basado en diccionarios.
# Parámetros de entrada: subset: Dataframe que contiene la predicción del algoritmo y la clasificación correcta de cada tweet.
# Salida: Matriz que representa la matriz de confusión del conjunto de tweets.
####
getCMDictionary<- function(subset)
{
  #Cálculo de los neutros acertados y los falsos neutros
  Neutros <- subset[subset$sentiment == "NEU"]
  TNeutro <- Neutros[Neutros$ave_sentiment == 0.000000000]
  FNeutro_positivos <-  Neutros[Neutros$ave_sentiment > 0.000000000]
  FNeutro_negativos <-  Neutros[Neutros$ave_sentiment < 0.000000000]
  
  #Cálculo de los positivos acertados y los falsos positivos
  Positivos <- subset[subset$sentiment == "P"]
  TPositivos <- Positivos[Positivos$ave_sentiment > 0.000000000]
  FPositivos_neutros <- Positivos[Positivos$ave_sentiment == 0.000000000]
  FPositivos_negativos <- Positivos[Positivos$ave_sentiment < 0.000000000]
  
  #Cálculo de los negativos acertados y los falsos negativos
  Negativos <- subset[subset$sentiment == "N"]
  TNegativos <- Negativos[Negativos$ave_sentiment < 0.000000000]
  FNegativos_neutros <- Negativos[Negativos$ave_sentiment == 0.000000000]
  FNegativos_positivos <- Negativos[Negativos$ave_sentiment > 0.000000000]
  
  #Construcción de la matriz con los datos calculados
  CM = matrix(c(TNegativos, FNegativos_neutros, FNegativos_positivos, 
                FNeutro_negativos, TNeutro, FNeutro_positivos,
                FPositivos_negativos, FPositivos_neutros, TPositivos)
              + nrow = 3, 
              + ncol = 3, 
              + byrow = TRUE)
  rm(Neutros)
  rm(Positivos)
  rm(Negativos)
  return(CM)
}

## GRÁFICA NUM TWEETS / SENTIMENT ##
# Descripción: Muestra una grÁfica donde se representa el nÚmero de tweets de cada sentimiento en el conjunto de tweets.
# ParÁmetros de entrada: subset: Dataframe que contiene un conjunto de tweets clasificado.
# Salida: Gráfica
####
NumTweetsPerSentiment <- function(subset)
{
  sum(is.na(subset))
  subset %>%
    ggplot(aes(sentiment)) +
    geom_bar(na.rm = TRUE) +
    xlab("Sentimiento") + 
    ylab("Número de tweets")
}



