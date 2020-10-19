## PROCESAMIENTO DEL LENGUAJE ##
# Descripción: Realiza procesamiento del lenguaje al texto de un conjunto de tweets
# Parámetros de entrada: data: Dataframe formado mínimo por dos columnas ID y content que contiene el texto del tweet a procesar
# Salida: Mismo dataframe de entrada con la columna content procesada
####
preProcessing <- function(data)
{
  # Eliminación de emojis
  data$content <- str_replace_all(data$content,"[^[:alnum:][:blank:][:punct:]]", "")
  # Eliminación de usuarios
  data$content <- str_replace_all(data$content,"@\\w+","")
  data$content <- str_replace_all(data$content,"#[a-z,A-Z]*","")
  
  # Eliminación URLs
  data$content <- str_replace_all(data$content,"http\\S*", "")
  
  # Eliminación signos de puntuación y números 
  data$content <- str_replace_all(data$content,"[[:punct:]]", "")
  data$content <- str_replace_all(data$content,"[[:digit:]]", "")
  
  # Transformación a minúscula
  data$content <- tolower(data$content)
  
  # Eliminación texto de RT
  data$content <- str_replace(data$content,"(RT|via)((?:\\b\\W*@\\w+)+)","")
  
  # Eliminación de artículos, monosílabas y palabras sobrantes
  articulos <- stopwords(kind = "spanish")
  stopwords_regex = paste(articulos, collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  data$content <- str_replace_all(data$content, stopwords_regex, "")
  rm(stopwords_regex)
  rm(articulos)
  
  # Eliminación de espacios en blanco sobrantes
  data$content <- str_replace_all(data$content,"[\\s]+", " ")
  data$content <- str_replace_all(data$content,"^[\\s]+", "")
  
  #Preprocesamiento Corpus
  corpus = Corpus(VectorSource(data$content))
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeWords,stopwords("spanish"))
  corpus = tm_map(corpus, stemDocument)
  
  #Conversión Corpus a dataframe
  tweets_procesados = as.data.frame(as.matrix(corpus$content))
  colnames(tweets_procesados) = make.names(colnames(tweets_procesados))
  data$content <- tweets_procesados
  return(data)
}

