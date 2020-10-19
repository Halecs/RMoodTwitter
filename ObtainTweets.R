## LIBRERÍAS NECESARIAS ##
library(rtweet)
library(knitr)
library(tidyverse)
library(jsonlite)
library(mongolite)
library(tm)
library(textclean)
library(tidytext)
library(sentimentr)
library(SnowballC)
library(textreadr)
library(plyr)
library(striprtf)
library(randomForest)
library(mlbench)
library(caret)
library(XML)


## TOKEN API TWITTER ##
# Para hacer uso de la API de Twitter es necesario registrarse como desarrollador y registrar la aplicación para obtener los tokens y la key
# Devuelve un token necesario para la conexión y obtención de información mediante la API
####
token <- create_token(
  app = "my_example",
  consumer_key = "xxxxxxxxxxxxxxxxxxxxx",
  consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  access_token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  access_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")


## CONEXIÓN BBDD ##
# Descripción: Usa la librería mongolite para conectar con la base de datos MongoDB
# Parámetros de entrada: collection: nombre de la colección
#                        db: nombre de la base de datos
# Salida: Objeto que guarda los parámetros de conexión de la base de datos
####
bbddConnect <- function(collection = "prueba", db="local")
{
  bbdd <- mongo(collection = collection, db = db)
  bbdd$count()
  return (bbdd)
}


## OBTENER TWEETS BBDD##
# Descripción: Obtiene los tweets almacenados en BBDD a partir de una cuenta
# Parámetros de entrada: user: nombre de la cuenta
# Salida: Dataframe de dos columnas con el ID y el contenido del tweet
####
getUserTweets <- function(user)
{
  bbdd <- mongo(collection = "prueba", db = 'local')
  userTweets <- bbdd$find(paste0('{ "screen_name" : "',user,'"}'), fields = '{"text":1}')
  colnames(userTweets)[2]<- "content"
  return(userTweets)
}

## OBTENER TWEETS BBDD##
# Descripción: Obtiene los tweets almacenados en BBDD de búsqueda por palabras
# Parámetros de entrada: Ninguno
# Salida: Dataframe de dos columnas con el ID y el contenido del tweet
####
getWordTweets <- function()
{
  bbdd_palabras <- mongo(collection = "tweets_palabras", db = 'local')
  wordTweets <- bbdd_palabras$find(fields = '{"text":1}')
  colnames(wordTweets)[2]<- "content"
  return(wordTweets)
}

## OBTENER TWEETS CUENTA TWITTER##
# Descripción: Obtiene los tweets de una cuenta específica
# Parámetros de entrada: user: nombre de la cuenta de Twitter con el @ incluido
#                        maxtweets: número de tweets a buscar
# Salida: Dataframe de dos columnas con el ID y el contenido del tweet
####
getTweetsbyAccount <- function(user, maxtweets = 100){
  
  bbdd <- bbddConnect("tweets_cuentas","local")
  nombre <- gsub("@","", user)
  
  # Comprobamos si se han obtenido esos tweets anteriormente
  extraer_id <- bbdd$find(paste0('{ "screen_name" : "',nombre,'"}'), fields = '{"status_id":1}', sort = '{"status_id":1}', limit = 1)
  
  # Si no se han extraído, se extraen
  if(nrow(extraer_id) == 0){
    datos_new <- get_timeline(user = usuario, n = maxtweets, parse = TRUE,
                            check = TRUE, include_rts = FALSE)
    bbdd$insert(datos_new)
  
  # Si se han extraído se encuentra el ultimo tweet y extrae a partir de ese punto
  }else{
    ultimo_id <- tail(extraer_id, 1)["status_id"] %>% pull()
    ultimo_id <- as.numeric(ultimo_id)
    ultimo_id = ultimo_id + 1
    ultimo_id <- as.character(ultimo_id)
    datos_new <- get_timeline(user = usuario, n = maxtweets, max_id = ultimo_id,
                            parse = TRUE, check = TRUE, include_rts = FALSE)
    bbdd$insert(datos_new)
  }
  
  tweets = getUserTweets(nombre)
  return(tweets)
}

## OBTENER TWEETS PALABRA TWITTER##
# Descripción: Obtiene los tweets de Twitter de una palabra especifica
# Parámetros de entrada: word: palabra clave
#                        maxtweets: número de tweets a buscar
# Salida: Dataframe de dos columnas con el ID y el contenido del tweet
####
getTweetsbyWord <- function(word, maxtweets = 100){
  bbdd_palabras <- bbddConnect("tweets_palabras","local")
  search_by_word <- search_tweets(paste(word, "-filter:retweets"), n = maxtweets, include_rts = FALSE, type="popular", lang= "es")
  bbdd_palabras$insert(search_by_word)
  tweets = getWordTweets()
  return (tweets)
}

