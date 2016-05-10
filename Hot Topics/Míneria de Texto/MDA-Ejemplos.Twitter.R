
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Twitter

Juan Esteban Mejía Velásquez

'''

# cargar librerias

if(!require(c("twitteR", "tm", "wordcloud"), quietly=TRUE))
  install.packages(c("twitteR", "tm", "wordcloud"))

library(twitteR)
library(tm)
library(wordcloud)

'''
api_key<- "Tú.api.key"
api_secret<- "Tú api secret"
access_token<- "Tú token de acceso"
access_token_secret<- "Tú token de acceso secreto"
setup_twitter_oauth
(api_key,api_secret,access_token,access_token_secret)

'''

api_key<- "CUOCEeddxxp0gMumAZUqHoe3u"
api_secret<- "PXCXNaB71mZvRVQLmR1DXq6ynz57btq0526xpKlnlJJmLgMzk9"
access_token<- "3348999459-kxjLrZYkbtNps4GS18iEPt9gynzoHVacPTZbwND"
access_token_secret<- "lVFAZsaJuPWa1g0iFYI9n3nO4lcg7BUZ2rOS9zBjH4AfA"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


## Extración de Tweets por tópico

Tweets.Paz = searchTwitter("Paz", since='2014-01-01')

head(Tweets.Paz, 5)

str(Tweets.Paz)

## Tópicos en tendencia

Locs = availableTrendLocations()

LocsColombia = subset(Locs, country == "Colombia")

woeidColombia = subset(LocsColombia, name == "Colombia")$woeid

trends = getTrends(woeid=woeidColombia)

head(trends)

## Ejemplo de Twietero

# recolecta tweets de @camila_vallejo
tweets = userTimeline("JuanManSantos", 2015)

# vuelca la informacion de los tweets a un data frame
df = twListToDF(tweets)

# obtiene el texto de los tweets
txt = df$text

### inicio limpieza de datos 

# remueve retweets

txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)

# remove @otragente

txtclean = gsub("@\\w+", "", txtclean)

# remueve simbolos de puntuación

txtclean = gsub("[[:punct:]]", "", txtclean)

# remove números

txtclean = gsub("[[:digit:]]", "", txtclean)

# remueve links

txtclean = gsub("http\\w+", "", txtclean)


### construye un corpus

corpus = Corpus(VectorSource(txtclean))

# convierte a minúsculas

corpus = tm_map(corpus, tolower)

# remueve palabras vacías (stopwords) en español

corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), "JuanManSantos"))

# carga archivo de palabras vacías personalizada y lo convierte a ASCII

sw <- readLines("stopwords.es.txt",encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")

# remueve palabras vacías personalizada

corpus = tm_map(corpus, removeWords, sw)

# remove espacios en blanco extras

corpus = tm_map(corpus, stripWhitespace)

# Inspeccionar el corpues

inspect(corpus[1])

corpus <- tm_map(corpus, PlainTextDocument)

# crea una matriz de términos

tdm = TermDocumentMatrix(corpus)
tdm
# convierte a una matriz

m = as.matrix(tdm)

# conteo de palabras en orden decreciente

wf <- sort(rowSums(m),decreasing=TRUE)

# crea un data frame con las palabras y sus frecuencias

dm <- data.frame(word = names(wf), freq=wf)
dm

# grafica la nube de palabras (wordcloud)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
