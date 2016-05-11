'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Twitter

Juan Esteban Mejía Velásquez

'''

# https://developers.facebook.com/apps/ 

if(!require(Rfacebook, quietly=TRUE))install.packages('Rfacebook')

library(Rfacebook)

library(devtools)

install_github("Rfacebook", "pablobarbera", subdir = "Rfacebook")
  
'''
fb_oauth<-fbOAuth(app_id="Tú.id",
                  app_secret="Tú.clave",
                  extended_permissions = TRUE)
'''

fb_oauth<-fbOAuth(app_id="908456369263071",
                  app_secret="c6731801aae8f5361fbe16960119d2e8",
                  extended_permissions = TRUE)

# Guardar y cargar a aplicación

save(fb_oauth, file="fb_oauth")
load("fb_oauth")

token = "EAACEdEose0cBAIP1ZCJv6WDqzoMvHxD3rIz2ZAXeZASVNpfH83ivhcijZB7eHcZBTCXa3mMtj5PnlZBU4abXg9NBQ6JWb3S9zeyYy3CXrQclB74DFBZCUNgZAEFV5lnaO0GxtljZBmEdQSNWkW3vOi6mQm1bYhHHQn5eJvsUIhe2TlwZDZD"

me <- getUsers("me", token, private_info = TRUE)

me$name

Amigos =  getFriends(token , simplify = FALSE)

head(Amigos, n=5)

Amigos.info <- getUsers(Amigos$id, token, private_info=TRUE)

table(Amigos.info$relationship_status)


my_network <- getNetwork(token, format="adj.matrix")
singletons <- rowSums(my_network)==0 

require(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=2, 
     #vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout)


### Analís de datos de una pagina de Facebook

page <- getPage("AsoBuitresUN", token, n = 5000)

## Convertir al formato de 'tiempo' en R
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## Agregar matrix con cuestas por mes
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# Crear data frame conel promedio de la matrix con cuestas por mes
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# Visualizar la evolucion de la metrica
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
       scale_x_date(breaks = "years", labels = date_format("%Y")) + scale_y_log10("Cuenta promedio por post", 
       breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())

# Otros analsis de la pagina
post_id <- head(page$id, n = 1)  
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = FALSE)
users <- getUsers(post$likes$from_id, token)
table(users$gender)