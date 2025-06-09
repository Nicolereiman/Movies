library(tidyverse)
library(missForest)
library(rpart)
library(rpart.plot)
library(funModeling)
library(mice)
library(rpart)
library(rpart.plot)
library(lubridate)
library(modeest)

dt= read_csv('netflix_titles.csv')
view(dt)

## Introducción ##



## Cantidad de variables ##

cant_variables <- dim(dt)[2]
cant_registros <- dim(dt)[1]


## Descripcion de las variables ##

summary(dt$show_id)
describe(dt$show_id)

summary(dt$type)
describe(dt$type)
dt %>% ggplot(aes(x=release_year, fill=type))+geom_boxplot()
dt %>% ggplot(aes(release_year, fill=type))+geom_bar(alpha=0.7)

summary(dt$title)
describe(dt$title)

summary(dt$director)
describe(dt$director)

summary(dt$cast)
describe(dt$cast)

summary(dt$country)
describe(dt$country)

summary(dt$release_year)
describe(dt$release_year)
plot(dt$release_year)
hist(dt$release_year)
boxplot(dt$release_year)

summary(dt$date_added)
describe(dt$date_added)

summary(dt$rating)
describe(dt$rating)

summary(dt$duration)
describe(dt$duration)

summary(dt$listed_in)
describe(dt$listed_in)

summary(dt$description)
describe(dt$description)


## Variables elegidas para el análisis ##

dtimp = dt %>% 
select(type, country, date_added, release_year, duration, listed_in, rating)
dtimp

## Agregamos una columna para hacer numerica la variable duracion ##

durac= strsplit(dt$duration,split=" ")

durapeli=c()
for(i in 1:length(durac)){
  if(dtimp[[1]][i] =="Movie")
    durapeli = append(durapeli, durac[[i]][[1]])
  else{durapeli=append(durapeli, NA)}
}
durapeli

duraserie=c()
for(i in 1:length(durac)){
  if(dtimp[[1]][i] !="Movie")
    duraserie = append(duraserie, durac[[i]][[1]])
  else{duraserie=append(duraserie,NA)}                    
}
duraserie

dt = dt %>% mutate(serie=strtoi(duraserie))
dt = dt %>% mutate(pelicula=strtoi(durapeli))
view(dt)

dtimp = dtimp %>% mutate(serie=strtoi(duraserie))
dtimp = dtimp %>% mutate(pelicula=strtoi(durapeli))

## Resumen y analisis de las nuevas variables ##

##Numericas##

describe(dtimp$release_year)
quantile(dtimp$release_year, c(0.75))- quantile(dtimp$release_year, c(0.25))

describe(dtimp$date_added)
quantile(dtimp$date_added, c(0.75), na.rm = T)- quantile(dtimp$date_added, c(0.25), na.rm = T)

describe(dtimp$pelicula)
quantile(dtimp$pelicula, c(0.75), na.rm=T)- quantile(dtimp$pelicula, c(0.25), na.rm = T)

describe(dtimp$serie)
quantile(dtimp$serie, c(0.75), na.rm=T)- quantile(dtimp$serie, c(0.25), na.rm = T)

boxplot(dt$pelicula)
boxplot(dt$serie)
boxplot(dt$release_year)

##Categoricas##

mlv(dtimp$type)
dt %>% distinct(type)

mlv(dtimp$country)
dt %>% distinct(country) %>% count()

mlv(dtimp$listed_in)
dt %>% distinct(listed_in) %>% count()


mlv(dtimp$rating)
dt %>% distinct(rating) %>% count()


#Duracion de las peliculas:

summary(dt$pelicula)
describe(dt$pelicula)
plot(dt$pelicula)
dt %>% ggplot(aes(x=pelicula, fill=type))+geom_boxplot()
dt %>% ggplot(aes(pelicula))+ geom_histogram()
dt %>% ggplot(aes(duration, fill=release_year))+geom_bar(alpha=0.7)

dt = dt %>%
  mutate(pelicula_discreta = 
           case_when(
            pelicula<87 ~ "Corta",
            pelicula<312 ~ "Normal",
            T ~ "Larga"))
tabla_pelicula_año=table(dt$release_year,dt$pelicula_discreta)
dt %>% 
  ggplot(aes(x=pelicula_discreta, y=release_year)) +
  geom_boxplot()+ 
  ylab("Añó que salió") + xlab("Tipo de pelicula")

#Duracion de las series: 

summary(dt$serie)
describe(dt$serie)
plot(dt$serie)
dt %>% ggplot(aes(x=serie, fill=type))+geom_boxplot()
dt %>% ggplot(aes(serie))+ geom_histogram()

dt = dt %>%
  mutate(serie_discreta = 
           case_when(
            serie<1 ~ "Corta",
            serie<2 ~ "Normal",
            T ~ "Larga"))
tabla_serie_año=table(dt$release_year,dt$serie_discreta)
dt %>% 
  ggplot(aes(x=serie_discreta, y=release_year)) +
  geom_boxplot()+ 
  ylab("Añó que salió") + xlab("Tipo de serie")


## Missings ##

contar_m(dt$show_id)
contar_m(dt$type)
contar_m(dt$title)
contar_m(dt$director)
contar_m(dt$cast)
contar_m(dt$country)
contar_m(dt$date_added)
contar_m(dt$release_year)
contar_m(dt$rating)
contar_m(dt$duration)
contar_m(dt$listed_in)
contar_m(dt$description)

## Outliers ##
# Contamos cuantos hay por variable, y si un registro es o no #

vectrelease=es.outlier(dtimp$release_year)
outlrelease=cant_outliers(vectrelease)
vectrelease
outlrelease

vecminpelis=es.outlier(dt$pelicula)
vecminpelis
vecminpelis=vecminpelis[!is.na(vecminpelis)]
vecminpelis
outlape=cant_outliers(vecminpelis)
outlape

vecminserie=es.outlier(dt$serie)
vecminserie
vecminserie=vecminserie[!is.na(vecminserie)]
vecminserie
outlase=cant_outliers(vecminserie)
outlase


## Para imputar otuliers con la mediana ##

mediana_year = median(dt$release_year,na.rm=T)
dt$release_year[es_outliera(dt$release_year)] = mediana_year


## Analisis de la base ##

# ¿Existe correlacion entre la duracion de la pelicula y
#el año en que salian.?
cor.test(dt$pelicula,dt$release_year)
plot(dt$release_year,dt$pelicula)

# Las peliculas en promedio son mas viejas que las series?
release_peli=dt$release_year[dt$type=="Movie"]
release_serie=dt$release_year[dt$type=="TV Show"]
t.test(release_peli,release_serie,alternative = 'less')

# El rating depende de si es pelicula o serie? 
chisq.test(table(dt$rating,dt$type))
#Se evaluara si el rating depende de si es serie o 
#pelicula, asi evaluando si se deberia hacer un analisis de que 
#rating es mas frecuente por ejemplo.
#Se rechaza Ho que es que el rating del cinema sea independiente del tipo
#Por ende se debe tomar en cuenta que hay una relacion fuerte.
#Sera necesario indagar directamente con cual



## Funciones ##

contar_m=function(x){
  cantidad=0
  for( i in x){
    if (is.na(i)){
      cantidad= cantidad +1
    }
  }
  return(cantidad)
}

cant_outliers= function(x){
  cont=0
  for (i in x) {
    if(i==FALSE){
      cont=cont+1
    }
  }
  return(cont)
}

es.outlier=function(x){
  sup = quantile(x,0.75,na.rm=T)+IQR(x,na.rm=T)*1.5
  inf = quantile(x,0.25,na.rm=T)-IQR(x,na.rm=T)*1.5
  es=between(x,inf,sup)
  return(es)
}

es_outliera=function(x){
  es=(x<1895|x>2022)
  return(es)
}



