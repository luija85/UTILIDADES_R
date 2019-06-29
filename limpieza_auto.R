#' ANTILLAS ANALYTICS
#' 
#' FUNCIÓN DE IMPUTACIÓN AUTOMÁTICA:
#' SE CREA UNA FUNCIÓN PARA REALIZAR LA IMPUTACIÓN AUTOMÁTICA DE UN data.frame:
#' 
#' 
#' 
#' 
#' 
#' 
library(tidyverse)

v1<- sample(150,20)
v2<- sample(150,20)
v3<- sample(150,20)
iris_nas <- iris
iris_nas[v1,1] <- NA
iris_nas[v2,2] <- NA
iris_nas[v3,3] <- NA


# IMPUTAMOS CON tidyverse, AGREGANDO CANTIDAD DE PERDIDOS
imputacion <- function(x) ifelse(is.na(x), mean(x,na.rm=T)+sd(x,na.rm=T)*rnorm(sum(is.na(x))),x)
iris_nas %>% group_by(Species) %>% nest() %>% 
  mutate(Sepal.Length = map(data, ~ imputacion(.x$Sepal.Length)),) %>% 
  mutate(Sepal.Width = map(data, ~ imputacion(.x$Sepal.Width)),) %>% 
  mutate(Petal.Length = map(data, ~ imputacion(.x$Petal.Length)),) %>% 
  unnest(Sepal.Length,Sepal.Width,Petal.Length,.drop=T) %>%
  mutate(cuenta1 = ifelse(is.na(iris_nas$Sepal.Length),1,0)) %>%
  mutate(cuenta2 = ifelse(is.na(iris_nas$Sepal.Width),1,0)) %>%
  mutate(cuenta3 = ifelse(is.na(iris_nas$Petal.Length),1,0)) %>%
  mutate(cuenta = cuenta1+cuenta2+cuenta3) -> iris_nas2



funcion_imputacion <- function(df,v,gr){
    df %>% group_by(eval(gr,df)) %>% nest() %>% 
    mutate(imput = map(data, ~ imputacion(eval(v,.x)))) %>%
    unnest(imput,.drop=T) #%>%  .[1:nrow(df),] %>%
    #mutate(cuenta = ifelse(is.na(eval(v,df)),1,0)) %>% 
    #select(imput,cuenta)
}
prueba<-funcion_imputacion(iris_nas,Sepal.Length,Species)
unique(prueba)

iris_nas %>% group_by(eval(Species,iris)) %>% nest() %>%
  mutate(imput = map(data, ~ imputacion(.x$Sepal.Length))) %>%
  unnest(imput,.drop=T)
?eval
