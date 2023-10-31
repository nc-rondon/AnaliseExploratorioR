#LISTA

print('#########################  LISTA  ######################')
bd<- c(5,6,4,1,3,2,2,3,5,2,0,8,12,7,15)
print(bd)

#Fórmulas

media <- function(x){coef<-sum(x)/length(x)
return(coef)}

mediana <- function(x) {
   x <- sort(x)
   if (length(x) %% 2 == 0) {
     e1 <- x[length(x) %/% 2]
     e2 <- x[(length(x) %/% 2) + 1]
     return((e1 + e2) / 2)
   } else {
       return(x[(length(x) %/% 2) + 1])
     }
 }

varianciaP<-function(x){
  soma = 0
  for( i in 1:length(x)){
    soma <- soma + (x[i]- media(x))**2}
  soma1 <- soma/length(x)
  return (soma1)}

varianciaA<-function(x){
  soma = 0
  for(i in 1:length(x)){
    soma = soma + (x[i]- media(x))**2}
  soma1 = soma/(length(x)-1)
  return(soma1)}

desvioPP<-function(x){
  soma=0
  for(i in 1:length(x)){
    soma = soma + (x[i]- media(x))**2}
  V = (soma/length(x))**0.5
  return(V)
}

desvioPA<-function(x){
  soma = 0
  for (i in 1:length(x)){
    soma = soma + (x[i]- media(x))**2}
  v = (soma/(length(x) - 1))**0.5
return (v)}

coefVP <- function(x){
  c = (desvioPP(x)/media(x))*100
  return(c)
}

coefVA <- function(x){
  c = (desvioPA(x)/media(x))*100
  return(c)
}

q1 <- function(x){
  x<-sort(x)
  m<-mediana(x)
  while(max(x)>=m){
    x <- x [-which.max(x)]
  }
  return(mediana(x))
}
q2 <- function(x){
  m<-mediana(x)
  return(m)
}

q3 <- function(x){
  x<-sort(x)
  m<-mediana(x)
  while(min(x)<=m){
    x <- x [-which.min(x)]
  }
  return(mediana(x))
}

#Funções pré definidas

print('#################  Funções pré definidas  ##################')
cat('Lista:')
print(bd)
print('Média:')
mean(bd)
print('Mediana:')
median(bd)
va<-var(bd)
vp<- var(bd)*(length(bd)-1)/length(bd) 
print('Variância Amostral:')
print(va)
print('Variância Populacional:')
print(vp)
print('Desvio de Padrão Amostral:')
sd(bd)
sdp<-function(x){s<-sqrt((length(bd)-1)/length(bd)) * sd(bd)
      return(s)}
print('Desvio de padrão populacional:')
sdp(bd)
cva <- function(x){coef<-sd(x)/mean(x)*100
return(coef)}
cvp <- function(x){coef<-sdp(x)/mean(x)*100
        return(coef)}

print('Coeficiente de Variação Amostral:')
cva(bd)
print('Coeficiente de Variação Populacional:')
cvp(bd)
print('Quartis:')
quantile(bd)
boxplot(bd)
hist(bd)


# FEITA POR MIM

print('###############  Feito por mim  ##################')
print('Média:')
media(bd)
print('Mediana:')
mediana(bd)
print('Variância Amostral:')
varianciaA(bd)
print('Variância Populacional:')
varianciaP(bd)
print('Desvio de Padrão Amostral:')
desvioPA(bd)
print('Coeficiente de Variação Populacional:')
desvioPP(bd)
print('Coeficiente de Variação Amostral:')
coefVA(bd)
print('Coeficiente de Variação Populacional:')
coefVP(bd)
print('Quartis')
q1(bd)
q2(bd)
q3(bd)
# OPERAÇÔES COM O ARQUIVO CSV
print('##############  OPERAÇÔES COM O ARQUIVO CSV  ###########')

print('##################  FUNÇÕES PRÉ DEFINIDAS  #############')
df<-read.csv("dataset.csv")  
BD<-df$value
print('Média:')
mean(BD)
print('Mediana:')
median(BD)
va<-var(BD)
vp<- var(BD)*(length(BD)-1)/length(BD) 
print('Variância Amostral:')
print(va)
print('Variância Populacional:')
print(vp)
print('Desvio de Padrão Amostral:')
sd(BD)
sdp<-function(x){s<-sqrt((length(BD)-1)/length(BD)) * sd(BD)
      return(s)}
print('Desvio de padrão populacional:')
sdp(BD)
cva <- function(x){coef<-sd(x)/mean(x)*100
return(coef)}
cvp <- function(x){coef<-sdp(x)/mean(x)*100
        return(coef)}

print('Coeficiente de Variação Amostral:')
cva(BD)
print('Coeficiente de Variação Populacional:')
cvp(BD)
print('Quartis:')
quantile(BD)
boxplot(BD)
hist(BD)
print('###############  Feito por mim  ##################')
print('Média:')
media(BD)
print('Mediana:')
mediana(BD)
print('Variância Amostral:')
varianciaA(BD)
print('Variância Populacional:')
varianciaP(BD)
print('Desvio de Padrão Amostral:')
desvioPA(BD)
print('Coeficiente de Variação Populacional:')
desvioPP(BD)
print('Coeficiente de Variação Amostral:')
coefVA(BD)
print('Coeficiente de Variação Populacional:')
coefVP(BD)
print('Quartis')
print(q1(BD))
print(q2(BD))
print(q3(BD))