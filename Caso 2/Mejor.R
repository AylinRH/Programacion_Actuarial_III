mejor<- function(estado, resultado){
    directorio <-  setwd("C:/Users/Aylín/Desktop/Calidad de Hospitales - data")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    prueba1 <- factor(outcome$State)
    z <- levels(prueba1)
    y1<- c()
    for(i in 1:length(z)){
        if(estado==z[i]){
            y1<- c(y1, i)
        }
    }
    if(length(y1)==0) stop("Estado inválido")
    prueba2 <- factor(c("ataque", "falla", "neumonia"))
    z2 <- levels(prueba2)
    y2 <- c()
    for(i in 1:length(z2)){
        if(resultado==z2[i]){
            y2<- c(y2, i)
        }
    }
    if(length(y2)==0) stop("Resultado inválido")
    extraccion <- split(outcome, outcome$State==estado)
    s <- extraccion$'TRUE'
    if(resultado=="ataque"){
        padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    } else if(resultado=="falla"){
        padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    } else{
        padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    }
    conversion<-suppressWarnings(as.numeric(padecimiento)) 
    x <- s$Hospital.Name[conversion==min(conversion, na.rm=T)]
    h <- complete.cases(x)
    hospital <- x[h]
    if(length(hospital)>1){
        alfabetico <- sort(hospital)
        primero <- alfabetico[1]
    }
    hospital
}
