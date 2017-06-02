mejor<- function(estado, resultado){
    directorio <-  setwd("C:/Users/Aylín/Desktop/Calidad de Hospitales - data")
    outcome <- read.csv("outcome-of-care-measures.csv")
    z <- levels(outcome$State)
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
    if(resultado=="ataque"){
        outcome[,11] <- suppressWarnings(as.numeric(levels(outcome[,11])[outcome[,11]]))
        padecimiento <- 11
    } else if(resultado=="falla"){
        outcome[,17] <- suppressWarnings(as.numeric(levels(outcome[,17])[outcome[,17]]))
        padecimiento <- 17
    } else{
        outcome[,23] <- suppressWarnings(as.numeric(levels(outcome[,23])[outcome[,23]]))
        padecimiento <- 23
    }
    outcome[,2] <- as.character(outcome[ ,2])
    hospital <- outcome[outcome$State==estado, ][order(outcome[outcome$State==estado, ][, padecimiento], outcome[outcome$State==estado, ][,2], na.last=NA), ]
    hospital[1,2]
}
mejor("TX","falla")
