rankingcompleto <- function(resultado, num="mejor"){
    hospitales<-c()
    estados<-c()
    directorio <-  setwd("C:/Users/Aylín/Desktop/Calidad de Hospitales - data")
    outcome <- read.csv("outcome-of-care-measures.csv")
    z <- levels(factor(c("ataque", "neumonia", "falla")))
    y <- c()
    for(i in 1:length(z)){
        if(resultado==z[i]){
            y<- c(y, i)
        }
    }
    if(length(y)==0) stop("Resultado inválido")
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
    estado <- levels(outcome$State)
    for(est in 1:length(estado)){
        y <- estado[est]
        ordenado <- outcome[outcome$State==y, ][order(outcome[outcome$State==y, ][, padecimiento], outcome[outcome$State==y, ][,2], na.last=NA), ]
        if(num=="mejor"){
            num=1
        }else if(num=="peor"){
            num=length(ordenado)
        }
        hospitales <- c(hospitales, ordenado[num, 2])
        estados <- c(estados, y)
    }
    DT <- data.frame(hospitales, estados)
    row.names(DT) <- estados
    DT  
}
    