rankingcompleto <- function(resultado, num="mejor"){
    directorio <-  setwd("C:/Users/Aylín/Desktop/Calidad de Hospitales - data")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    prueba <- factor(c("ataque", "falla", "neumonia"))
    z <- levels(prueba)
    y <- c()
    for(i in 1:length(z)){
        if(resultado==z[i]){
            y<- c(y, i)
        }
    }
    hospitales<-c()
    estados<-c()
    estado <- factor(outcome$State)
    abreviaturas <- levels(estado)
    for(est in 1:length(abreviaturas)){
        extraccion <- split(outcome, outcome$State==abreviaturas[est])
        s <- extraccion$'TRUE'
        if(resultado=="ataque"){
            padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        } else if(resultado=="falla"){
            padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        } else{
            padecimiento <- s$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        }
        completos <- padecimiento[!(padecimiento=="Not Available")]
        ordenados<- sort(as.numeric(completos))
        if(num=="mejor"){
            num=1
        }else if(num=="peor"){
            num=length(ordenados)
        }
        tasa <- ordenados[num]
        hospital <- s$Hospital.Name[suppressWarnings(as.numeric(padecimiento))==tasa]
        if(length(hospital)>1){
            h <- sort(hospital)
            hospital=h[1]
        }
        hospitales <- c(hospitales, hospital)
        estados <- c(estados, abreviaturas[est])
    }
    DT <- data.frame(hospitales, estados)
    row.names(DT) <- estados
    DT
}
