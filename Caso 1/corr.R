corr <- function(directorio, horizonte){
    sulfato <- c()
    nitrato <- c()
    correlacion <- c()
    archivos <- c()
    directorio<- setwd("C:/Users/Aylín/Desktop/specdata")
    for(i in 1:332){
        if(i <= 9){
            cadena <- paste("00", i, ".", "csv", sep = "")
        } else if(i <=99) {
            cadena <- paste("0", i, ".", "csv", sep = "")
        } else{
            cadena <- paste(i, ".", "csv", sep="")
        }
        data<-read.table(cadena, header = TRUE, sep=",")
        datos <- data[c("sulfate", "nitrate")]
        datos_modificada <- na.omit(datos)
        x <- nrow(datos_modificada)
        
        if(x>horizonte){
            sulfato <- datos_modificada["sulfate"]
            nitrato <- datos_modificada["nitrate"]
            y <- cor(sulfato, nitrato)
            correlacion <- c(correlacion, y)
            archivos <- c(archivos, datos_modificada )
        }
    }
    if(length(archivos)==0){
        print(0)
    } else{
        print(correlacion)
    }
}

