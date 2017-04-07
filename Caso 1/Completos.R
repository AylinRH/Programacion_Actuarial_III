completos <- function(directorio, id){
    ID <- c()
    NOBS <- c()
    encabezados <- c("ID", "Datos completos")
    directorio<- setwd("C:/Users/Aylín/Desktop/specdata")
    for(i in id){
        if(i <= 9){
            cadena <- paste("00", i, ".", "csv", sep = "")
        } else if (i <= 99) {
            cadena <- paste("0", i, ".", "csv", sep = "")
        } else {
            cadena <- paste(i, ".", "csv", sep="")
        }
        data<-read.table(cadena, header = TRUE, sep=",")
        datos <- data[c("sulfate", "nitrate")]
        datos_modificada <- na.omit(datos)
        n_renglones <- nrow(datos_modificada)
        ID <- c(ID, i)
        NOBS <- c(NOBS, n_renglones)
    }
    data.frame(ID, NOBS)
}


