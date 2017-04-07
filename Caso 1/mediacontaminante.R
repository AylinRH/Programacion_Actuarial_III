#Ls valores que tomarán los contaminantes son los siguientes:
#Sulfato=1
#nitrato=2
options(max.print=1000000)
mediacontaminante <- function(directorio, contaminante, ID){
    directorio<- setwd("C:/Users/Aylín/Desktop/specdata")
    vector<- c()
    b <- c()
    directorio<- setwd("C:/Users/Aylín/Desktop/specdata")
    for(i in ID){
        if(i <= 9){
            cadena <- paste("00", i, ".", "csv", sep = "")
        } else if (i <= 99) {
            cadena <- paste("0", i, ".", "csv", sep = "")
        } else {
            cadena <- paste(i, ".", "csv", sep="")
        }
        data<-read.table(cadena, header = TRUE, sep=",")
        
        if(contaminante == 1){
            y <- data["sulfate"]
        } else{
            y <- data["nitrate"]
        }
        a <- y[!is.na(y)]
        b <- c(b, a)
        suma <- sum(a)
        vector<- c(vector, suma)
        
    }
    sum(vector)/length(b)
}

