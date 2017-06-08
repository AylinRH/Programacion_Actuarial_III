setwd("C:/Users/Aylín/Desktop/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

#Primera parte
xtest <- read.table("./test/X_test.txt")
xtrain <- read.table("./train/X_train.txt")
ytest <- read.table("./test/y_test.txt")
ytrain <- read.table("./train/y_train.txt")

x <- rbind(xtest, xtrain)
y <- rbind(ytest, ytrain)


#segunda parte
caracteristicas <- read.table("features.txt")[,2]
colnames(x)<-as.character(caracteristicas)
recolector<-c()
numero=1
for (i in 1:17){
    
    if (0<i &&i<=5){
        
        for (w in 1:6){
            z<-as.character(caracteristicas[numero])
            numero<-numero + 1
            recolector<-c(recolector,z)
            
        }
        numero<-numero+34
    }
    
    if (5<i && i<=10){
        for (w in 1:2){
            z<-as.character(caracteristicas[numero])
            numero<-numero + 1
            recolector<-c(recolector,z)
            
        }
        numero<-numero+11
    }
    if (10<i && i<=13){
        for (w in 1:6){
            z<-as.character(caracteristicas[numero])
            numero<-numero + 1
            recolector<-c(recolector,z)
            
        }
        numero<-numero+73      
    }
    if (i>13){
        for (w in 1:2){
            z<-as.character(caracteristicas[numero])
            numero<-numero + 1
            recolector<-c(recolector,z)
            
        }
        numero<-numero+11          
    }
    
}
extraccion<-x[recolector]


#tercera parte
actividades <- levels(read.table("activity_labels.txt")[,2])
y <- as.factor(y[,1])
levels(y) <- actividades
Actividad <- y


#cuarta parte
sujetostest <- read.table("./test/subject_test.txt")
sujetostrain <- read.table("./train/subject_train.txt")
Sujetos <- rbind(sujetostest, sujetostrain)
colnames(Sujetos) <- "Sujetos"
tabla_final <- cbind(Actividad, extraccion, Sujetos)


#quinta parte
library(dplyr)
Datos_ordenados <- tabla_final %>%
                        group_by(Actividad, Sujetos)
                        summarise_each(
                            Datos_ordenados,funs(mean)
                        )


