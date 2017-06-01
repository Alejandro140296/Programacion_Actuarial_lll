RankHospital <- function(estado,resultado,num = "mejor"){
    setwd("C:/Users/Acer/Documents/GitHub/Programacion_Actuarial_lll/Caso2")
    leer <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    std <- as.character(leer$State)
    if(!(estado %in% std)){
        stop("estado invalido")
        
    }
    
    if(!(resultado %in% c("ataque","falla cardiaca","neumonia"))){
        stop("resultado invalido")
    }
    if(resultado == "ataque"){
        f <- subset(leer,std == estado)
        extrac <- f[,11]
        extrac <- suppressWarnings(as.numeric(extrac))
    }else if (resultado == "falla cardiaca"){
        f <- subset(leer,std == estado)
        extrac <- f[,17]
        extrac <- suppressWarnings(as.numeric(extrac))
    }else if(resultado == "neumonia"){
        f <- subset(leer,std == estado)
        extrac <- f[,23]
        extrac <- suppressWarnings(as.numeric(extrac))
    }
    if(num=="peor"){
        maximo<-f[which.max(extrac),]
        peores<-maximo[order(maximo$Hospital.Name, na.last = NA),]
        peorfinal<-peores[1,2]
        peorfinal
    }else if(num=="mejor"){
        minimo<-f[which.min(extrac),]
        mejores<-minimo[order(minimo$Hospital.Name, na.last = NA),]
        mejorfinal<-mejores[1,2]
        mejorfinal
    }else{
        
        aleatorios<- order(extrac,f$Hospital.Name, na.last = NA)
        al <- f[aleatorios,2]
        aleatoriofinal <- al[num]
        aleatoriofinal
    }
}
RankHospital("MN","ataque",5000)
