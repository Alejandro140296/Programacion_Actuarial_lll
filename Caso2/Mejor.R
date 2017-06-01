mejor <- function(estado,resultado){
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
        minimo <- f[which.min(extrac),]
        mejores <- minimo[order(minimo$Hospital.Name, na.last = NA),]
        mejorfinal <- mejores[1,2]
        mejorfinal
    }else if (resultado == "falla cardiaca"){
        f <- subset(leer,std == estado)
        extrac <- f[,17]
        extrac <- suppressWarnings(as.numeric(extrac))
        minimo <- f[which.min(extrac),]
        mejores <- minimo[order(minimo$Hospital.Name, na.last = NA),]
        mejorfinal <- mejores[1,2]
        mejorfinal
    }else if(resultado == "neumonia"){
        f <- subset(leer,std == estado)
        extrac <- f[,23]
        extrac <- suppressWarnings(as.numeric(extrac))
        minimo <- f[which.min(extrac),]
        mejores <- minimo[order(minimo$Hospital.Name, na.last = NA),]
        mejorfinal <- mejores[1,2]
        mejorfinal
    }
    
}
mejor("MD","neumonia")
