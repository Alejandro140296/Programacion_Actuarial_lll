RankingCompleto <- function(resultado,num = "mejor"){
    setwd("C:/Users/Acer/Documents/GitHub/Programacion_Actuarial_lll/Caso2")
    leer <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    std <- as.character(leer$State)
    if(!(resultado %in% c("ataque","falla cardiaca","neumonia"))){
        stop("resultado invalido")
    }
    
    
    if(resultado == "ataque"){
        f <- 11
    }else if (resultado == "falla cardiaca"){
        f <- 17
    }else if(resultado == "neumonia"){
        f <- 23
    }
    v <- c()
    
    
    mydata <- data.frame(leer[,2],leer[,7],leer[,f])
    mydata <- suppressWarnings(mydata[order(leer[,2],leer[,7],na.last = NA),])
    mysp <- split(mydata,mydata[,2])
    
    for (k in 1:54){
        aux <- num
        numeros <- as.numeric(mydata[,2])
        aux2 <- length(which(k == numeros))
        dat1 <- as.data.frame(mysp[k])
        mysp <- dat1[order(as.numeric(as.vector(dat1[,3])),dat1[,1]),]
        
        if(num == "mejor"){
            aux <- 1
        }
        if(num == "peor"){
            aux <- aux2
        }
        
        mysp <- data.frame(mysp[aux,1], mysp[1,2])
        v <- rbind(v,mysp)
        mysp <- split(mydata,mydata[,2])
        num <- num
    }
    
    colnames(v) <- c("Hospital","State")
    v
    
}
head(RankingCompleto("ataque",20),10)
length(std)
