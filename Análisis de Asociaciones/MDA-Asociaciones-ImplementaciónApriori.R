
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Creación de Algoritmo Apriori

Juan Esteban Mejía Velásquez

'''

Apriori <- function (data, I, MIN_SUP, parameter = NULL){
  f <- CrearItemsets()
  c <- EncontrartItemsetFrecuente(data,I,1, MIN_SUP)
  k <- 2
  Longitud <- ObtenerTamanoData(data)
  while( !EsVacio(c[[k-1]]) ){
    f[[k]] <- AprioriGen(c[k-1])
    for( idx in 1: Longitud ){
      ft <- ObtenerSubSet(f[[k]],data[[idx]])
      len4ft <- ObtenerTamanoData(ft)
      for( jdx in 1:Longitud ){
        AumentarSupportCount(f[[k]],ft[jdx])
      }
    }
    c[[k]] <- EncontrartItemsetFrecuente(f[[k]],I,k,MIN_SUP)
    k <- k+1
  }
  c
}

#Para la simplificación del agoritmo se craron funciones por fuera 


CrearItemsets <- function(data,base_items){
  list(cbind(diag(length(base_items)),apply(data,2,sum)))
}


AprioriGen <- function(c,k){
  ck <- c[[k]][,-ncol(c[[k]])]
  f <- NULL
  len <- nrow(ck)
  for(idx in seq(nrow(ck))){
    jdx <- idx+1
    while(idx<jdx && jdx<=len){
      a <- ck[idx,]
      b <- ck[jdx,]
      if( k==1 || identical(a[1:(k-1)],b[1:(k-1)]) ){
        ab <- ifelse(a+b,1,0)
        if( !Podar(ck,ab,k) ){
          f <- rbind(f,ab)
        }else{
           print("Podado")
        }
      }
      jdx <- jdx + 1
    }
  }
  
  if(length(f)){
    f <- cbind(f,rep(0,dim(f)[1]))
    rownames(f) <- NULL
  }
  return(f)
}

Podar <- function(ck,ab,k){
  ck <- rbind(ck,ab)
  len <- dim(ck)[1]
  for(idx in which(ab>0)){
    temp <- ab
    temp[idx] <- 0
    for(idx in seq(len)){
      if(identical(temp,ck[idx,]))break
    }
    if(idx==len)return(TRUE)
  }
  return(FALSE)
}

EsVacio <- function(ck,k){
  return(ifelse(nrow(ck[[k]])>0,FALSE,TRUE))
}

AumentarSupportCount <- function(fk,data){
  w4f <- ncol(fk)
  len4f <- nrow(fk)
  len4d <- nrow(data)
  for(idx in seq(len4d)){
    for(jdx in seq(len4f)){
      if(identical(fk[jdx,-w4f],fk[jdx,-w4f]*data[idx,])){
        fk[jdx,w4f] <- fk[jdx,w4f] + 1
      }
    }
  }
  return(fk)
}


ObtenerTamanoData <- function(data){
  return( nrow(data) )
}


EncontrartItemsetFrecuente <- function(fk,base_items,k,MIN_SUP){
  data <- fk[[k]]
  return(data[data[,dim(data)[2]]>MIN_SUP,])
}

# Obtencion de itemset frecuente

itemsets.Frecuente <- Apriori(itemsets,items,min_sup)
print(itemsets.Frecuente)
