mytranspose <- function(x) {
  if(is.null(x)){
    y <- NULL
    return(y)
  }else if(is.null(nrow(x))){
    x <- matrix(x, nrow = 1)
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    return(y)  
  }else if(ncol(x) == 0){
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    y = x
    return(y)
  }else if(is.data.frame(x)){
    col <- colnames(x)
    row <- rownames(x)
    x <- as.matrix(x)
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    y <- as.data.frame(y)
    colnames(y) <- row
    rownames(y) <- col
    return(y) 
  }else{
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    return(y)    
  }
  
}

