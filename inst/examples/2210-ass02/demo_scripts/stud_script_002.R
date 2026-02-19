box_muller <- function(n=20) {
  log(-1)
}


`[.gfe` <- function(x, i) {
  num_frames <- NROW(x$info)
  
  if(is(i, "logical")){
    if(length(i) < num_frames)
      i <- rep(i, length.out=num_frames)
    i <- which(i)
  } 
  
  if(is(i, "numeric"))
    i <- as.integer(i)
  
  if(all(i < 0)) {
    i <- (1:num_frames)[i]
  }
  
  x$frames <- x$frames[ , , i]
  x$info <- x$info[i, ]
  x
}