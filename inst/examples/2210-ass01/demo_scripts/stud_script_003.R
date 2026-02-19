akima_data <- readRDS("../data/assignment01.rds")
missing_id <- which(is.na(akima_data[,1]))
akima_data[missing_id, 1] <- mean(akima_data[(missing_id-1):(missing_id+1),1], 
                                  na.rm=TRUE)

compute_slopes <- function(x, y){
  m <- (y[-1]-y[-5])/(x[-1]-x[-5])
  r_diff <- abs(m[4]-m[3])
  l_diff <- abs(m[2]-m[1])
  
  #if(m[1] == m[2] && m[4] == m[3]){
  if(m[1] != m[2] || m[4] != m[3]){
    mid_slope <- (r_diff*m[2] + l_diff*m[3])/(l_diff + r_diff)
  } else {
    mid_slope <- (1/2)*(m[2] + m[3])
  }
  mid_slope
}

est_poly <- function(x, y, slopes, n=50) {
  x_vals <- seq(x[1], x[2], length.out=n)
  p <- c(y[1], slopes[1], 
         (3*(y[2]-y[1])/(x[2]-x[1]) - 2*slopes[1] - slopes[2])/(x[2]-x[1]),
         (slopes[1] + slopes[2] -2*(y[2]-y[1])/(x[2]-x[1]))/(x[2]-x[1])^2)
  unname(cbind(x_vals, p[1] + p[2]*(x_vals-x[1]) + p[3]*(x_vals-x[1])^2 + p[4]*(x_vals-x[1])^3))
}
