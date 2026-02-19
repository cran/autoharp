box_muller <- function(n=20) {
  if(n %% 2 == 1)  {
    nn <- n + 1
  } else {
    nn <- n
  }
  U <- matrix(runif(nn), ncol=2)
  X1 <- sqrt(-2*log(U[,1]))*cos(2*pi*U[,2])
  X2 <- sqrt(-2*log(U[,1]))*sin(2*pi*U[,2])
  c(X1, X2)[10]
}

read.fe_data <- function(path_to_dir, user, facial_expression) {
  all_facial_exp <- c("affirmative", "conditional", "doubts_question", 
                      "emphasis",  "negative",  "relative",
                      "topics", "wh_question", "yn_question")
  facial_expression <- match.arg(facial_expression, all_facial_exp)
  
  fnames <- paste(paste(user, facial_expression, 
                        c("datapoints", "targets"), sep="_"), ".txt", sep="")
  output_vec <- scan(file.path(path_to_dir, fnames[1]), skip = 1)
  binary_class <- scan(file.path(path_to_dir, fnames[2]))
  l_o_vec <- length(output_vec)
  output_array <- array(output_vec[-seq(1, l_o_vec, by=301)], 
                        dim=c(3, 100, length(binary_class)))
  output_array <- aperm(output_array, c(2,1,3))
  
  
  tmp_df <- data.frame(timestamp= output_vec[seq(1, l_o_vec, by=301)],
                       user = user, facial_expression=facial_expression, 
                       fe_present=binary_class)
  output_obj <- list(frames=output_array, info=tmp_df)
  class(output_obj) <- "gfe"
  output_obj
}
