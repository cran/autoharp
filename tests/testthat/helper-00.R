# Valid adjacency lists for TreeHarp:
th1 <- list(a= c(2L,3L), b=NULL, c=NULL)
th2 <- list(a= c(2L,3L), b=4L, c=NULL, d=NULL)
th3 <- list(a= c(2L,3L), b=NULL, c=4L, d=NULL)
th3a <- list(a= c(2L,3L,4L), b=NULL, c=c(5L, 6L), d=7L, e=NULL, f=NULL, g=NULL)
th00 <- list(leaf=NULL)

# Invalid adjacency list (disconnected):
th4 <- list(a=c(2L,3L), b=NULL, c=NULL, d=NULL)
# Invalid adjacency list (cyclic, and each edge represented twice):
th5 <- list(a=c(2L,3L), b=c(1L,4L), c=c(1L,4L), d=c(2L,3L))
# Invalid adjacency list (cyclic):
th6 <- list(a=c(2L,3L), b=4L, c=4L, d=NULL)
# Invalid adjacency list (not in BFS order):
th7 <- list(a = c(3L,4L,6L), f = NULL, c = c(2L,5L), b=NULL, e = NULL, d = NULL)
th7a <- list(a=c(2L, 4L), b=NULL, d=NULL, c=3L)
# Invalid adjacency list (unlabelled):
th8 <- list(c(2,3), b=NULL, c=NULL)

# Valid adjacency matrix:
m1 <- matrix(0L, 3, 3)
dimnames(m1) <- list(letters[1:3], letters[1:3])
m1[1, ] <- c(0, 1L, 1L)
m1[, 1] <- c(0, 1L, 1L)
# Invalid adjacency matrix:
m1a <- m1
dimnames(m1a) <- NULL

# Invalid adjacency matrix (contains cycle):
m2 <- matrix(c(0, 1, 1, 0,
               1, 0, 0, 1,
               1, 0, 0, 1,
               0, 1, 1, 0), nrow=4, byrow = TRUE,
             dimnames = list(letters[1:4], letters[1:4]))

e1 <- new.env()

## Language objects

ex1 <- quote(ggplot(data) + geom_point(aes(x=x1, y=y1)))
ex2 <- quote(x)
ex3 <- quote(x <- 1)
ex4 <- quote(x <- NA)
