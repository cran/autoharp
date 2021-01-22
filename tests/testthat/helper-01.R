tho1 <- TreeHarp(th1)
tho2 <- TreeHarp(th2)
tho3 <- TreeHarp(th3)
tho3a <- TreeHarp(th3a)
tho00 <- TreeHarp(th00)

l1 <- list(a=c(2,3), b=NULL, c=4, d=NULL)
l2 <- list(a=c(2,3), b=NULL, d=NULL)
l3 <- list(a=c(2,3), b=NULL, c=NULL)

l4 <- list(a=c(2,3), b=NULL, b=NULL)
l5 <- list(a=c(2,3), b=NULL, b=4, c=NULL)
l6 <- list(a=c(2,3,4), b=NULL, b=5, d=NULL, c=NULL)
l7 <- list(a=c(2,3,4), b=NULL, b=NULL, b=NULL)

t1 <- TreeHarp(l1)
t2 <- TreeHarp(l2)
t3 <- TreeHarp(l3)

t4 <- TreeHarp(l4)
t5 <- TreeHarp(l5)
t6 <- TreeHarp(l6)
t7 <- TreeHarp(l7)
