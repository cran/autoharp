## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(autoharp)

## ----th-example-1, echo=TRUE--------------------------------------------------
tree1 <- TreeHarp(quote(lm(y ~ x1 + x2, data=mydata)), TRUE)

## ----th-example-1-plot, echo=TRUE, fig.align='center', collapse=TRUE, fig.cap='Example TreeHarp object'----
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color="gray", vertex.frame.color=NA)
par(opar)

## ----th1-adjlist--------------------------------------------------------------
slot(tree1, "adjList")

## ----th1-nodetypes, echo=TRUE-------------------------------------------------
get_node_types(tree1)

## ----ex1_call-----------------------------------------------------------------
slot(tree1, "call")

## ----ex1_repr-----------------------------------------------------------------
tree1

## ----th-example-2-plot, echo=TRUE, fig.align='center', fig.cap='TreeHarp object with colored nodes'----
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color=tree1@nodeTypes$call_status)
par(opar)

