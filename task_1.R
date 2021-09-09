################################################################################
## LAB 1: Graphical models
## ANDREAS HERTIN andhe794
## TDDE15 - Advanced Machine Learning
## LiU 2021

#################################### SETUP #####################################

## Install if not installed
## if (!requireNamespace("BiocManager", quietly = TRUE))
##   install.packages("BiocManager")
## BiocManager::install("RBGL")
## BiocManager::install("Rgraphviz")
## BiocManager::install("gRain")

## Import packages
## library(RGBL)
## library(Rgraphviz)
## library(gRain)

## Import data
data("asia")
## Variables in data:
## A, S, T, L, B, E, X, D

################################################################################
## Task 1

set.seed(1)
HC_1 <- hc(x = asia,
           score = "bde",
           restart = 3)
plot(HC_1)

set.seed(1)
HC_2 <- hc(x = asia,
           score = "bde",
           restart = 1000)
plot(HC_2)

all.equal(HC_1, HC_2)

HC_3 <- hc(x = asia,
           score = "bic",
           restart = 1000)
## plot(HC_2)

vstructs(HC_1)
vstructs(HC_2)
vstructs(HC_3)

arcs(HC_1)
arcs(HC_2)
arcs(HC_3)

cpdag(HC_0)
cpdag(HC_1)
cpdag(HC_2)
cpdag(HC_3)




