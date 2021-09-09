################################################################################
## LAB 1: Graphical models
## ANDREAS HERTIN andhe794
## TDDE15 - Advanced Machine Learning
## LiU 2021

#################################### SETUP #####################################

## Install if not installed
## if (!requireNamespace("BiocManager", quietly = TRUE))
##  install.packages("BiocManager")
## BiocManager::install("RBGL")
## BiocManager::install("Rgraphviz")
## BiocManager::install("gRain")

## Import packages
## library(RGBL)
## library(Rgraphviz)
library(gRain)
library(bnlearn)

## Import data
data("asia")
## Variables in data:
## A, S, T, L, B, E, X, D

################################################################################
## Task 1

set.seed(1)
HC_0 <- hc(x = asia)

set.seed(1)
HC_1 <- hc(x = asia,
           score = "bde",
           restart = 3)
# plot(HC_1)

set.seed(1)
HC_2 <- hc(x = asia,
           score = "bde",
           restart = 1000)
# plot(HC_2)

set.seed(1)
HC_3 <- hc(x = asia,
           score = "bic",
           restart = 1000)
# plot(HC_2)

all.equal(HC_0, HC_1)
all.equal(HC_0, HC_2)
all.equal(HC_0, HC_3)

vstructs(HC_1)
vstructs(HC_2)
vstructs(HC_3)

arcs(HC_1)
arcs(HC_2)
arcs(HC_3)

cpdag(HC_1)
cpdag(HC_2)
cpdag(HC_3)

################################################################################
## Task 2

predictNet <- function(juncTree, data, features, target){
  predArray <- matrix(nrow = nrow(data),
                      ncol = 1)
  
  for (i in 1:nrow(data)) {
    obsStates <- NULL
    for (j in features) {
      obsStates[j] <- if(data[i, j] == "yes") "yes" else "no"
    }
    
    obsEvidence <- setEvidence(object = juncTree,
                               nodes = features,
                               states = obsStates)
    
    obsPredProb <- querygrain(object = obsEvidence,
                              nodes = target)$S
    
    predArray[i] <- if(obsPredProb["yes"] >= 0.5) "yes" else "no"
  }
  
  return(predArray)
  
}


trainingIndices = sample(1:dim(asia)[1],
                         floor(dim(asia)[1] * 0.8))

asia.train <- asia[trainingIndices,]
asia.test <- asia[-trainingIndices,]

set.seed(1)
HC_BN_test <- hc(x = asia.train,
                 score = "bde",
                 restart = 3)

HC_BN_true <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# plot(HC_BN_test)
# plot(HC_BN_true)

fitTest <- bn.fit(HC_BN_test,
                  asia.train)

fitTrue <- bn.fit(HC_BN_true,
                  asia.train)

grainTest <- as.grain(fitTest)
grainTrue <- as.grain(fitTrue)

fitTestJunctionTree <- compile(grainTest)
fitTrueJunctionTree <- compile(grainTrue)

observedVars = c("A", "T", "L", "B", "E", "X", "D")
targetVar    = c("S")

predTest <- predictNet(fitTestJunctionTree,
                       asia.test,
                       observedVars,
                       targetVar)

predTrue <- predictNet(fitTestJunctionTree,
                       asia.test,
                       observedVars,
                       targetVar)

confMatrixTest <- table(predTest, 
                        asia.test$S)

confMatrixTrue <- table(predTrue, 
                        asia.test$S)

print(confMatrixTest)
print(confMatrixTrue)

################################################################################
## Task 3

# mbTest <- mb(HC_BN_test,
#              node = "S")
mbTrue <- mb(fitTrue,
             node = "S")

print(mbTrue)

# predMbTest <- predictNet(fitTrueJunctionTree,
#                          asia.test,
#                          mbTest,
#                          targetVar)

predMbTrue <- predictNet(fitTrueJunctionTree,
                         asia.test,
                         mbTrue,
                         targetVar)

# confMatrixMbTest <- table(predMbTest,
#                           asia.test$s)

# confMatrixMbTrue <- table(predMbTrue,
#                           asia.test$s)

# This gives error:
# "all arguments must have the same length" ???

# print(confMatrixMbTest)
# print(confMatrixMbTrue)


################################################################################
## Task 4

set.seed(1)

naiveBN <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]")

fitNaive <- bn.fit(naiveBN,
                   asia.train)

grainNaive <- as.grain(fitNaive)

junctionNaive <- compile(grainNaive)

predNaive <- predictNet(junctionNaive,
                        asia.test,
                        observedVars,
                        targetVar)

confMatrixNaive <- table(predNaive,
                         asia.test$s)

print(confMatrixNaive)









