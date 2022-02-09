library("HMM")
library("entropy")

set.seed(12345)

states = 1:3
symbols = 1:2

transMatrix <- matrix(c(0.9, 0.1, 0,
                        0, 0, 1,
                        0.2, 0, 0.8),
                      nrow = length(states),
                      ncol = length(states),
                      byrow = TRUE)

emissMatrix <- matrix(c(0.6, 0.4,
                        0.3, 0.7,
                        0.3, 0.7),
                      nrow = length(states),
                      ncol = length(symbols),
                      byrow = TRUE)

startMatrix <- c(0.5, 0.5, 0)

hmm <- initHMM(states, symbols, startMatrix, transMatrix, emissMatrix)
simHMM(hmm, 100)
