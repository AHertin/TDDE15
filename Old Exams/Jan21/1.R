library(bnlearn)

data("lizards")

lizardsnet<-model2network("[Species][Diameter|Species][Height|Species]") # True DAG
plot(lizardsnet)
plot(cpdag(lizardsnet)) # Plot the true pattern

ci.test("Diameter", "Species", test = "x2", data = lizards) # Keep edge D-S.
ci.test("Height", "Species", test = "x2", data = lizards) # Keep edge H-S.
ci.test("Height", "Diameter", test = "x2", data = lizards) # Remove edge D-H.
ci.test("Diameter", "Species", "Height", test = "x2", data = lizards) # Keep edge D-S.
ci.test("Height", "Species", "Diameter", test = "x2", data = lizards) # Keep edge H-S.

# Orientate D->S<-H. Wrong model !
