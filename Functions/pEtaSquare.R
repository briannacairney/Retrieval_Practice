# Function to calculate Partial Eta Squared 

pEtaSquare <- function(x) {
  x$ANOVA["SSn"] / (x$ANOVA["SSn"] + x$ANOVA["SSd"])
}
