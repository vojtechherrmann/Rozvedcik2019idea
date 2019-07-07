
testFramework <- function() {
  ### test framework
  trials = 10000
  vysledek = numeric(trials)
  for (t in 1:trials) {
    name = "Vojta"
    gffp <- generateFunctionForPerson(name)
    vysledek[t] = evaluateFunctionForPerson(gffp, 1000)
  }
  return(vysledek)
}





mainFramework <- function() {
  name = "Vojta"
  
  gffp <- generateFunctionForPerson (name)
  
  png(paste0(name, "_formula.png"), height = 500, width = 1000)
  plot.new()
  generateFormulaPlot(gffp)
  dev.off()
  
  N = 876543
  
  SKUPINA = evaluateFunctionForPerson(gffp, N)
  
  png(paste0(name, "_skupina.png"))
  plot.new()
  generateSkupinaPlot(SKUPINA)
  dev.off()
}
