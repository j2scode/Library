## ---- create_zipf_obj

#==============================================================================#
#                            createZipfObject                                  #
#==============================================================================#
#'  createZipfObject 
#' 
#' This function creates a zipf object given vocabulary growth data from 
#' languageR's growth.fnc or a custom vgc.
#' 
#' @param register - the register for the document
#' @param document - the document for which the growth curves are calculated
#' @param growth - the growth object for the document
#' @author John James
#' @export
createZipfObject <- function(register, document, growth, numSamples, 
                             sampleSize) {
  
  # Read document and extract words
  nTokens <- length(document)
  
  # Create frequency spectrum object and term frequency list
  docSpc    <- text2spc.fnc(document)
  docSpc$m  <- as.integer(docSpc$m)
  docSpc$Vm <- as.integer(docSpc$Vm)
  docTfl    <- spc2tfl(docSpc)
  
  # Prepare Empirical Vocabulary Growth Curve
  docEmpVgc <- growth2vgc.fnc(growth)

  # Prepare extrapolated vocabulary growth curve
  docLnre <- lnre('zm', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  docExtVgc <- lnre.vgc(docLnre, (1:numSamples)*sampleSize)
  
  # Calculate goodness of fit
  g <- lnre.goodness.of.fit(docLnre, docSpc, n.estimated=0, m.max=15)
  goodness <- data.frame(Register = register, X2 = g$X2, P = g$p)

  zipfObject <- list(
    category = register,
    nTokens = nTokens,
    docTfl = docTfl,
    docSpc = docSpc,
    docLnre = docLnre,
    docEmpVgc = docEmpVgc,
    docExtVgc = docExtVgc,
    goodness = goodness)
  
  return(zipfObject)
}
## ---- end