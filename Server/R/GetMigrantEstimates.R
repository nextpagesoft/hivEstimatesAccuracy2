GetMigrantEstimates <- function(

) {
  # Estimates of the fixed effects parameters from the bivariate LMM applied to CASCADE data
  fixedEffectsBivCASCADE <- read.csv(GetSystemFile('extdata/fixedEffectsBivCASCADE.csv'))
  bFE <- fixedEffectsBivCASCADE[, 1]
  names(bFE) <- rownames(fixedEffectsBivCASCADE)

  bFECD4 <- bFE[1:27]
  bFEVL <- bFE[28:length(bFE)]

  # Variance-covariance parameters
  varCovBivCASCADE <- read.csv(GetSystemFile('extdata/varCovBivCASCADE.csv'))
  varCovRE <- as.matrix(varCovBivCASCADE[1:5, 1:5])

  varCovRECD4 <- varCovRE[1:2, 1:2]
  varCovREVL <- varCovRE[3:5, 3:5]

  sigma2 <- unlist(varCovBivCASCADE[1, 6:7])
  sigma2CD4 <- sigma2[1]
  sigma2VL <- sigma2[2]

  fitAids <- readRDS(GetSystemFile('extdata/fitAids.rds'))
  betaAids <- -coef(fitAids) / fitAids$scale
  kappa <- 1 / fitAids$scale

  return(NULL)
}
