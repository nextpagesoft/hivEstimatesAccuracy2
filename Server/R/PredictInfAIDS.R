PredictInfAIDS <- function(
  baseAIDS,
  params
) {
  baseAIDS <- copy(baseAIDS)

  xAIDS <- cbind(
    1,
    as.integer(baseAIDS$Gender == 'Male'),
    baseAIDS$AgeDiag
  )

  # Posterior mean
  baseAIDS[, ProbPre := NA_real_]
  for (i in seq_len(nrow(baseAIDS))) {
    fit1 <- try(integrate(
      VPostWAIDS,
      lower = baseAIDS$Mig[i],
      upper = baseAIDS$U[i],
      x = xAIDS[i, ],
      dTime = baseAIDS$DTime[i],
      betaAIDS = params$betaAIDS,
      kappa = params$kappa
    ), silent = TRUE)

    fit2 <- try(integrate(
      VPostWAIDS,
      lower = 0,
      upper = baseAIDS$U[i],
      x = xAIDS[i, ],
      dTime = baseAIDS$DTime[i],
      betaAIDS = params$betaAIDS,
      kappa = params$kappa
    ), silent = TRUE)

    if (IsError(fit1) || IsError(fit2) || fit1$message != 'OK' || fit2$message != 'OK') {
      next
    } else {
      baseAIDS[i, ProbPre := fit1$value / fit2$value]
    }
  }

  return(baseAIDS)
}
