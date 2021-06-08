PredictInfCD4VL <- function(
  baseCD4VL,
  params
) {
  baseCD4VL <- copy(baseCD4VL)

  baseCD4VL[, Ord := seq_len(.N), by = .(Id)]

  baseCD4VL[, ProbPre := NA_real_]
  pb <- progress::progress_bar$new(
    format = '[:bar] :current/:total (:percent, ETA: :eta)',
    total = length(ids)
  )
  pb$tick(0)
  # patient <- baseCD4VL[, unique(Patient)][1]
  for (patient in baseCD4VL[, unique(Patient)]) {
    patientData <- baseCD4VL[Patient == patient]

    migTime <- patientData[Ord == 1, Mig]
    upTime <- patientData[Ord == 1, U]
    x <- patientData[, .(Gender, GroupedRegion, Mode, AgeDiag, DTime, Calendar, Consc, Consr)]
    y <- patientData[, .(YVar)]
    z <- patientData[, .(Consc, CobsTime, Consr, RobsTime, RLogObsTime2, DTime)]
    xAIDS <- as.matrix(patientData[Ord == 1, .(1, as.integer(Gender == 'Male'), AgeDiag)])
    maxDTime <- patientData[, max(DTime)]

    if (patientData[1, KnownPrePost] != 'Unknown') {
      next
    }

    switch(
      patientData[1, Only],
      'Both' = {
        bFE <- params$bFE
        sigma2 <- params$sigma2
        varCovRE <- params$varCovRE
      },
      'CD4 only' = {
        bFE <- params$bFECD4
        sigma2 <- params$sigma2CD4
        varCovRE <- params$varCovRECD4
      },
      'VL only' = {
        bFE <- params$bFEVL
        sigma2 <- params$sigma2VL
        varCovRE <- params$varCovREVL
      }
    )

    fit1 <- try(integrate(
      VPostWCD4,
      lower = migTime,
      upper = upTime,
      x = x,
      y = y,
      z = z,
      xAIDS = xAIDS,
      maxDTime = maxDTime,
      betaAIDS = params$betaAIDS,
      kappa = params$kappa,
      bFE = bFE,
      sigma2 = sigma2,
      varCovRE = varCovRE
    ), silent = TRUE)
    fit2 <- try(integrate(
      VPostWCD4,
      lower = 0,
      upper = upTime,
      x = x,
      y = y,
      z = z,
      xAIDS = xAIDS,
      maxDTime = maxDTime,
      betaAIDS = params$betaAIDS,
      kappa = params$kappa,
      bFE = bFE,
      sigma2 = sigma2,
      varCovRE = varCovRE
    ), silent = TRUE)

    if (IsError(fit1) || IsError(fit2) || fit1$message != 'OK' || fit2$message != 'OK') {
      next
    } else {
      baseCD4VL[Patient == patient, ProbPre := fit1$value / fit2$value]
    }
    pb$tick(1)
  }

  return(baseCD4VL)
}
