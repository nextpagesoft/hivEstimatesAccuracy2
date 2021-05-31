PredictInfCD4VL <- function(
  baseCD4VL,
  bFE,
  sigma2,
  varCovRE
) {
  set.seed(10)

  baseCD4VL[, Ord := seq_len(.N), by = .(Id)]

  # Get the design matrices
  x <- split(
    baseCD4VL[, .(Id, Gender, GroupedRegion, Mode, AgeDiag, DTime, Calendar, Consc, Consr)],
    by = c('Id'),
    keep.by = FALSE
  )

  z <- split(
    baseCD4VL[, .(Id, Consc, CobsTime, Consr, RobsTime, RLogObsTime2, DTime)],
    by = c('Id'),
    keep.by = FALSE
  )

  y <- split(baseCD4VL[, .(Id, YVar)], by = c('Id'), keep.by = FALSE)

  u <- split(baseCD4VL[, .(Id, U)], by = c('Id'), keep.by = FALSE)

  ids <- unique(baseCD4VL$Id)
  only <- split(baseCD4VL[, .(Id, Only)], by = c('Id'), keep.by = FALSE)
  mig <- split(baseCD4VL[, .(Id, Mig)], by = c('Id'), keep.by = FALSE)
  known <- split(baseCD4VL[, .(Id, KnownPrePost)], by = c('Id'), keep.by = FALSE)

  # One row per subject
  baseCD4VLId <- baseCD4VL[Ord == 1]

  xAIDS <- cbind(
    1,
    as.integer(baseCD4VLId$Gender == 'Male'),
    baseCD4VLId$AgeDiag
  )
  maxDTime <- baseCD4VL[, .(DTime = max(DTime)), by = .(Id)]
  ind <- list()
  for (i in seq_along(unique(ids))) {
    ind[[i]] <- which(baseCD4VL$Id == ids[i])
  }

  baseCD4VL[, ProbPre := NA]

  for (i in seq_along(unique(ids))) {
    upTime <- u[[i]][1, U]
    migTime <- mig[[i]][1, Mig]

    if (known[[i]][1, KnownPrePost] != 'Unknown') {
      next
    }

    if (only[[i]][1, Only] == 'Both') {
      fit1 <- try(integrate(
        VpostW2,
        lower = migTime,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)
      fit2 <- try(integrate(
        VpostW2,
        lower = 0,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)

      if (IsError(fit1) || IsError(fit2)) {
        next
      } else {
        res <- fit1$value / fit2$value
      }

      if (fit1$message == 'OK' && fit2$message == 'OK') {
        baseCD4VL[ind[[i]], ProbPre := res]
      }
    }

    if (only[[i]][1, Only] == 'CD4 only') {
      fit1 <- try(integrate(
        VpostWCD42,
        lower = migTime,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)
      fit2 <- try(integrate(
        VpostWCD42,
        lower = 0,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)

      if (IsError(fit1) || IsError(fit2)) {
        next
      } else {
        res <- fit1$value / fit2$value
      }

      if (fit1$message == 'OK' && fit2$message == 'OK') {
        baseCD4VL[ind[[i]], ProbPre := res]
      }
    }

  #   if (only[[i]][1] == "VL only") {
  #     uptime <- u[[i]][1]
  #     migtime <- mig[[i]][1]

  #     fit1 <- try(integrate(VpostWvl, lower = migtime, upper = uptime), silent = T)
  #     fit2 <- try(integrate(VpostWvl, lower = 0, upper = uptime), silent = T)

  #     if ("try-error" %in% class(fit1) | "try-error" %in% class(fit2)) {
  #       next
  #     } else {
  #       res <- fit1$value / fit2$value
  #     }

  #     if (fit2$message == "OK" & fit1$message == "OK") {
  #       baseCD4VL$ProbPre[ind[[i]]] <- res
  #     }
  #   }
  }

}
