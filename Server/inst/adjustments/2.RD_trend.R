list(
  # Adjustment name ----
  Name = 'Reporting Delays with trend',

  # Adjustment type ----
  Type = 'REPORTING_DELAYS',

  # Adjustment subtype ----
  SubType = 'TREND',

  # Input parameters to the adjustment function ----
  Parameters = list(
    startYear = list(
      label = 'Diagnosis start year',
      value = 2000L,
      input = 'numeric'),
    endYear = list(
      label = 'Notification end year',
      value = 2017,
      input = 'numeric'),
    endQrt = list(
      label = 'Notification end quarter (integer between 1 and 4)',
      value = 1,
      min = 1,
      max = 4,
      input = 'numeric'),
    stratGender = list(
      name = 'stratGender',
      label = 'Gender',
      value = FALSE,
      input = 'checkbox'),
    stratTrans = list(
      name = 'stratTrans',
      label = 'Transmission',
      value = FALSE,
      input = 'checkbox'),
    stratMigr = list(
      name = 'stratMigr',
      label = 'Migration',
      value = FALSE,
      input = 'checkbox')
  ),

  # Names of packages that must be made available to the adjustment function ----
  RequiredPackageNames = c(),

  ## Adjustment function ----
  AdjustmentFunction = function(inputData, parameters) {

    require(ggplot2, quietly = TRUE)
    require(data.table, quietly = TRUE)
    require(survival, quietly = TRUE)

    # A) SETUP -------------------------------------------------------------------------------------

    # Work on a copy
    compData <- copy(inputData)

    # Start year
    startYear <- parameters$startYear + 0.125
    # End quarter
    endQrt <- parameters$endYear + parameters$endQrt / 4 - 0.125
    # Stratifiation columns
    stratVarNames <- c()
    if (parameters$stratGender) {
      stratVarNames <- union(stratVarNames, 'Gender')
    }
    if (parameters$stratTrans) {
      stratVarNames <- union(stratVarNames, 'Transmission')
    }
    if (parameters$stratMigr) {
      stratVarNames <- union(stratVarNames, 'GroupedRegionOfOrigin')
    }
    stratVarNames <- stratVarNames[stratVarNames %in% colnames(compData)]

    # B) PROCESS DATA ------------------------------------------------------------------------------

    # Is this only original data?
    isOriginalData <- compData[, all(Imputation == 0L)]

    # Make sure the strata columns exist in the data
    stratVarNames <- stratVarNames[stratVarNames %in% colnames(compData)]
    stratVarNamesTrend <- union(c('YearOfHIVDiagnosis'), stratVarNames)

    # Create dimensions to match the weights later
    compData[, VarT := as.integer(4L * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime))]
    outputData <- copy(compData)

    # Filter
    compData <- compData[!is.na(VarX)]
    compData[is.na(DiagnosisTime), DiagnosisTime := DateOfDiagnosisYear + 0.125]
    compData[is.na(NotificationTime), NotificationTime := DiagnosisTime + VarX / 4]

    compData <- compData[
      VarX >= 0L &
        DiagnosisTime >= startYear &
        NotificationTime <= endQrt
    ]

    compData[, ':='(
      Tf = as.integer(4L * (pmin.int(MaxNotificationTime, endQrt) - pmax.int(min(DiagnosisTime), startYear))), # nolint
      ReportingDelay = 1L
    )]
    compData[, ':='(
      VarXs = Tf - VarX,
      VarTs = Tf - VarT
    )]
    # NOTE: Otherwise survival model complains
    compData <- droplevels(compData[VarXs > VarTs])

    totalPlot <- NULL
    totalPlotData <- NULL
    stratPlotList <- NULL
    stratPlotListData <- NULL
    rdDistribution <- NULL
    reportTableData <- NULL
    univAnalysis <- NULL
    if (nrow(compData) > 0) {
      # --------------------------------------------------------------------------------------------
      # Prepare diagnostic table based on original data

      mostPrevGender <- compData[
        !is.na(Gender), .N,
        by = .(Gender)
      ][frank(-N, ties.method = 'first') == 1, as.character(Gender)]
      mostPrevTrans <- compData[
        !is.na(Transmission), .N,
        by = .(Transmission)
      ][frank(-N, ties.method = 'first') == 1, as.character(Transmission)]
      mostPrevRegion <- compData[
        !is.na(GroupedRegionOfOrigin), .N,
        by = .(GroupedRegionOfOrigin)
      ][frank(-N, ties.method = 'first') == 1, as.character(GroupedRegionOfOrigin)]

      if (!IsEmptyString(mostPrevGender)) {
        compData[, Gender := relevel(Gender, ref = mostPrevGender)]
      }
      if (!IsEmptyString(mostPrevTrans)) {
        compData[, Transmission := relevel(Transmission, ref = mostPrevTrans)]
      }
      if (!IsEmptyString(mostPrevRegion)) {
        compData[, GroupedRegionOfOrigin := relevel(GroupedRegionOfOrigin, ref = mostPrevRegion)]
      }

      model <- compData[
        Imputation == 0L,
        Surv(time = VarTs, time2 = VarXs, event = ReportingDelay)
      ]

      # Defining univariate models
      univFormulas <- lapply(stratVarNamesTrend, function(x) as.formula(sprintf('model ~ %s', x)))

      # Applying univariate models
      univModels <- lapply(univFormulas, function(x) coxph(x, data = compData[Imputation == 0L]))

      # Extract results of univariable analysis (whether particular covariates
      # are associated with RD)
      univAnalysis <- try(rbindlist(lapply(
        univModels,
        function(x) {
          y <- summary(x)
          z <- cox.zph(x)
          res <- merge(
            as.data.table(y$conf.int),
            as.data.table(y$coefficients)
          )
          res <- cbind(
            res,
            as.data.table(z$table[rownames(z$table) != 'GLOBAL', 'p', drop = FALSE])
          )
          res[, lapply(.SD, signif, 2), .SDcols = colnames(res)]
          setnames(res, c(
            'HR', '1/HR', 'HR.lower.95', 'HR.upper.95', 'Beta', 'SE.Beta', 'Z', 'P.value',
            'Prop.assumpt.p'
          ))

          if (!is.null(x$xlevels)) {
            varName <- names(x$xlevels)[1]
            refLevel <- x$xlevels[[varName]][1]
            compLevels <- x$xlevels[[varName]][-1]
            predictor <- sprintf('%s (%s vs %s)', varName, compLevels, refLevel)
          } else {
            predictor <- rownames(y$conf.int)
          }

          res <- cbind(Predictor = predictor, res)
          return(res)
        }
      )), silent = TRUE)

      if (IsError(univAnalysis)) {
        univAnalysis <- data.table()
      }

      # --------------------------------------------------------------------------------------------
      # RD estimation with time trend

      # Define parameters
      lastYear <- compData[, max(YearOfHIVDiagnosis)]
      years <- (lastYear - 4L):lastYear
      imputations <- compData[, sort(unique(Imputation))]
      formula <- as.formula(
        sprintf(
          'Surv(time = VarTs, time2 = VarXs, event = ReportingDelay) ~ (%s):strata(tgroup)',
          paste(stratVarNamesTrend, collapse = ' + ')
        )
      )
      cuts <- GetCuts(data = copy(compData), count = 3L)
      PrintAlert('Cuts determined: {cuts}')
      tGroups <- seq_along(cuts)

      # Run fitting per imputation separately
      fitStratum <- list()
      for (imputation in imputations) {
        compDataSplit <- data.table::setDT(survival::survSplit(
          formula = Surv(time = VarTs, time2 = VarXs, event = ReportingDelay) ~ .,
          data = compData[Imputation == imputation],
          cut = cuts,
          episode = 'tgroup'
        ))
        compDataSplit[, ':='(
          VarTs = as.integer(VarTs),
          VarXs = as.integer(VarXs),
          ReportingDelay = as.integer(ReportingDelay),
          tgroup = as.integer(tgroup)
        )]
        fitCox <- survival::coxph(formula, data = compDataSplit)
        estFrame <- data.table::CJ(YearOfHIVDiagnosis = years, tgroup = tGroups)
        estCov <- na.omit(unique(compData[, ..stratVarNames]))
        if (nrow(estCov) > 0) {
          estFrame[, MergeDummy := 1L]
          estCov[, MergeDummy := 1L]
          estFrame <- estFrame[estCov, on = .(MergeDummy), allow.cartesian = TRUE]
          estFrame[, MergeDummy := NULL]
        }
        estFrame[, ':='(
          Id = rleid(YearOfHIVDiagnosis),
          VarTs = c(0, cuts)[tgroup],
          VarXs = cuts[tgroup],
          ReportingDelay = 0L
        )]

        fit <- try({
            survival::survfit(formula = fitCox, newdata = estFrame, id = Id)
          },
          silent = TRUE
        )

        if (IsError(fit)) {
          fitStratumImp <- data.table(
            Imputation = imputation,
            Delay = 0L,
            P = 1,
            Weight = 1,
            Var = 0,
            unique(estFrame[, ..stratVarNamesTrend])
          )
        } else {
          fitStratumImp <- data.table(
            Imputation = imputation,
            Delay = fit$time,
            P = fit$surv,
            Weight = 1 / fit$surv,
            Var = fit$std.err^2,
            unique(estFrame[, ..stratVarNamesTrend])[rep(seq_len(.N), fit$strata)]
          )
        }
        fitStratumImp[, VarT := max(Delay) - Delay]

        # Store this imputation results
        fitStratum[[as.character(imputation)]] <- fitStratumImp[VarT >= 0]
      }
      fitStratum <- rbindlist(fitStratum)

      # Merge P, Weight and Var with outputData object
      mergeVars <- union(stratVarNamesTrend, c('VarT', 'Imputation'))
      outputData[, ':='(
        YearOfHIVDiagnosisOrig = YearOfHIVDiagnosis,
        YearOfHIVDiagnosis = pmax.int(lastYear - 4L, YearOfHIVDiagnosis)
      )]
      outputData[
        fitStratum[, c(..mergeVars, 'Weight', 'P', 'Var')],
        ':='(
          Weight = i.Weight,
          P = i.P,
          Var = i.Var
        ),
        on = mergeVars
      ]
      outputData[, ':='(
        YearOfHIVDiagnosis = YearOfHIVDiagnosisOrig,
        YearOfHIVDiagnosisOrig = NULL
      )]
      outputData[, ':='(
        Source = ifelse(Imputation == 0L, 'Reported', 'Imputed'),
        MissingData = is.na(Var) | is.infinite(Var)
      )]
      outputData[MissingData == TRUE, ':='(
        Weight = 1,
        P = 1,
        Var = 0
      )]

      # --------------------------------------------------------------------------------------------

      # Get distribution object as artifact
      varNames <- setdiff(colnames(fitStratum), c('Delay', 'P', 'Weight', 'Var', 'VarT'))
      rdDistribution <- fitStratum[
        VarT > 0,
        union(varNames, c('VarT', 'P', 'Weight', 'Var')),
        with = FALSE
      ]
      setnames(rdDistribution, old = 'VarT', new = 'Quarter')
      setorderv(rdDistribution, union(varNames, 'Quarter'))

      # Aggregate and keep only required dimensions
      agregat <- outputData[, .(
          Count = .N,
          P = mean(P),
          Weight = mean(Weight),
          Var = mean(Var)
        ),
        by = c(mergeVars, 'Source', 'MissingData')
      ]

      # Compute estimated count and its variance
      agregat[, ':='(
        EstCount = Count * Weight,
        EstCountVar = (Count * (Count + 1) / P^4 * Var) + Count * (1 - P) / P^2
      )]

      # C) TOTAL PLOT ------------------------------------------------------------------------------
      totalPlotData <- GetRDPlotData(
        data = agregat,
        by = c('MissingData', 'Source', 'Imputation', 'YearOfHIVDiagnosis')
      )
      setorderv(totalPlotData, c('MissingData', 'YearOfHIVDiagnosis'))
      totalPlot <- GetRDPlots(plotData = totalPlotData, isOriginalData = isOriginalData)

      reportTableData <- dcast(
        totalPlotData[Source == ifelse(isOriginalData, 'Reported', 'Imputed')],
        YearOfHIVDiagnosis + EstCount + LowerEstCount + UpperEstCount ~ MissingData,
        value.var = 'Count',
        fun.aggregate = sum
      )
      if ('TRUE' %in% colnames(reportTableData)) {
        setnames(reportTableData, old = 'TRUE', new = 'RDWeightNotEstimated')
      } else {
        reportTableData[, RDWeightNotEstimated := 0]
      }
      if ('FALSE' %in% colnames(reportTableData)) {
        setnames(reportTableData, old = 'FALSE', new = 'RDWeightEstimated')
      } else {
        reportTableData[, RDWeightEstimated := 0]
      }

      reportTableData <- reportTableData[,
        lapply(.SD, sum),
        by = YearOfHIVDiagnosis,
        .SDcols = setdiff(colnames(reportTableData), 'YearOfHIVDiagnosis')
      ]
      reportTableData[, Reported := RDWeightEstimated + RDWeightNotEstimated]
      reportTableData[, ':='(
        EstUnreported = EstCount - Reported,
        LowerEstUnreported = LowerEstCount - Reported,
        UpperEstUnreported = UpperEstCount - Reported
      )]
      setcolorder(
        reportTableData,
        c(
          'YearOfHIVDiagnosis', 'Reported', 'RDWeightEstimated', 'RDWeightNotEstimated',
          'EstUnreported', 'LowerEstUnreported', 'UpperEstUnreported',
          'EstCount', 'LowerEstCount', 'UpperEstCount'
        )
      )

      # D) STRATIFIED PLOT (OPTIONAL) --------------------------------------------------------------
      if (length(stratVarNames) > 0) {
        # Stratification
        colNames <- union(
          c('MissingData', 'Source', 'YearOfHIVDiagnosis', 'Count', 'EstCount', 'EstCountVar'),
          c(stratVarNamesTrend, 'Imputation')
        )
        # Keep only required columns, convert data to 'long' format...
        agregatLong <- melt(
          agregat[, ..colNames],
          measure.vars = stratVarNames,
          variable.name = 'Stratum',
          value.name = 'StratumValue'
        )
        agregatLong[, StratumValue := factor(StratumValue)]

        stratPlotListData <- GetRDPlotData(
          data = agregatLong,
          by = c(
            'MissingData', 'Source', 'Imputation', 'YearOfHIVDiagnosis', 'Stratum', 'StratumValue'
          )
        )
        stratPlotList <- lapply(
          stratVarNames,
          GetRDPlots,
          plotData = stratPlotListData,
          isOriginalData = isOriginalData
        )

        names(stratPlotList) <- stratVarNames
      }
    } else {
      outputData[, Weight := 1]
    }

    # Keep only columns present in the input object plus the weight
    outColNames <- union(colnames(inputData), c('VarT', 'Weight'))
    outputData <- outputData[, ..outColNames]

    artifacts <- list(
      OutputPlotTotal = totalPlot,
      OutputPlotTotalData = totalPlotData,
      OutputPlotStrat = stratPlotList,
      OutputPlotStratData = stratPlotListData,
      ReportTableData = reportTableData,
      RdDistribution = rdDistribution,
      UnivAnalysis = univAnalysis
    )

    cat('No adjustment specific text outputs.\n')

    return(list(Data = outputData, Artifacts = artifacts))
  }
)
