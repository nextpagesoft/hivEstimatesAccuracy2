#' HIVModelManager
#'
#' R6 class for representing the HIV Model manaager
#'
#' @name HIVModelManager
#' @examples
#' hivModelMgr <- HIVModelManager$new()
NULL

#' @export
HIVModelManager <- R6::R6Class( # nolint
  classname = 'HIVModelManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL,
      appMgr = NULL
    ) {
      private$AppMgr <- appMgr
      private$Session <- session
      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        MigrConnFlag = FALSE,
        PopCombination = NULL,
        AggrDataSelection = NULL,
        MainFitTask = NULL,
        MainFitResult = NULL,
        AvgModelOutputs = NULL,
        Years = NULL,
        BootstrapFitTask = NULL,
        BootstrapFitResult = NULL,
        BootstrapFitStats = NULL,
        PlotData = NULL
      )
    },

    print = function() {
      print(self$Session)
    },

    # USER ACTIONS =================================================================================

    SetMigrConnFlag = function(
      migrConnFlag
    ) {
      private$Catalogs$MigrConnFlag <- migrConnFlag
      return(invisible(self))
    },

    LoadParameters = function(
      xmlModel
    ) {
      status <- 'SUCCESS'
      msg <- 'Parameters loaded correctly'
      tryCatch({
        params <- ParseXMLModel(xmlModel)
      }, error = function(e) {
        status <<- 'FAIL'
        msg <<-
          paste(
            'There was a difficulty encountered when reading the parameters file.',
            'They have not been loaded.'
          )
      })

      if (status == 'SUCCESS') {
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          Params = params
        )
      } else {
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('MODELS_PARAMS_LOADED', payload)

      return(invisible(self))
    },

    DetermineYearRanges = function() {
      status <- 'SUCCESS'
      msg <- 'Allowed parameters determined'
      tryCatch({
        # Data
        caseData <- FilterCaseBasedData(
          private$AppMgr$CaseMgr$PreProcessedData,
          private$AppMgr$CaseMgr$Filters
        )
        aggrData <- private$AppMgr$AggrMgr$Data

        # Combination 'All data'
        popCombination <- list(
          Case = NULL,
          Aggr = private$AppMgr$AggrMgr$PopulationNames
        )

        # Aggregated data filters
        aggrDataSelection <- private$Catalogs$AggrDataSelection

        res <- GetPopulationData(caseData, aggrData, popCombination, aggrDataSelection)
        caseDataAll <- PrepareDataSetsForModel(caseData)
        dataSets <- CombineData(caseDataAll, res$Aggr)[[1]]
        dataSets <- Filter(function(dt) nrow(dt) > 0, dataSets)
        optimalYears <- hivModelling::GetAllowedYearRanges(dataSets)
        rangeYears <- lapply(dataSets, function(dt) dt[, c(min(Year), max(Year))])
      }, error = function(e) {
        status <<- 'FAIL'
        msg <<- 'There was a difficulty encountered when determining year ranges.'
      })

      if (status == 'SUCCESS') {
        private$Catalogs$Years <- list(
          Range = rangeYears,
          Optimal = optimalYears
        )
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          Years = private$Catalogs$Years
        )
      } else {
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('MODELS_YEAR_RANGES_DETERMINED', payload)
      PrintAlert('HIV model year ranges determined')
      return(invisible(self))
    },

    SetAggrFilters = function(
      aggrFilters
    ) {
      private$Catalogs$AggrDataSelection <- aggrFilters
      PrintAlert('Aggregated data filters set')

      self$DetermineYearRanges()
    },

    RunMainFit = function(
      settings = list(),
      parameters = list(),
      popCombination = NULL
    ) {
      if (!any(is.element(
        private$AppMgr$Steps[c('CASE_BASED_READ', 'AGGR_READ')],
        private$AppMgr$CompletedSteps
      ))) {
        PrintAlert(
          'Data must be read before running main fit of the HIV model',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting HIV Model main fit task')

        private$Catalogs$MainFitTask <- Task$new(
          function(
            caseData,
            aggrData,
            settings,
            parameters,
            popCombination,
            aggrDataSelection,
            migrConnFlag,
            randomSeed
          ) {
            if (!require('hivPlatform', quietly = TRUE)) {
              suppressMessages(pkgload::load_all())
            }

            options(width = 120)
            .Random.seed <- randomSeed # nolint

            PrintH1('Parameters')

            PrintH2('Migrant module connection')
            PrintAlert('Enabled: {ifelse(migrConnFlag, "Yes", "No")}')

            if (length(popCombination$Aggr) > 0) {
              PrintH2('Aggregated data selection')
              cli::cli_text(
                '{.val {cat(capture.output(aggrDataSelection), sep = "\n")}}'
              )
            }

            PrintH2('Population combination')
            if (!is.null(caseData)) {
              if (length(popCombination$CaseAbbr) > 0) {
                PrintAlert('Case-based populations: {.val {popCombination$CaseAbbr}}')
              } else {
                PrintAlert('Case-based populations: All data available')
              }
            } else {
              PrintAlert('Case-based populations: None')
            }

            if (!is.null(aggrData) && length(popCombination$Aggr) > 0) {
              PrintAlert('Aggregated populations: {.val {popCombination$Aggr}}')
            } else {
              PrintAlert('Aggregated populations: None')
            }

            PrintH2('Time intervals and diagnosis rates modelling')
            cli::cli_text(
              '{.val {cat(capture.output(parameters$Intervals), sep = "\n")}}'
            )

            PrintH2('Advanced paramaters')
            # nolint start
            PrintAlert('Range of calculations                                : {parameters$ModelMinYear} - {parameters$ModelMaxYear}')
            PrintAlert('HIV diagnoses, total                                 : {parameters$FitPosMinYear} - {parameters$FitPosMaxYear}')
            PrintAlert('HIV diagnoses, by CD4 count                          : {parameters$FitPosCD4MinYear} - {parameters$FitPosCD4MaxYear}')
            PrintAlert('AIDS diagnoses, total                                : {parameters$FitAIDSMinYear} - {parameters$FitAIDSMaxYear}')
            PrintAlert('HIV/AIDS diagnoses, total                            : {parameters$FitAIDSPosMinYear} - {parameters$FitAIDSPosMaxYear}')
            PrintAlert('Do you have data from the start of the epidemic      : {ifelse(parameters$FullData, "Yes", "No")}')
            PrintAlert('Knots count                                          : {parameters$ModelNoKnots}')
            PrintAlert('Start at zero                                        : {ifelse(parameters$StartIncZero, "Yes", "No")}')
            PrintAlert('Prevent sudden changes at end of observation interval: {ifelse(parameters$MaxIncCorr, "Yes", "No")}')
            PrintAlert('Maximum likelihood distribution                      : {parameters$FitDistribution}')
            PrintAlert('Extra diagnosis rate due to non-AIDS symptoms        : {parameters$Delta4Fac}')
            PrintAlert('Country-specific settings                            : {parameters$Country}')
            # nolint end

            if (!is.null(caseData)) {
              caseData <- caseData[FinalData == TRUE]
              if (!('Weight' %in% colnames(caseData))) {
                caseData[, Weight := 1]
              }
            }
            dataAfterMigr <- 'ProbPre' %in% colnames(caseData)

            if (dataAfterMigr && migrConnFlag) {
              stopifnot(caseData[!is.na(Excluded), is.na(unique(ProbPre))])
              stopifnot(caseData[is.na(Excluded) & KnownPrePost == 'Pre', unique(ProbPre) == 1])
              stopifnot(caseData[is.na(Excluded) & KnownPrePost == 'Post', unique(ProbPre) == 0])

              caseData[, MigrClass := data.table::fcase(
                !is.na(DateOfArrival) & DateOfHIVDiagnosis < DateOfArrival, 'Diagnosed prior to arrival', # nolint
                !is.na(ProbPre) & ProbPre >= 0.5, 'Infected in the country of origin',
                !is.na(ProbPre) & ProbPre < 0.5, 'Infected in the country of destination',
                default = 'Not considered migrant'
              )]
              stopifnot(caseData[, sum(is.na(MigrClass)) == 0])
            }

            # Select population data
            res <- GetPopulationData(caseData, aggrData, popCombination, aggrDataSelection)
            caseData <- res$Case
            aggrData <- res$Aggr

            # Prepare data sets for the HIV model
            if (dataAfterMigr && migrConnFlag) {
              # Prepare the Dead file based on the whole population dataset
              caseDataDead <- PrepareDataSetsForModel(
                caseData,
                splitBy = 'Imputation',
                dataSets = 'Dead'
              )

              # Prepare other datasets based on the subset of population
              caseDataRest <- PrepareDataSetsForModel(
                caseData[!(MigrClass %chin% c('Diagnosed prior to arrival', 'Infected in the country of origin'))], # nolint
                splitBy = 'Imputation',
                dataSets = c('HIV', 'AIDS', 'HIVAIDS', 'CD4')
              )
              caseDataAll <- modifyList(caseDataDead, caseDataRest)

              if ('Dead' %in% names(aggrData)) {
                aggrData <- aggrData['Dead']
              } else {
                aggrData <- NULL
              }
            } else {
              caseDataAll <- PrepareDataSetsForModel(caseData, splitBy = 'Imputation')
            }
            dataSets <- CombineData(caseDataAll, aggrData)

            if (is.null(dataSets)) {
              stop('No input data specified')
            }

            PrintH1('Performing main fit')

            impResults <- list()
            for (imp in names(dataSets)) {
              PrintH2('Iteration {.val {imp}}')
              context <- hivModelling::GetRunContext(
                data = dataSets[[imp]],
                settings = settings,
                parameters = list(
                  INCIDENCE = parameters
                )
              )
              popData <- hivModelling::GetPopulationData(context)

              startTime <- Sys.time()
              fitResults <- hivModelling::PerformMainFit(
                context,
                popData,
                attemptSimplify = TRUE,
                verbose = TRUE
              )
              runTime <- Sys.time() - startTime

              preMigrCounts <- GetPreMigrCounts(
                caseData[Imputation == as.integer(imp)],
                migrConnFlag,
                dataAfterMigr
              )
              PostProcessModelOutputs(
                fitResults$MainOutputs,
                preMigrCounts,
                migrConnFlag,
                dataAfterMigr
              )

              impResults[[imp]] <- list(
                Context = context,
                PopData = popData,
                Results = fitResults,
                RunTime = runTime,
                Imputation = imp
              )
            }

            # Combine models from multiple imputations as one average fit
            avgModelOutputs <- GetAverageHIVModelOutputs(impResults)
            preMigrCounts <- GetAveragePreMigrCounts(
              caseData,
              migrConnFlag,
              dataAfterMigr
            )
            PostProcessModelOutputs(
              avgModelOutputs,
              preMigrCounts,
              migrConnFlag,
              dataAfterMigr
            )

            plotData <- GetHIVPlotData(
              mainFitOutputs = avgModelOutputs,
              parameters = parameters
            )

            result <- list(
              MainFitResult = impResults,
              AvgModelOutputs = avgModelOutputs,
              PlotData = plotData
            )

            return(result)
          },
          args = list(
            caseData = private$AppMgr$CaseMgr$Data,
            aggrData = private$AppMgr$AggrMgr$Data,
            settings = settings,
            parameters = parameters,
            popCombination = popCombination,
            aggrDataSelection = private$Catalogs$AggrDataSelection,
            migrConnFlag = private$Catalogs$MigrConnFlag,
            randomSeed = .Random.seed
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$PopCombination <- popCombination
            private$Catalogs$MainFitResult <- result$MainFitResult
            private$Catalogs$AvgModelOutputs <- result$AvgModelOutputs
            private$Catalogs$PlotData <- result$PlotData
            private$InvalidateAfterStep('MODELLING')
            PrintAlert('Running HIV Model main fit task finished')
            private$SendMessage(
              'MODELS_RUN_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running HIV Model main fit task finished',
                PlotData = ConvertObjToJSON(result$PlotData, dataframe = 'columns')
              )
            )
          },
          failCallback = function(msg = NULL) {
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
            PrintAlert('Running HIV Model main fit task failed', type = 'danger')
            private$SendMessage(
              'MODELS_RUN_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running HIV Model main fit task failed'
              )
            )
          }
        )

        private$SendMessage(
          'MODELS_RUN_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model main fit task started'
          )
        )
      },
      error = function(e) {
        private$SendMessage(
          'MODELS_RUN_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Running HIV Model main fit task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelMainFit = function() {
      if (!is.null(private$Catalogs$MainFitTask)) {
        private$Catalogs$MainFitTask$Stop()
        private$SendMessage(
          'MODELS_RUN_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model main fit task cancelled'
          )
        )
      }

      return(invisible(self))
    },

    RunBootstrapFit = function(
      bsCount = 0,
      bsType = 'PARAMETRIC',
      maxRunTimeFactor = 3
    ) {
      if (!is.element(
        private$AppMgr$Steps['MODELLING'],
        private$AppMgr$CompletedSteps
      )) {
        PrintAlert(
          'Main fit must be performed before running bootstrap',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        avgRunTime <- mean(sapply(private$Catalogs$MainFitResult, '[[', 'RunTime'))
        maxRunTime <- as.difftime(avgRunTime * maxRunTimeFactor, units = 'secs')

        PrintAlert('Starting HIV Model bootstrap fit task')
        PrintAlert('Maximum allowed run time: {.timestamp {prettyunits::pretty_dt(maxRunTime)}}')

        private$Catalogs$BootstrapFitTask <- Task$new(
          function(
            bsCount,
            bsType,
            maxRunTime,
            mainFitResult,
            avgModelOutputs,
            caseData,
            aggrData,
            popCombination,
            aggrDataSelection,
            migrConnFlag,
            randomSeed
          ) {
            if (!require('hivPlatform', quietly = TRUE)) {
              suppressMessages(pkgload::load_all())
            }

            options(width = 120)
            .Random.seed <- randomSeed # nolint

            mainCount <- length(mainFitResult)

            if (bsType == 'NON-PARAMETRIC' & is.null(caseData)) {
              bsType <- 'PARAMETRIC'
              PrintAlert(
                'Bootstrap type "NON-PARAMETERIC" is selected, but there is no case-based data loaded.', # nolint
                'Bootstrap of type "PARAMETRIC" will be performed.',
                type = 'warning'
              )
            }

            fits <- list()
            i <- 0
            for (imp in names(mainFitResult)) {
              i <- i + 1
              mainFit <- mainFitResult[[imp]]
              context <- mainFit$Context
              param <- mainFit$Results$Param
              info <- mainFit$Results$Info

              context$Settings <- modifyList(
                context$Settings,
                list(
                  ModelFilePath = NULL,
                  InputDataPath = NULL,
                  Verbose = FALSE
                ),
                keep.null = TRUE
              )

              PrintH2('Main data set {.val {imp}}')

              dataAfterMigr <- FALSE
              if (bsType == 'NON-PARAMETRIC') {
                caseDataImp <- caseData[Imputation == as.integer(imp)]
                dataAfterMigr <- 'ProbPre' %in% colnames(caseDataImp)
              } else {
                bootData <- context$Data
              }

              jSucc <- 0
              j <- 0
              bootResults <- list()
              while (jSucc < bsCount) {
                j <- j + 1

                # Bootstrap data set

                if (bsType == 'NON-PARAMETRIC') {
                  bootCaseDataImp <- caseDataImp[sample.int(nrow(caseDataImp), replace = TRUE)]
                  if (dataAfterMigr && migrConnFlag) {
                    bootCaseDataImp[, MigrClass := data.table::fcase(
                      !is.na(DateOfArrival) & DateOfHIVDiagnosis < DateOfArrival, 'Diagnosed prior to arrival', # nolint
                      !is.na(ProbPre) & ProbPre >= 0.5, 'Infected in the country of origin',
                      !is.na(ProbPre) & ProbPre < 0.5, 'Infected in the country of destination',
                      default = 'Not considered migrant'
                    )]
                  }

                  res <- GetPopulationData(bootCaseDataImp, aggrData, popCombination, aggrDataSelection) # nolint
                  caseData <- res$Case
                  aggrData <- res$Aggr

                  if (dataAfterMigr && migrConnFlag) {
                    # Prepare the Dead file based on the whole population dataset
                    caseDataDead <- PrepareDataSetsForModel(
                      caseData,
                      splitBy = 'Imputation',
                      dataSets = 'Dead'
                    )

                    # Prepare other datasets based on the subset of population
                    caseDataRest <- PrepareDataSetsForModel(
                      caseData[!(MigrClass %chin% c('Diagnosed prior to arrival', 'Infected in the country of origin'))], # nolint
                      splitBy = 'Imputation',
                      dataSets = c('HIV', 'AIDS', 'HIVAIDS', 'CD4')
                    )
                    caseDataAll <- modifyList(caseDataDead, caseDataRest)

                    if ('Dead' %in% names(res$Aggr)) {
                      aggrData <- res$Aggr['Dead']
                    } else {
                      aggrData <- NULL
                    }
                  } else {
                    caseDataAll <- PrepareDataSetsForModel(caseData, splitBy = 'Imputation')
                  }
                  bootData <- CombineData(caseDataAll, aggrData)[[1]]
                }

                bootContext <- hivModelling::GetRunContext(
                  data = bootData,
                  parameters = context$Parameters,
                  settings = context$Settings
                )

                bootPopData <- hivModelling::GetPopulationData(bootContext)

                startTime <- Sys.time()
                switch(
                  bsType,
                  'PARAMETRIC' = {
                    bootResult <- hivModelling::PerformBootstrapFit(
                      j, bootContext, bootPopData, mainFit$Results
                    )
                  },
                  'NON-PARAMETRIC' = {
                    capture.output({
                      bootResult <- hivModelling::PerformMainFit(
                        bootContext, bootPopData,
                        param = param, info = info, attemptSimplify = FALSE,
                        maxRunTime = maxRunTime, verbose = FALSE
                      )
                    })
                  }
                )
                runTime <- Sys.time() - startTime

                preMigrCounts <- GetPreMigrCounts(
                  bootCaseDataImp,
                  migrConnFlag,
                  dataAfterMigr
                )
                PostProcessModelOutputs(
                  bootResult$MainOutputs,
                  preMigrCounts,
                  migrConnFlag,
                  dataAfterMigr
                )

                if (bootResult$Converged) {
                  msgType <- 'success'
                  jSucc <- jSucc + 1
                  progress <- (jSucc + (i - 1) * bsCount) / (mainCount * bsCount) * 100
                } else {
                  msgType <- 'danger'
                }
                jSuccRate <- jSucc / j

                PrintAlert(
                  'Iteration {.val {jSucc}} done |',
                  'Run time: {.timestamp {prettyunits::pretty_dt(runTime)}} |',
                  'Success rate: {.val {jSuccRate * 100}}%',
                  type = msgType
                )

                bootResults[[j]] <- list(
                  Context = bootContext,
                  Data = bootData,
                  Results = bootResult,
                  RunTime = runTime,
                  DataSet = imp,
                  BootIteration = jSucc
                )
              }

              fits[[imp]] <- bootResults
            }

            stats <- GetBootstrapFitStats(fits)

            plotData <- GetHIVPlotData(
              mainFitOutputs = avgModelOutputs,
              bootstrapFitStats = stats,
              parameters = info
            )

            result <- list(
              Fits = fits,
              Stats = stats,
              PlotData = plotData
            )

            return(result)
          },
          args = list(
            bsCount = bsCount,
            bsType = bsType,
            maxRunTime = maxRunTime,
            mainFitResult = isolate(private$Catalogs$MainFitResult),
            avgModelOutputs = isolate(private$Catalogs$AvgModelOutputs),
            caseData = isolate(private$AppMgr$CaseMgr$Data),
            aggrData = isolate(private$AppMgr$AggrMgr$Data),
            popCombination = isolate(private$Catalogs$PopCombination),
            aggrDataSelection = isolate(private$Catalogs$AggrDataSelection),
            migrConnFlag = private$Catalogs$MigrConnFlag,
            randomSeed = .Random.seed
          ),
          session = private$Session,
          successCallback = function(result) {
            PrintAlert('Running HIV Model bootstrap fit task finished')

            private$Catalogs$BootstrapFitResult <- result$Fits
            private$Catalogs$BootstrapFitStats <- result$Stats
            private$Catalogs$PlotData <- result$PlotData
            private$InvalidateAfterStep('BOOTSTRAP')

            private$SendMessage(
              'BOOTSTRAP_RUN_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running HIV Model bootstrap fit task finished',
                PlotData = ConvertObjToJSON(result$PlotData, dataframe = 'columns')
              )
            )
          },
          failCallback = function(msg = NULL) {
            PrintAlert('Running HIV Model bootstrap fit task failed', type = 'danger')
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
            private$SendMessage(
              'BOOTSTRAP_RUN_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running HIV Model bootstrap fit task failed'
              )
            )
          }
        )

        private$SendMessage(
          'BOOTSTRAP_RUN_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model bootstrap fit task started'
          )
        )
      },
      error = function(e) {
        private$SendMessage(
          'BOOTSTRAP_RUN_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Running HIV Model bootstrap fit task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelBootstrapFit = function() {
      if (!is.null(private$Catalogs$BootstrapFitTask)) {
        private$Catalogs$BootstrapFitTask$Stop()
        private$SendMessage(
          'BOOTSTRAP_RUN_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model bootstrap fit task cancelled'
          )
        )
      }
      return(invisible(self))
    },

    GetState = function() {
      state <- list(
        Catalogs = list(
          MigrConnFlag = private$Catalogs$MigrConnFlag,
          PopCombination = private$Catalogs$PopCombination,
          AggrDataSelection = private$Catalogs$AggrDataSelection,
          MainFitResult = private$Catalogs$MainFitResult,
          AvgModelOutputs = private$Catalogs$AvgModelOutputs,
          Years = private$Catalogs$Years,
          BootstrapFitResult = private$Catalogs$BootstrapFitResult,
          BootstrapFitStats = private$Catalogs$BootstrapFitStats,
          PlotData = private$Catalogs$PlotData
        )
      )

      return(state)
    },

    SetState = function(state) {
      private$Catalogs$MigrConnFlag <- state$Catalogs$MigrConnFlag
      private$Catalogs$PopCombination <- state$Catalogs$PopCombination
      private$Catalogs$AggrDataSelection <- state$Catalogs$AggrDataSelection
      private$Catalogs$MainFitResult <- state$Catalogs$MainFitResult
      private$Catalogs$AvgModelOutputs <- state$Catalogs$AvgModelOutputs
      private$Catalogs$Years <- state$Catalogs$Years
      private$Catalogs$BootstrapFitResult <- state$Catalogs$BootstrapFitResult
      private$Catalogs$BootstrapFitStats <- state$Catalogs$BootstrapFitStats
      private$Catalogs$PlotData <- state$Catalogs$PlotData

      return(invisible(self))
    }
  ),

  private = list(
    # Shiny session
    Session = NULL,

    # Private application manager
    AppMgr = NULL,

    # Storage
    Catalogs = NULL,

    SendMessage = function(...) {
      if (is.function(private$AppMgr$SendMessage)) {
        private$AppMgr$SendMessage(...)
      }
    },

    InvalidateAfterStep = function(step) {
      if (
        step %in% c('MODELLING')
      ) {
        private$Catalogs$BootstrapFitTask <- NULL
        private$Catalogs$BootstrapFitResult <- NULL
        private$Catalogs$BootstrapFitStats <- NULL
      }

      private$AppMgr$SetCompletedStep(step)

      return(invisible(self))
    }
  ),

  active = list(
    MigrConnFlag = function() {
      return(private$Catalogs$MigrConnFlag)
    },

    PopCombination = function() {
      return(private$Catalogs$PopCombination)
    },

    AggrDataSelection = function() {
      return(private$Catalogs$AggrDataSelection)
    },

    Years = function() {
      return(private$Catalogs$Years)
    },

    MainFitTask = function() {
      return(private$Catalogs$MainFitTask)
    },

    MainFitResult = function() {
      return(private$Catalogs$MainFitResult)
    },

    AvgModelOutputs= function() {
      return(private$Catalogs$AvgModelOutputs)
    },

    BootstrapFitTask = function() {
      return(private$Catalogs$BootstrapFitTask)
    },

    BootstrapFitResult = function() {
      return(private$Catalogs$BootstrapFitResult)
    },

    BootstrapFitStats = function() {
      return(private$Catalogs$BootstrapFitStats)
    },

    PlotData = function() {
      return(private$Catalogs$PlotData)
    }
  )
)
