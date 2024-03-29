#' AppManager
#'
#' R6 class for representing the app manaager
#'
#' @name AppManager
#' @examples
#' pppManager <- AppManager$new()
NULL

#' @export
AppManager <- R6::R6Class(
  classname = 'AppManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL
    ) {
      packageName <- methods::getPackageName()
      packageDescr <- utils::packageDescription(
        pkg = packageName,
        fields = c('Package', 'Title', 'Version', 'Date', 'Description')
      )
      private$PackageDetails_ <- list(
        Name = packageName,
        Title = packageDescr$Title,
        Version = packageDescr$Version,
        Date = packageDescr$Date,
        Description = packageDescr$Description
      )
      private$Session <- session
      private$CaseMgrPriv <- CaseDataManager$new(session, self)
      private$AggrMgrPriv <- AggrDataManager$new(session, self)
      private$HIVModelMgrPriv <- HIVModelManager$new(session, self)
      private$UIStatePriv <- NULL
      private$Observers <- NULL

      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        Seed = NULL,
        CompletedSteps = NULL,
        ReportTask = NULL,
        ReportArtifacts = NULL,
        Report = NULL
      )

      self$SendMessage(
        'PACKAGE_DETAILS_SENT',
        list(
          ActionStatus = 'SUCCESS',
          PackageDetails = self$PackageDetails
        )
      )

      self$SetCompletedStep('SESSION_INITIALIZED')
    },

    print = function() {
      print(self$Session)
    },

    SendMessage = function(type, payload = list()) {
      if (missing(type)) {
        PrintAlert('Arguments {.arg type} must be provided', type = 'danger')
        return(invisible(NULL))
      }

      if (!is.null(private$Session)) {
        private$Session$sendCustomMessage('shinyHandler', list(
          type = type,
          payload = payload
        ))
      }
    },

    Steps = c(
      'SESSION_INITIALIZED' = 1L,
      'CASE_BASED_READ' = 2L,
      'CASE_BASED_ATTR_MAPPING' = 3L,
      'CASE_BASED_ORIGIN_GROUPING' = 4L,
      'CASE_BASED_SUMMARY' = 5L,
      'CASE_BASED_ADJUSTMENTS' = 6L,
      'CASE_BASED_MIGRATION' = 7L,
      'REPORTS' = 8L,
      'AGGR_READ' = 9L,
      'MODELLING' = 10L,
      'BOOTSTRAP' = 11L,
      'OUTPUTS' = 12L
    ),

    SetCompletedStep = function(step) {
      completedSteps <- isolate(private$Catalogs$CompletedSteps)
      step <- self$Steps[step]
      keptSteps <- completedSteps[completedSteps < step]
      newCompletedSteps <- sort(union(keptSteps, step))
      private$Catalogs$CompletedSteps <- self$Steps[newCompletedSteps]

      if (!identical(completedSteps, newCompletedSteps)) {
        self$SendMessage(
          'COMPLETED_STEPS_SET',
          payload = list(
            ActionStatus = 'SUCCESS',
            CompletedSteps = isolate(names(private$Catalogs$CompletedSteps))
          )
        )
      }
    },

    SetSeed = function(seed) {
      private$Catalogs$Seed <- seed
      set.seed(seed)
      if (is.null(seed)) {
        msg <- 'Random seed set to time-based (random) for all subsequent computations'
      } else {
        msg <- sprintf('Random seed set to %s for all subsequent computations', seed)
      }
      self$SendMessage(
        'SEED_SET',
        payload = list(
          ActionStatus = 'SUCCESS',
          ActionMessage = msg,
          Seed = seed
        )
      )
    },

    SetUIState = function(uiState) {
      private$UIStatePriv <- uiState
      return(invisible(self))
    },

    SaveState = function() {
      state <- private$GetState()
      fileName <- sprintf('HIVPlatformState_%s.rds', GetTimeStamp())
      if (is.null(private$Session)) {
        saveRDS(state, fileName)
      } else {
        rc <- rawConnection(raw(0), 'r+')
        saveRDS(state, rc)
        rcData <- rawConnectionValue(rc)
        close(rc)
        self$SendMessage(
          'SAVE_STATE',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Application state available for saving',
            Data = rcData,
            FileName = fileName
          )
        )
      }
      PrintAlert('State file {.file {fileName}} saved')
      return(invisible(self))
    },

    LoadState = function(
      filePath,
      fileName = NULL
    ) {
      if (!is.element(self$Steps['SESSION_INITIALIZED'], self$CompletedSteps)) {
        PrintAlert(
          'AppManager is not initialized properly before loading the state',
          type = 'danger'
        )
        return(invisible(self))
      }

      if (is.null(fileName)) {
        fileName <- basename(filePath)
      }
      status <- 'SUCCESS'
      msg <- 'State file read correctly'
      tryCatch(
        {
          state <- readRDS(filePath)
          private$SetState(state)
        },
        error = function(e) {
          status <<- 'FAIL'
          msg <<- e$message
        }
      )

      if (status == 'SUCCESS') {
        PrintAlert('State file {.file {fileName}} loaded')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          UIState = self$UIState
        )
      } else {
        PrintAlert('Loading state file {.file {fileName}} failed', type = 'danger')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      self$SendMessage('UI_STATE_READY_FOR_LOAD', payload)

      return(invisible(self))
    },

    # USER ACTIONS =================================================================================

    CreateReport = function(
      reportSpec
    ) {
      if (!is.element(
        self$Steps['CASE_BASED_ADJUSTMENTS'],
        private$Catalogs$CompletedSteps
      )) {
        PrintAlert(
          'Adjustments must be run before creating a report',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting report task')

        private$Catalogs$ReportTask <- Task$new(
          function(reportSpec, fileName, filters, adjustedData, randomSeed) {
            if (!requireNamespace('hivPlatform', quietly = TRUE)) {
              suppressMessages(pkgload::load_all())
            }

            .Random.seed <- randomSeed #nolint

            reportFilePath <- hivPlatform::GetReportFileNames()[reportSpec$name]
            params <- modifyList(
              reportSpec,
              list(
                AdjustedData = adjustedData
              )
            )
            params <- hivPlatform::GetMainReportArtifacts(params)
            params <- modifyList(
              params,
              list(
                Artifacts =
                  list(
                    FileName = fileName,
                    Filters = filters
                  )
              )
            )
            report <- hivPlatform::RenderReportToHTML(reportFilePath, params)

            result <- list(
              Artifacts = params,
              Report = report
            )

            return(result)
          },
          args = list(
            reportSpec = reportSpec,
            fileName = private$CaseMgrPriv$FileName,
            filters = private$CaseMgrPriv$Filters,
            adjustedData = private$CaseMgrPriv$AdjustmentResult,
            randomSeed = .Random.seed
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$Report <- result$Report
            private$Catalogs$ReportArtifacts <- result$Artifacts
            PrintAlert('Running report task finished')
            self$SendMessage(
              'CREATING_REPORT_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running report task finished',
                Report = result$Report
              )
            )
            self$SetCompletedStep('REPORTS')
          },
          failCallback = function(msg = NULL) {
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
            PrintAlert('Running report task failed', type = 'danger')
            self$SendMessage(
              'CREATING_REPORT_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running report task failed'
              )
            )
          }
        )
        self$SendMessage(
          'CREATING_REPORT_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running report task started'
          )
        )
      },
      error = function(e) {
        self$SendMessage(
          'CREATING_REPORT_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Starting report task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelReport = function() {
      if (!is.null(private$Catalogs$ReportTask)) {
        private$Catalogs$ReportTask$Stop()

        self$SendMessage(
          'CREATING_REPORT_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running report task cancelled'
          )
        )
      }

      return(invisible(self))
    },

    SetObservers = function(observers) {
      private$Observers <- observers
      return(invisible(self))
    },
    SuspendObservers = function() {
      sapply(private$Observers, function(o) o$suspend())
      private$ObserversSuspended_ <- TRUE
      PrintAlert('Observers suspended:', self$ObserversSuspended)
      return(invisible(self))
    },

    ResumeObservers = function() {
      sapply(private$Observers, function(o) o$resume())
      private$ObserversSuspended_ <- FALSE
      PrintAlert('Observers suspended:', self$ObserversSuspended)
      return(invisible(self))
    }
  ),

  private = list(
    # Package details
    PackageDetails_ = list(),

    # Shiny session
    Session = NULL,

    # UI state (JSON)
    UIStatePriv = NULL,

    # Case-based data manager
    CaseMgrPriv = NULL,

    # Aggregated data manager
    AggrMgrPriv = NULL,

    # HIV Model manager
    HIVModelMgrPriv = NULL,

    # Observers
    Observers = NULL,

    ObserversSuspended_ = FALSE,

    # Storage
    Catalogs = NULL,

    GetState = function() {
      state <- list(
        UIState = self$UIState,
        Catalogs = list(
          Seed = private$Catalogs$Seed,
          CompletedSteps = private$Catalogs$CompletedSteps,
          ReportArtifacts = private$Catalogs$ReportArtifacts,
          Report = private$Catalogs$Report
        ),
        CaseMgr = self$CaseMgr$GetState(),
        AggrMgr = self$AggrMgr$GetState(),
        HIVModelMgr = self$HIVModelMgr$GetState()
      )

      return(state)
    },

    SetState = function(state) {
      private$UIStatePriv <- state$UIState
      private$Catalogs$Seed <- state$Catalogs$Seed
      private$Catalogs$CompletedSteps <- state$Catalogs$CompletedSteps
      private$Catalogs$ReportArtifacts <- state$Catalogs$ReportArtifacts
      private$Catalogs$Report <- state$Catalogs$Report
      self$CaseMgr$SetState(state$CaseMgr)
      self$AggrMgr$SetState(state$AggrMgr)
      self$HIVModelMgr$SetState(state$HIVModelMgr)
      return(invisible(self))
    }
  ),

  active = list(
    PackageDetails = function() {
      return(private$PackageDetails_)
    },

    UIState = function() {
      return(private$UIStatePriv)
    },

    CaseMgr = function() {
      return(private$CaseMgrPriv)
    },

    AggrMgr = function() {
      return(private$AggrMgrPriv)
    },

    HIVModelMgr = function() {
      return(private$HIVModelMgrPriv)
    },

    CompletedSteps = function() {
      return(private$Catalogs$CompletedSteps)
    },

    Seed = function() {
      return(private$Catalogs$Seed)
    },

    ReportTask = function() {
      return(private$Catalogs$ReportTask)
    },

    ReportArtifacts = function() {
      return(private$Catalogs$ReportArtifacts)
    },

    Report = function() {
      return(private$Catalogs$Report)
    },

    ObserversSuspended = function() {
      return(private$ObserversSuspended_)
    }
  )
)
