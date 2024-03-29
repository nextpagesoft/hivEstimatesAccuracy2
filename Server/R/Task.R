#' Task
#'
#' R6 class for representing a background task
#'
#' @name Task
#' @examples
#' \dontrun{
#' task <- Task$new(function() { 1 + 1})
#' task$Result
#' }
NULL

#' @export
Task <- R6::R6Class(
  classname = 'Task',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      expr,
      args = list(),
      successCallback = NULL,
      failCallback = NULL,
      progressRefresh = 2,
      session = NULL,
      autorun = TRUE
    ) {
      private$Expr <- expr
      private$Args <- args
      private$SuccessCallback <- successCallback
      private$FailCallback <- failCallback
      private$ProgressRefresh <- progressRefresh
      private$Session <- session
      private$Autorun <- autorun

      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)

      private$Catalogs <- catalogStorage(
        Status = 'IDLE',
        TaskHandle = NULL,
        Result = NULL,
        StartTime = NULL,
        RunLog = '',
        HTMLRunLog = '',
        FailMessage = ''
      )

      if (autorun) {
        self$Run()
      }
    },

    finalize = function() {
      self$Stop()
    },

    Run = function() {
      if (self$IsRunning) {
        PrintAlert('Task is already running', type = 'warning')
        return(invisible(self))
      }
      private$InitializeCatalogs()

      taskHandle <- callr::r_bg(
        force(private$Expr),
        args = private$Args,
        supervise = TRUE,
        package = FALSE,
        stdout = '|',
        stderr = '|',
        user_profile = 'project',
        error = 'error'
      )

      private$Catalogs$Status <- 'CREATED'

      private$Catalogs$TaskHandle <- taskHandle
      private$Process <- taskHandle$as_ps_handle()
      private$Catalogs$StartTime <- taskHandle$get_start_time()

      private$Monitor()
      return(invisible(self))
    },

    Stop = function(force = FALSE) {
      if (force || self$IsRunning) {
        isolate(private$Catalogs$TaskHandle)$kill()
      }
    }
  ),

  private = list(
    # Expression to run
    Expr = NULL,

    # Arguments for the expression
    Args = NULL,

    # On-success callback function
    SuccessCallback = NULL,

    # On-fail callback function
    FailCallback = NULL,

    # Progress refresh timeout in seconds
    ProgressRefresh = 2,

    # Shiny session
    Session = NULL,

    # Reference to the underlying process
    Process = NULL,

    # Start job immediately
    Autorun = TRUE,

    # Storage
    Catalogs = NULL,

    CancelProcessed = FALSE,

    InitializeCatalogs = function(skipRunLog = FALSE) {
      private$Catalogs$Status <- 'IDLE'
      private$Catalogs$TaskHandle <- NULL
      private$Catalogs$Result <- NULL
      private$Catalogs$StartTime <- NULL
      private$Catalogs$FailMessage <- ''
      if (!skipRunLog) {
        private$Catalogs$HTMLRunLog <- ''
        private$Catalogs$RunLog <- ''
        private$CancelProcessed <- FALSE
      }
    },

    CollectRunLog = function() {
      log <- ''
      if (self$IsRunning) {
        log <- private$Catalogs$TaskHandle$read_output()
      } else if (self$IsFinished) {
        if (!self$IsCancelled) {
          log <- private$Catalogs$TaskHandle$read_all_output()
        } else if (!private$CancelProcessed) {
          log <- '\nTask cancelled'
          private$Catalogs$Status <- 'CANCELLED'
          private$CancelProcessed <- TRUE
        }
      }
      return(log)
    },

    AddToRunLog = function(log) {
      private$Catalogs$RunLog <- paste0(
        private$Catalogs$RunLog,
        CollapseTexts(log, collapse = '\n')
      )
    },

    IsReactive = function() {
      return(!is.null(private$Session))
    },

    Monitor = function(timeout) {
      if (private$IsReactive()) {
        o <- shiny::observe({
          private$Catalogs$Status
          if (self$IsRunning) {
            private$Catalogs$Status <- 'RUNNING'
            self$RunLog
            shiny::invalidateLater(private$ProgressRefresh * 1000)
          } else {
            private$Catalogs$Status <- 'SUCCESS'
            self$Result
            self$RunLog
            if (
              private$Catalogs$Status == 'SUCCESS' && is.function(private$SuccessCallback)
            ) {
              private$SuccessCallback(isolate(self$Result))
            } else if (
              private$Catalogs$Status == 'FAIL' && is.function(private$FailCallback)
            ) {
              failMsg <- private$Catalogs$FailMessage
              errorMsg <- CollapseTexts('Background task failed:', failMsg, collapse = '\n')
              private$AddToRunLog(errorMsg)
              private$FailCallback(failMsg)
            }
            o$destroy()
          }
        })
      } else {
        while (self$IsRunning) {
          private$Catalogs$Status <- 'RUNNING'
          log <- private$CollectRunLog()
          cat(log)
          private$AddToRunLog(log)
          Sys.sleep(private$ProgressRefresh)
        }
        private$Catalogs$Status <- 'SUCCESS'
        log <- private$CollectRunLog()
        cat(log)
        private$AddToRunLog(log)
      }

      result <- isolate(self$Result)
      status <- isolate(self$Status)
      if (
        status == 'SUCCESS' && is.function(private$SuccessCallback)
      ) {
        private$SuccessCallback(result)
      } else if (
        status == 'FAIL' && is.function(private$FailCallback)
      ) {
        failMsg <- private$Catalogs$FailMessage
        errorMsg <- CollapseTexts("Background task failed:", failMsg, collapse = "\n")
        private$AddToRunLog(errorMsg)
        private$FailCallback(failMsg)
      }
      return(invisible(self))
    }
  ),

  active = list(
    Result = function() {
      if (self$IsFinished && !self$IsCancelled) {
        result <- NULL
        tryCatch({
          result <- private$Catalogs$TaskHandle$get_result()
        },
        error = function(e) {
          private$Catalogs$Status <- 'FAIL'
          private$Catalogs$FailMessage <- gsub('callr subprocess failed: ', '', e$parent)
        })
        private$Catalogs$Result <- result
      }
      return(isolate(private$Catalogs$Result))
    },

    RunLog = function() {
      private$AddToRunLog(private$CollectRunLog())

      return(private$Catalogs$RunLog)
    },

    HTMLRunLog = function() {
      return(fansi::sgr_to_html(private$Catalogs$RunLog, warn = FALSE))
    },

    FailMessage = function() {
      return(private$Catalogs$FailMessage)
    },

    TaskHandle = function() {
      return(private$Catalogs$TaskHandle)
    },

    StartTime = function() {
      return(private$Catalogs$StartTime)
    },

    IsInitialized = function() {
      return(!is.null(private$Process))
    },

    IsRunning = function() {
      return(
        self$IsInitialized &&
          ps::ps_is_running(private$Process)
      )
    },

    IsFinished = function() {
      return(
        self$IsInitialized &&
          !self$IsRunning &&
          !is.null(private$Catalogs$TaskHandle$get_exit_status())
      )
    },

    IsCancelled = function() {
      return(
        self$IsFinished &&
          (
            is.na(private$Catalogs$TaskHandle$get_exit_status()) ||
            private$Catalogs$TaskHandle$get_exit_status() %in% c(2, -9)
          )
      )
    },

    Status = function() {
      return(private$Catalogs$Status)
    }
  )
)
