#' AggrDataManager
#'
#' R6 class for representing the aggregated data manaager
#'
#' @name AggrDataManager
#' @examples
#' caseMgr <- AggrDataManager$new()
NULL

#' @export
AggrDataManager <- R6::R6Class(
  classname = 'AggrDataManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL,
      appMgr = NULL
    ) {
      private$Session <- session
      private$AppMgr <- appMgr
      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        FileName = NULL,
        Data = NULL,
        PopulationNames = NULL
      )
    },

    print = function() {
      print('AggrDataManager')
    },

    # USER ACTIONS =================================================================================

    # 1. Read case-based data ----------------------------------------------------------------------
    ReadData = function(
      filePath,
      fileName = basename(filePath)
    ) {
      if (!is.null(private$AppMgr) && !is.element(
        private$AppMgr$Steps['SESSION_INITIALIZED'],
        private$AppMgr$CompletedSteps
      )) {
        PrintAlert(
          'AppManager is not initialized properly before reading data',
          type = 'danger'
        )
        return(invisible(self))
      }

      status <- 'SUCCESS'
      msg <- 'Data read correctly'
      tryCatch({
        data <- hivModelling::ReadInputData(filePath)
        fileInfo <- file.info(filePath)
        if (!fileInfo$isdir && tolower(tools::file_ext(filePath)) == 'csv') {
          names(data) <- tools::file_path_sans_ext(fileName)
        }
        if (is.null(data)) {
          status <- 'FAIL'
          msg <- 'No data to load found'
        } else {
          dataYears <- lapply(data, '[[', 'Year')
          dataNames <- names(data)
          dataTypesGroupings <- c('^Dead$', '^AIDS$', '^(HIV|HIVAIDS)$', '^HIV_CD4_[1-4]{1}$')
          dataFiles <- lapply(dataTypesGroupings, function(grouping) {
            names <- grep(grouping, dataNames, value = TRUE)
            years <- Filter(function(x) length(x) > 0, dataYears[names])
            if (length(years) > 0) {
              minYear <- min(sapply(years, min, na.rm = TRUE))
              maxYear <- max(sapply(years, max, na.rm = TRUE))
              return(list(
                name = paste(names, collapse = ', '),
                use = TRUE,
                years = c(minYear, maxYear)
              ))
            } else {
              return(NULL)
            }
          })
          dataFiles <- Filter(Negate(is.null), dataFiles)
          dataRangeYears <- sapply(dataFiles, '[[', 'years')
          populationNames <- NULL
          if (is.matrix(dataRangeYears)) {
            rangeYears <- c(min(dataRangeYears), max(dataRangeYears))
            populationNames <- names(data[[1]])[-1]
          }
        }
      },
      error = function(e) {
        status <<- 'FAIL'
        msg <<- e$message
      })

      if (status == 'SUCCESS') {
        private$Catalogs$FileName <- filePath
        private$Catalogs$Data <- data
        private$Catalogs$PopulationNames <- populationNames
        if (!is.null(private$AppMgr)) {
          private$AppMgr$SetCompletedStep('AGGR_READ')
        }
        PrintAlert('Data file {.file {filePath}} loaded')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          DataFiles = dataFiles,
          PopulationNames = names(data[[1]])[-1],
          RangeYears = rangeYears
        )
        if (is.function(private$AppMgr$HIVModelMgr$DetermineYearRanges)) {
          private$AppMgr$HIVModelMgr$DetermineYearRanges()
        }
      } else {
        PrintAlert('Loading data file {.file {filePath}} failed', type = 'danger')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('AGGR_DATA_READ', payload)

      return(invisible(self))
    },

    GetState = function() {
      state <- list(
        Catalogs = list(
          FileName = private$Catalogs$FileName,
          Data = private$Catalogs$Data,
          PopulationNames = private$Catalogs$PopulationNames
        )
      )

      return(state)
    },

    SetState = function(state) {
      private$Catalogs$FileName <- state$Catalogs$FileName
      private$Catalogs$Data <- state$Catalogs$Data
      private$Catalogs$PopulationNames <- state$Catalogs$PopulationNames

      return(invisible(self))
    }
  ),

  private = list(
    # Shiny session
    Session = NULL,

    # Parent application manager
    AppMgr = NULL,

    # Storage
    Catalogs = NULL,

    SendMessage = function(...) {
      if (is.function(private$AppMgr$SendMessage)) {
        private$AppMgr$SendMessage(...)
      }
    }
  ),

  active = list(
    FileName = function() {
      return(private$Catalogs$FileName)
    },

    Data = function() {
      return(private$Catalogs$Data)
    },

    PopulationNames = function() {
      return(private$Catalogs$PopulationNames)
    }
  )
)
