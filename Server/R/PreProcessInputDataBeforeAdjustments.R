#' PreProcessInputDataBeforeAdjustments
#'
#' Pre-processes input data before passing it to adjustment scripts.
#' All changes are applied to the input object by reference.
#' There is no need to assign the result of this function.
#'
#' @param inputData Input data. Required.
#'
#' @return data.table object (invisibly)
#'
#' @examples
#' \dontrun{
#' PreProcessInputDataBeforeAdjustments(inputData)
#' }
#'
#' @export
PreProcessInputDataBeforeAdjustments <- function(
  inputData
) {
  stopifnot(!missing(inputData))

  if (is.null(inputData)) {
    return(NULL)
  }

  # Support imputing reporting delay -----------------------------------------------

  # Create Min and Max notification time
  inputData[, ':='(
    MinNotificationTime = min(NotificationTime, na.rm = TRUE),
    MaxNotificationTime = max(NotificationTime, na.rm = TRUE)
  ), by = .(ReportingCountry)]

  inputData[is.infinite(MinNotificationTime), MinNotificationTime := NA_real_]
  inputData[is.infinite(MaxNotificationTime), MaxNotificationTime := NA_real_]

  # Create VarX, MaxPossibleDelay
  inputData[, c('VarX', 'TweakedVarX', 'MaxPossibleDelay', 'TweakedMaxPossibleDelay') := {
    # Compute VarX
    varX <- as.integer(4L * (NotificationTime - DiagnosisTime))
    varX[varX < 0L] <- NA_integer_

    # Compute MaxPossibleDelay
    maxPossibleDelay <- ifelse(
      is.na(DiagnosisTime),
      as.integer(4L * (MaxNotificationTime - (YearOfHIVDiagnosis + 0.125))),
      4L * (MaxNotificationTime - DiagnosisTime)
    )
    maxPossibleDelay <- ifelse(
      is.na(DateOfHIVDiagnosis),
      as.integer(4L * (NotificationTime - MinNotificationTime)),
      maxPossibleDelay
    )
    maxPossibleDelay <- ifelse(
      is.na(maxPossibleDelay),
      as.integer(4L * (MaxNotificationTime - MinNotificationTime)),
      maxPossibleDelay
    )

    tweakedVarX <- ifelse(varX == 0L, 0.01, varX)
    tweakedVarX <- ifelse(tweakedVarX == maxPossibleDelay, maxPossibleDelay - 0.01, tweakedVarX)

    # Tweak MaxPossibleDelay
    tweakedMaxPossibleDelay <- ifelse(maxPossibleDelay == 0, 0.02, maxPossibleDelay)

    list(varX, tweakedVarX, maxPossibleDelay, tweakedMaxPossibleDelay)
  }]

  return(invisible(inputData))
}
