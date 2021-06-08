library(hivPlatform)

Sys.setenv(RSTUDIO_PANDOC = 'c:/SoftDevel/pandoc')

appMgr <- AppManager$new()
# STEP 1 - Load data -------------------------------------------------------------------------------

# nolint start
# appMgr$CaseMgr$ReadData(GetSystemFile('testData', 'dummy_miss1.zip'))
# appMgr$CaseMgr$ReadData('D:/_DEPLOYMENT/hivEstimatesAccuracy/PL2019.xlsx')
# appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/dummy2019_exclUK.csv')
# appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/dummy2019_exclUK.xlsx')
# appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/dummy2019_exclUK.csv')
# appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/dummy2019_exclUK.csv')
# appMgr$CaseMgr$ReadData(filePath = 'D:/VirtualBox_Shared/PLtest.csv')
# appMgr$AggrMgr$ReadData(GetSystemFile('testData', 'test_-_2_populations.zip'))
# appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/HIV test files/Data/HEAT_202102_1_no_prevpos_random_id.csv')
# appMgr$AggrMgr$ReadData('D:/VirtualBox_Shared/HIV test files/Data/Test NL.zip')
# appMgr$AggrMgr$ReadData('D:/VirtualBox_Shared/HIV test files/Data/Test NL - Copy.zip')
# appMgr$AggrMgr$ReadData(fileName = 'D:/VirtualBox_Shared/DATA_PL.ZIP')
appMgr$CaseMgr$ReadData('D:/VirtualBox_Shared/BE.csv')
# nolint end


# STEP 2 - Pre-process case-based data -------------------------------------------------------------
appMgr$CaseMgr$ApplyAttributesMapping()
appMgr$CaseMgr$ApplyOriginGrouping(originGrouping = list())

appMgr$CaseMgr$SetFilters(filters = list(
  DiagYear = list(
    ApplyInAdjustments = TRUE,
    MinYear =  1980,
    MaxYear =  2019
  ),
  NotifQuarter <- list(
    ApplyInAdjustments = FALSE,
    MinYear = 1995.375,
    MaxYear = 2020.375
  )
))

# STEP 3 - Adjust case-based data ------------------------------------------------------------------
adjustmentSpecs <- GetAdjustmentSpecs(c('Multiple Imputation using Chained Equations - MICE'))
# adjustmentSpecs <- GetAdjustmentSpecs(c('Reporting Delays with trend'))
adjustmentSpecs$`Reporting Delays`$Parameters$startYear$value <- 2015
adjustmentSpecs$`Reporting Delays`$Parameters$endYear$value <- 2020
adjustmentSpecs$`Reporting Delays`$Parameters$endQrt$value <- 2
appMgr$CaseMgr$RunAdjustments(adjustmentSpecs)

# saveRDS(appMgr$CaseMgr$Data, 'D:/VirtualBox_Shared/BE_adjusted.rds')

# STEP 4 - Create adjusted case-based data report --------------------------------------------------
appMgr$CreateReport(
  reportSpec = list(
    name = 'Main Report',
    reportingDelay = TRUE,
    smoothing = FALSE,
    cd4ConfInt = FALSE
  )
)

fileName <- RenderReportToFile(
  reportFilePath = GetReportFileNames()['Main Report'],
  format = 'pdf_document',
  params = appMgr$ReportArtifacts
)
browseURL(fileName)


# STEP 5 - Fit the HIV model -----------------------------------------------------------------------
aggrDataSelection <- data.table(
  Name = c(
    'Dead', 'AIDS', 'HIV', 'HIVAIDS', 'HIV_CD4_1', 'HIV_CD4_2', 'HIV_CD4_3', 'HIV_CD4_4'
  ),
  Use = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  MinYear = c(1990, 1991, 1992, 1992, 1992, 1992, 1992, 1992),
  MaxYear = c(2015, 2019, 2013, 2013, 2013, 2013, 2013, 2013)
)
appMgr$HIVModelMgr$SetAggrFilters(aggrDataSelection)

appMgr$HIVModelMgr$RunMainFit(
  settings = list(Verbose = FALSE),
  parameters = test$params,
  popCombination = test$popCombination
)


popCombination <- list(
  Case = NULL,
  Aggr = appMgr$AggrMgr$PopulationNames
)
appMgr$HIVModelMgr$RunMainFit(
  settings = list(Verbose = FALSE),
  popCombination = popCombination
)



# 1. Detailed HIV Model main fit results (rds)
names(appMgr$HIVModelMgr$MainFitResult$`0`$Results$MainOutputs)

# 2. Main outputs (txt, rds, stata)
data <- rbindlist(lapply(names(appMgr$HIVModelMgr$MainFitResult), function(iter) {
  dt <- appMgr$HIVModelMgr$MainFitResult[[iter]]$Results$MainOutputs
  dt[, ':='(
    Imputation = iter,
    Run = NULL
  )]
  setcolorder(dt, 'Imputation')
}))

# STEP 5 - Run bootstrap to get the confidence bounds estimates ------------------------------------
appMgr$HIVModelMgr$RunBootstrapFit(bsCount = 2, bsType = 'PARAMETRIC')
appMgr$HIVModelMgr$RunBootstrapFit(bsCount = 2, bsType = 'NON-PARAMETRIC')

# 3. Detailed HIV Model bootstrap results (rds)
appMgr$HIVModelMgr$BootstrapFitResult

# 4. Main outputs of bootstrap (txt, rds, stata)
succFlatList <- Filter(
  function(item) item$Results$Converged,
  Reduce(c, appMgr$HIVModelMgr$BootstrapFitResult)
)
mainOutputs <- rbindlist(lapply(succFlatList, function(res) {
  mainOutputs <- res$Results$MainOutputs
  mainOutputs[, ':='(
    DataSet = res$DataSet,
    BootIteration = res$BootIteration
  )]
  return(mainOutputs)
}))
setcolorder(
  mainOutputs,
  c('DataSet', 'BootIteration')
)

# 5. Detailed bootstrap statistics (rds)
appMgr$HIVModelMgr$BootstrapFitStats

# 6. Main output stats (rds)
bootstrap <- rbindlist(appMgr$HIVModelMgr$BootstrapFitStats$MainOutputsStats)


# STEP 6 - Explore bootstrap results ---------------------------------------------------------------
# All data sets
hist(appMgr$HIVModelMgr$BootstrapFitStats$RunTime)
table(appMgr$HIVModelMgr$BootstrapFitStats$Converged)

# Successful fits only data sets
appMgr$HIVModelMgr$BootstrapFitStats$Beta
pairs(appMgr$HIVModelMgr$BootstrapFitStats$Beta)
appMgr$HIVModelMgr$BootstrapFitStats$BetaStats

appMgr$HIVModelMgr$BootstrapFitStats$Theta
pairs(appMgr$HIVModelMgr$BootstrapFitStats$Theta)
appMgr$HIVModelMgr$BootstrapFitStats$ThetaStats


# STEP 7 - Save and load ---------------------------------------------------------------------------

saveRDS(appMgr, file = 'D:/_DEPLOYMENT/hivEstimatesAccuracy2/appMgr.rds')
appMgr <- readRDS(file = 'D:/_DEPLOYMENT/hivEstimatesAccuracy2/appMgr.rds')


data <- appMgr$CaseMgr$PreProcessedData
cat(appMgr$CaseMgr$AdjustmentTask$RunLog)
cat(appMgr$CaseMgr$AdjustmentTask$HTMLRunLog)

test <- readRDS(file = 'D:/Downloads/HIVPlatformState_20210516_201741.rds')
cat(test$UIState)

caseData <- appMgr$CaseMgr$Data
aggrData <- appMgr$AggrMgr$Data
popCombination <- list(
  Case = NULL,
  Aggr = 'pop_0'
)
aggrDataSelection <- data.table(
  Name = c(
    'AIDS'
  ),
  Use = c(TRUE),
  MinYear = c(1995),
  MaxYear = c(2005)
)
CombineData(caseData, aggrData, popCombination, aggrDataSelection)

# Migration
data <- GetMigrantData()
params <- GetMigrantParams()
migrantAIDS <- PredictInfAIDS(baseAIDS = data$AIDS, params)
migrantCD4VL <- PredictInfCD4VL(baseCD4VL = data$CD4VL, params)

# Reconciliations
reconData <- data.table::setDT(haven::read_dta('D:/VirtualBox_Shared/Migrant_test/TESSY_sample.dta'))
reconAIDS <- data.table::setDT(haven::read_dta('D:/VirtualBox_Shared/Migrant_test/baseAIDS.dta'))
reconCD4VL <- data.table::setDT(haven::read_dta('D:/VirtualBox_Shared/Migrant_test/baseCD4VL.dta'))

baseAIDS <- reconAIDS[, 1:18]
isLabelled <- sapply(baseAIDS, is.labelled)
colNames <- names(isLabelled[isLabelled])
baseAIDS[, (colNames) := lapply(.SD, as_factor), .SDcols = colNames]
setnames(
  baseAIDS,
  c(
    'Patient', 'Gender', 'Mode', 'AgeDiag', 'GroupedRegion', 'Calendar', 'Art', 'DateOfArt',
    'DateOfHIVDiagnosis', 'DateOfAIDSDiagnosis', 'DateOfArrival', 'DateOfBirth', 'AtRiskDate',
    'U', 'Mig', 'KnownPrePost', 'Id', 'DTime'
  )
)

testAIDS <- PredictInfAIDS(baseAIDS, params)

compareAIDS <- merge(
  reconAIDS[, .(PATIENT, ProbPre)],
  testAIDS[, .(Patient, ProbPre)],
  by.x = c('PATIENT'),
  by.y = c('Patient'),
  suffix = c('.Recon', '.Test'),
  all = TRUE
)
compareAIDS[, Diff := ProbPre.Recon - ProbPre.Test]
compareAIDS[abs(Diff) > 1e-7]

baseCD4VL <- reconCD4VL[, 1:27]
isLabelled <- sapply(baseCD4VL, is.labelled)
colNames <- names(isLabelled[isLabelled])
baseCD4VL[, (colNames) := lapply(.SD, as_factor), .SDcols = colNames]
setnames(
  baseCD4VL,
  c(
    'Patient', 'DateOfExam', 'YVar', 'Indi', 'Gender', 'Mode', 'AgeDiag', 'GroupedRegion',
    'Calendar', 'Art', 'DateOfArt', 'DateOfHIVDiagnosis', 'DateOfAIDSDiagnosis', 'DateOfArrival',
    'DateOfBirth', 'AtRiskDate', 'U', 'Mig', 'KnownPrePost', 'DTime', 'Id', 'Consc', 'Consr',
    'CobsTime', 'RobsTime', 'RLogObsTime2', 'Only'
  )
)

testCD4VL <- PredictInfCD4VL(baseCD4VL, params)

compareCD4VL <- merge(
  reconCD4VL[1:100, .(PATIENT, ProbPre)],
  testCD4VL[, .(Patient, ProbPre)],
  by.x = c('PATIENT'),
  by.y = c('Patient'),
  suffix = c('.Recon', '.Test'),
  all = TRUE
)
compareCD4VL[, Diff := ProbPre.Recon - ProbPre.Test]
compareCD4VL[abs(Diff) > 1e-7]
