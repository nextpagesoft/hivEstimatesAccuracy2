import { observable, action, computed, makeObservable, autorun, toJS } from 'mobx';
import TimeIntervalsCollectionManager from './TimeIntervalsCollectionManager';
import LoadTxtFile from '../utilities/LoadTxtFile';
import IsNull from '../utilities/IsNull';
import FormatNumber from '../utilities/FormatNumber';

export default class ModelsManager {
  id = 'ModelsManager';
  parentMgr = null;
  timeIntCollMgr = null;

  constructor(mgr) {
    this.parentMgr = mgr;
    this.timeIntCollMgr = new TimeIntervalsCollectionManager(this);

    makeObservable(this, {
      modelsParamFile: observable,
      modelsParamFileName: observable,

      rangeYears: observable,
      optimalYears: observable,

      minYear: observable,
      maxYear: observable,
      minFitPos: observable,
      maxFitPos: observable,
      minFitCD4: observable,
      maxFitCD4: observable,
      minFitAIDS: observable,
      maxFitAIDS: observable,
      minFitHIVAIDS: observable,
      maxFitHIVAIDS: observable,
      fullData: observable,
      knotsCount: observable,
      startIncZero: observable,
      maxIncCorr: observable,
      distributionFit: observable,
      delta4Fac: observable,
      country: observable,

      modelsRunProgress: observable,
      modelsRunLog: observable,
      bootstrapRunProgress: observable,
      bootstrapRunLog: observable,
      bootstrapCount: observable,
      bootstrapType: observable,
      plotData: observable,
      setModelsParamFile: action,
      setModelsParamFileName: action,
      setMinYear: action,
      setMaxYear: action,
      setMinFitPos: action,
      setMaxFitPos: action,
      setMinFitCD4: action,
      setMaxFitCD4: action,
      setMinFitAIDS: action,
      setMaxFitAIDS: action,
      setMinFitHIVAIDS: action,
      setMaxFitHIVAIDS: action,
      setFullData: action,
      setKnotsCount: action,
      setStartIncZero: action,
      setMaxIncCorr: action,
      setDistributionFit: action,
      setDelta4Fac: action,
      setCountry: action,
      setBootstrapCount: action,
      setBootstrapType: action,
      setRangeYears: action,
      setOptimalYears: action,
      runModels: action,
      cancelModels: action,
      setModelsRunProgress: action,
      setModelsRunLog: action,
      runBootstrap: action,
      cancelBootstrap: action,
      modelsRunInProgress: computed,
      bootstrapRunInProgress: computed,
      gofTable1Data: computed,
      gofTable2Data: computed,
      gofTable3Data: computed,
      gofTable4Data: computed,
      gofTable5Data: computed,
      gofTable6Data: computed,
      gofTable7Data: computed,
      outputTable1Data: computed,
      outputTable2Data: computed,
      outputTable3Data: computed,
      outputTable4Data: computed,
      mainOutputTableData: computed
    });

    autorun(() => {
      this.timeIntCollMgr.setMinYear(this.minYear);
    });

    autorun(() => {
      this.timeIntCollMgr.setMaxYear(this.maxYear);
    });
  };

  // File details
  modelsParamFile = null;
  modelsParamFileName = '';

  // Parameters
  rangeYears = null;
  optimalYears = null;

  minYear = 1980;
  maxYear = 2016;
  minFitPos = 1979;
  maxFitPos = 1979;
  minFitCD4 = 1984;
  maxFitCD4 = 2016;
  minFitAIDS = 1980;
  maxFitAIDS = 1995;
  minFitHIVAIDS = 1996;
  maxFitHIVAIDS = 2016;

  fullData = true;
  knotsCount = 4;
  startIncZero = true;
  maxIncCorr = true;
  distributionFit = 'POISSON';
  delta4Fac = 0;
  country = 'OTHER';
  bootstrapCount = 100;
  bootstrapType = 'PARAMETRIC';

  // Run details
  modelsRunProgress = null;
  modelsRunLog = null;
  bootstrapRunProgress = null;
  bootstrapRunLog = null;

  plotData = null;

  setModelsParamFile = paramFile => {
    if (paramFile) {
      this.modelsParamFile = paramFile;
      this.modelsParamFileName = paramFile.name;
      LoadTxtFile(this.modelsParamFile).then(
        action('success', content => {
          this.parentMgr.btnClicked('xmlModel', content);
        }),
        action('error', error => console.log(error))
      );
    }
  };
  setModelsParamFileName = fileName => this.modelsParamFileName = fileName;
  setRangeYears = rangeYears => this.rangeYears = rangeYears;
  setOptimalYears = optimalYears => {
    this.optimalYears = optimalYears;
    this.setMinYear(optimalYears.All[0]);
    this.setMaxYear(optimalYears.All[1]);
    this.setMinFitPos(optimalYears.HIV[0]);
    this.setMaxFitPos(optimalYears.HIV[1]);
    this.setMinFitCD4(optimalYears.HIVCD4[0]);
    this.setMaxFitCD4(optimalYears.HIVCD4[1]);
    this.setMinFitAIDS(optimalYears.AIDS[0]);
    this.setMaxFitAIDS(optimalYears.AIDS[1]);
    this.setMinFitHIVAIDS(optimalYears.HIVAIDS[0]);
    this.setMaxFitHIVAIDS(optimalYears.HIVAIDS[1]);
  };
  setMinYear = minYear =>
    this.minYear = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(minYear)), this.optimalYears.All[1]);
  setMaxYear = maxYear =>
    this.maxYear = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(maxYear)), this.optimalYears.All[1]);
  setMinFitPos = minFitPos =>
    this.minFitPos = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(minFitPos)), this.optimalYears.All[1]);
  setMaxFitPos = maxFitPos =>
    this.maxFitPos = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(maxFitPos)), this.optimalYears.All[1]);
  setMinFitCD4 = minFitCD4 =>
    this.minFitCD4 = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(minFitCD4)), this.optimalYears.All[1]);
  setMaxFitCD4 = maxFitCD4 =>
    this.maxFitCD4 = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(maxFitCD4)), this.optimalYears.All[1]);
  setMinFitAIDS = minFitAIDS =>
    this.minFitAIDS = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(minFitAIDS)), this.optimalYears.All[1]);
  setMaxFitAIDS = maxFitAIDS =>
    this.maxFitAIDS = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(maxFitAIDS)), this.optimalYears.All[1]);
  setMinFitHIVAIDS = minFitHIVAIDS =>
    this.minFitHIVAIDS = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(minFitHIVAIDS)), this.optimalYears.All[1]);
  setMaxFitHIVAIDS = maxFitHIVAIDS =>
    this.maxFitHIVAIDS = Math.min(Math.max(this.optimalYears.All[0] - 1, parseInt(maxFitHIVAIDS)), this.optimalYears.All[1]);
  setFullData = fullData => this.fullData = fullData;
  setKnotsCount = knotsCount => this.knotsCount = parseInt(knotsCount);
  setStartIncZero = startIncZero => this.startIncZero = startIncZero;
  setMaxIncCorr = maxIncCorr => this.maxIncCorr = maxIncCorr;
  setDistributionFit = distributionFit => this.distributionFit = distributionFit;
  setDelta4Fac = delta4Fac => this.delta4Fac = parseFloat(delta4Fac);
  setCountry = country => this.country = country;
  setBootstrapCount = bootstrapCount => this.bootstrapCount = bootstrapCount;
  setBootstrapType = bootstrapType => this.bootstrapType = bootstrapType;
  setPlotData = plotData => this.plotData = plotData;

  setModelsRunProgress = progress => this.modelsRunProgress = progress;
  setModelsRunLog = runLog => this.modelsRunLog = runLog;
  runModels = () => {
    const params = {
      minYear: this.minYear,
      maxYear: this.maxYear,
      minFitPos: this.minFitPos,
      maxFitPos: this.maxFitPos,
      minFitCD4: this.minFitCD4,
      maxFitCD4: this.maxFitCD4,
      minFitAIDS: this.minFitAIDS,
      maxFitAIDS: this.maxFitAIDS,
      minFitHIVAIDS: this.minFitHIVAIDS,
      maxFitHIVAIDS: this.maxFitHIVAIDS,
      fullData: this.fullData,
      knotsCount: this.knotsCount,
      startIncZero: this.startIncZero,
      maxIncCorr: this.maxIncCorr,
      distributionFit: this.distributionFit,
      delta4Fac: this.delta4Fac,
      country: this.country,
      timeIntervals: toJS(this.timeIntCollMgr.selectedRunCollection.intervals),
      popCombination: toJS(this.parentMgr.popCombMgr.selectedCombination)
    };
    this.parentMgr.btnClicked('runModelBtn:HIVModelParams', params);
  };

  cancelModels = () => this.parentMgr.btnClicked('cancelModelBtn');

  setBootstrapRunProgress = progress => this.bootstrapRunProgress = progress;

  setBootstrapRunLog = runLog => this.bootstrapRunLog = runLog;

  runBootstrap = () => this.parentMgr.btnClicked('runBootstrapBtn', {
    count: this.bootstrapCount,
    type: this.bootstrapType
  });

  cancelBootstrap = () => this.parentMgr.btnClicked('cancelBootstrapBtn');

  get modelsRunInProgress() {
    return this.modelsRunProgress !== null;
  };

  get bootstrapRunInProgress() {
    return this.bootstrapRunProgress !== null;
  };

  get gofTable1Data() {
    return (this.getTableData(['Year', 'N_HIV_D', 'N_HIV_Obs_M', 'N_HIV_Obs_M_LB', 'N_HIV_Obs_M_UB']));
  };

  get gofTable2Data() {
    return (this.getTableData(['Year', 'N_CD4_1_D', 'N_CD4_1_Obs_M', 'N_CD4_1_Obs_M_LB', 'N_CD4_1_Obs_M_UB']));
  };

  get gofTable3Data() {
    return (this.getTableData(['Year', 'N_CD4_2_D', 'N_CD4_2_Obs_M', 'N_CD4_2_Obs_M_LB', 'N_CD4_2_Obs_M_UB']));
  };

  get gofTable4Data() {
    return (this.getTableData(['Year', 'N_CD4_3_D', 'N_CD4_3_Obs_M', 'N_CD4_3_Obs_M_LB', 'N_CD4_3_Obs_M_UB']));
  };

  get gofTable5Data() {
    return (this.getTableData(['Year', 'N_CD4_4_D', 'N_CD4_4_Obs_M', 'N_CD4_4_Obs_M_LB', 'N_CD4_4_Obs_M_UB']));
  };

  get gofTable6Data() {
    return (this.getTableData(['Year', 'N_HIVAIDS_D', 'N_HIVAIDS_Obs_M', 'N_HIVAIDS_Obs_M_LB', 'N_HIVAIDS_Obs_M_UB']));
  };

  get gofTable7Data() {
    return (this.getTableData(['Year', 'N_AIDS_D', 'N_AIDS_M', 'N_AIDS_M_LB', 'N_AIDS_M_UB']));
  };

  get outputTable1Data() {
    return (this.getTableData(['Year', 'N_Inf_M', 'N_Inf_M_LB', 'N_Inf_M_UB']));
  };

  get outputTable2Data() {
    return (this.getTableData(['Year', 't_diag', 't_diag_LB', 't_diag_UB']));
  };

  get outputTable3Data() {
    return (this.getTableData(['Year', 'N_Alive', 'N_Alive_LB', 'N_Alive_UB']));
  };

  get outputTable4Data() {
    return (this.getTableData(['Year', 'N_Und_Alive_p', 'N_Und_Alive_p_LB', 'N_Und_Alive_p_UB']));
  };

  get mainOutputTableData() {
    return (this.getTableData([
      'Year',
      'N_HIV_D', 'N_HIV_Obs_M',
      'N_CD4_1_D', 'N_CD4_2_D', 'N_CD4_3_D', 'N_CD4_4_D',
      'N_CD4_1_Obs_M', 'N_CD4_2_Obs_M', 'N_CD4_3_Obs_M', 'N_CD4_4_Obs_M',
      'N_AIDS_D', 'N_AIDS_M',
      'N_HIVAIDS_D', 'N_HIVAIDS_Obs_M',
      'N_Inf_M',
      't_diag', 't_diag_p25', 't_diag_p50', 't_diag_p75',
      'N_Alive', 'N_Alive_Diag_M',
      'N_Und', 'N_Und_Alive_p',
      'N_Und_CD4_3_M', 'N_Und_CD4_4_M'
    ]));
  };

  getTableData = colNames => {
    if (!IsNull(this.plotData)) {
      let finalColNames = colNames.filter(colName => !IsNull(this.plotData[colName]));

      const data = finalColNames.map(colName => {
        if (colName === 'Year') {
          return (this.plotData.Year);
        } else {
          return (this.plotData[colName].map(val => FormatNumber(val)));
        }
      })

      return ({
        ColNames: finalColNames,
        Data: data
      });
    } else {
      return ({
        ColNames: [],
        Data: []
      });
    }
  }
}
