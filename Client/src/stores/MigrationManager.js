import { observable, action, computed, makeObservable } from 'mobx';
import IsNull from '../utilities/IsNull'

export default class MigrationManager {
  rootMgr = null;

  runProgress = null;

  runLog = null;

  stats = null;

  dataCompatibleFlag = null;

  yodRegion = 'All';
  tableRegion = 'All';

  constructor(mgr) {
    this.rootMgr = mgr;
    makeObservable(this, {
      runProgress: observable,
      runLog: observable,
      yodRegion: observable,
      tableRegion: observable,
      dataCompatibleFlag: observable,
      runInProgress: computed,
      missingnessArray: computed,
      regionDistr: computed,
      yodDistr: computed,
      tableDistr: computed,
      setRunProgress: action,
      setRunLog: action,
      setStats: action,
      setYodRegion: action,
      setTableRegion: action,
      setDataCompatibleFlag: action,
      run: action,
      cancel: action
    });
  };

  get runInProgress() {
    return this.runProgress !== null;
  };

  get missingnessArray() {
    let arr = [];
    if (!IsNull(this.stats) && !IsNull(this.stats.Missingness)) {
      const excluded = this.stats.Missingness.Excluded;
      const counts = this.stats.Missingness.Count;
      arr = excluded.map((el, i) => ({
        excluded: excluded[i],
        count: counts[i],
        isTotal: /Total/.test(excluded[i])
      }));
    }
    return arr;
  };

  get regionDistr() {
    let res = null;
    if (!IsNull(this.stats) && !IsNull(this.stats.RegionDistr)) {
      res = this.stats.RegionDistr;
    }
    return res;
  };

  get yodDistr() {
    let res = null;
    if (this.yodRegion != '' && !IsNull(this.stats) && !IsNull(this.stats.YODDistr)) {
      res = this.stats.YODDistr[this.yodRegion];
    }
    return res;
  };

  get tableDistr() {
    let res = null;
    if (
      this.tableRegion != '' &&
      !IsNull(this.stats) &&
      !IsNull(this.stats.TableDistr) &&
      !IsNull(this.stats.TableDistr[this.tableRegion])
    ) {
      res = this.stats.TableDistr[this.tableRegion];
    }
    return res;
  }

  setRunProgress = progress => this.runProgress = progress;

  setRunLog = runLog => this.runLog = runLog;

  setStats = stats => this.stats = stats;

  setYodRegion = yodRegion => this.yodRegion = yodRegion;

  setTableRegion = tableRegion => this.tableRegion = tableRegion;

  setDataCompatibleFlag = dataCompatibleFlag => this.dataCompatibleFlag = dataCompatibleFlag;

  run = () => this.rootMgr.btnClicked('runMigrantBtn');

  cancel = () => this.rootMgr.btnClicked('cancelMigrantBtn');
}
