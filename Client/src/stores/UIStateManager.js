import { observable, action, computed, makeObservable } from 'mobx';
import InArray from '../utilities/InArray';
import EnsureArray from '../utilities/EnsureArray';

export default class UIStateManager {

  rootMgr = null;

  lastEventType = null;

  completedSteps = null;

  pages = [
    { title: 'Welcome', completed: false, disabled: false, subPages: [] },
    {
      title: 'Input data upload',
      completed: false,
      disabled: true,
      subPages: [
        { title: 'Case-based data', disabled: false },
        { title: 'Aggregated data', disabled: false }
      ],
      activeSubPageId: 0
    },
    {
      title: 'Case-based data summary',
      completed: false,
      disabled: true,
      subPages: [],
      description: 'Subset of case-based data can be selected here for adjustments and modelling'
    },
    {
      title: 'Adjustments',
      completed: false,
      disabled: true,
      subPages: [
        { title: 'Inputs', disabled: false },
        { title: 'Run', disabled: false },
      ],
      activeSubPageId: 0
    },
    { title: 'Migration', completed: false, disabled: false, subPages: [] },
    {
      title: 'Modelling',
      completed: false,
      disabled: true,
      subPages: [
        { title: 'Populations', disabled: false },
        { title: 'Inputs', disabled: false },
        { title: 'Advanced', disabled: false },
        { title: 'Run Main Fit', disabled: false },
        { title: 'Run Bootstrap', disabled: false },
        { title: 'Tables and charts', disabled: false }
      ],
      activeSubPageId: 0
    },
    { title: 'Reports', completed: false, disabled: true, subPages: [] },
    { title: 'Outputs', completed: false, disabled: true, subPages: [] },
  ];

  activePageId = 0;

  constructor(mgr) {
    this.rootMgr = mgr;
    makeObservable(this, {
      completedSteps: observable,
      setCompletedSteps: action,
      pages: observable,
      pagesTitles: computed,
      activePageId: observable,
      setActivePageId: action,
      setSubPageDisabledStatus: action,
      lastEventType: observable,
      setLastEventType: action,
      uploadPageEnabled: computed,
      summaryPageEnabled: computed,
      adjustmentsPageEnabled: computed,
      modellingPageEnabled: computed,
      reportsPageEnabled: computed,
      outputsPageEnabled: computed,
      caseBasedAttrMappingEnabled: computed,
      caseBasedOrigGroupingEnabled: computed,
      bootstrapEnabled: computed,
      adjustmentsOutputsEnabled: computed,
      repDelOutputsEnabled: computed,
      modellingOutputsEnabled: computed,
      bootstrapOutputsEnabled: computed,
      caseBasedDataUnloadEnabled: computed,
      aggrDataUnloadEnabled: computed
    });
  };

  get pagesTitles() {
    return this.pages.map(page => page.title);
  };

  get activeSubPageId() {
    return this.pages[this.activePageId].activeSubPageId;
  };

  setCompletedSteps = steps => {
    this.completedSteps = EnsureArray(steps);
    this.refreshPagesStatus();
  }

  setActivePageId = (pageId, subPageId = -1) => {
    if (!this.pages[pageId].disabled) {
      this.activePageId = pageId;
      if (subPageId !== -1 && subPageId < this.pages[pageId].subPages.length) {
        this.pages[pageId].activeSubPageId = subPageId;
      }
      this.refreshPagesStatus();
    } else {
      this.rootMgr.notificationsMgr.setMsg('Page this link refers to is not enabled');
    }
  };

  setSubPageDisabledStatus = (pageId, subPageId, status) => {
    this.pages[pageId].subPages[subPageId].disabled = status;
  };

  setLastEventType = eventType => this.lastEventType = eventType;

  refreshPagesStatus = () => {
    this.pages[0].completed = this.activePageId > 0;
    this.pages[1].completed = this.activePageId > 1;
    this.pages[2].completed = this.activePageId > 2;
    this.pages[3].completed = this.activePageId > 3;
    this.pages[4].completed = this.activePageId > 4;

    this.pages[1].disabled = !this.uploadPageEnabled;
    this.pages[2].disabled = !this.summaryPageEnabled;
    this.pages[3].disabled = !this.adjustmentsPageEnabled;
    this.pages[4].disabled = false;
    this.pages[5].disabled = !this.modellingPageEnabled;
    this.pages[6].disabled = !this.reportsPageEnabled;
    this.pages[7].disabled = !this.outputsPageEnabled;

    this.setSubPageDisabledStatus(5, 4, !this.bootstrapEnabled);
    this.setSubPageDisabledStatus(5, 5, !this.modellingOutputsEnabled);
  };

  get uploadPageEnabled() {
    return InArray('SESSION_INITIALIZED', this.completedSteps);
  };

  get summaryPageEnabled() {
    return InArray('CASE_BASED_SUMMARY', this.completedSteps);
  };

  get adjustmentsPageEnabled() {
    return InArray('CASE_BASED_ORIGIN_GROUPING', this.completedSteps);
  };

  get modellingPageEnabled() {
    return (
      InArray('CASE_BASED_ORIGIN_GROUPING', this.completedSteps) ||
      InArray('AGGR_READ', this.completedSteps)
    );
  };

  get reportsPageEnabled() {
    return InArray('CASE_BASED_ADJUSTMENTS', this.completedSteps);
  };

  get outputsPageEnabled() {
    return (
      InArray('CASE_BASED_ADJUSTMENTS', this.completedSteps) ||
      InArray('MODELLING', this.completedSteps)
    );
  };

  get caseBasedAttrMappingEnabled() {
    return InArray('CASE_BASED_READ', this.completedSteps);
  };

  get caseBasedDataUnloadEnabled() {
    return InArray('CASE_BASED_READ', this.completedSteps);
  };

  get aggrDataUnloadEnabled() {
    return InArray('AGGR_READ', this.completedSteps);
  };

  get caseBasedOrigGroupingEnabled() {
    return InArray('CASE_BASED_ATTR_MAPPING', this.completedSteps);
  };

  get bootstrapEnabled() {
    return InArray('MODELLING', this.completedSteps);
  };

  get nonParametricBootstrapEnabled() {
    return InArray('CASE_BASED_ORIGIN_GROUPING', this.completedSteps);
  };

  get adjustmentsOutputsEnabled() {
    return InArray('CASE_BASED_ADJUSTMENTS', this.completedSteps);
  };

  get repDelOutputsEnabled() {
    return (
      InArray('CASE_BASED_ADJUSTMENTS', this.completedSteps) &&
      InArray('REPORTING_DELAYS', this.rootMgr.adjustMgr.runAdjustmentsTypes)
    )
  };

  get modellingOutputsEnabled() {
    return InArray('MODELLING', this.completedSteps);
  };

  get bootstrapOutputsEnabled() {
    return InArray('BOOTSTRAP', this.completedSteps);
  };
}
