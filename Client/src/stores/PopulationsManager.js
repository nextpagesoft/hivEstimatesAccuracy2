import { observable, computed, action, toJS, makeObservable } from 'mobx';
import RemoveElementsFromArray from '../utilities/RemoveElementsFromArray';

export default class PopulationsManager {
  rootMgr = null;

  constructor(mgr) {
    this.rootMgr = mgr;
    makeObservable(this, {
      populations: observable,
      availableStrata: observable,
      addEmptyPopulation: action,
      removePopulations: action,
      setPopulationName: action,
      setPopulationStrata: action,
      populationsJS: computed,
      availableStrataNames: computed,
      definedPopulations: computed,
    });
  }

  availableStrata = {
    Gender : ['F', 'M'],
    Transmission: ['IDU', 'MSM']
  };

  populations = [];

  addEmptyPopulation = () => {
    this.populations.push({
      name: 'Strata',
      strata: [],
      populations: []
    });
  };

  setAvailableStrata = strata => this.availableStrata = strata;

  removePopulations = selectedIds => {
    this.populations = RemoveElementsFromArray(this.populations, selectedIds);
  };

  setPopulationName = (i, name) => {
    this.populations[i].name = name;
  };

  setPopulationStrata = (i, strata) => {
    this.populations[i].strata = strata;
    this.recreatePopulations(i);
  };

  get populationsJS() {
    return toJS(this.populations);
  };

  get availableStrataNames() {
    return Object.keys(this.availableStrata);
  };

  get definedPopulations() {
    return [...new Set(this.populations.map(el => el.populations).flat())];
  };

  recreatePopulations = (i) => {
    let strataGroup = this.populations[i];
    let populations = [];

    if (strataGroup.strata.length > 0) {
      populations = this.availableStrata[strataGroup.strata[0]];
      for (let j = 1; j < strataGroup.strata.length; ++j) {
        let extraPopulations = this.availableStrata[strataGroup.strata[j]];
        populations = populations.map(el1 => extraPopulations.map(el2 => [el1, el2].join('_'))).flat();
      }
    }
    this.populations[i].populations = populations;
  };
}
