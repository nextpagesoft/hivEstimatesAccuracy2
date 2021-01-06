import React from 'react';
import { observer } from 'mobx-react';
import { makeStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Slider from '@material-ui/core/Slider';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Switch from '@material-ui/core/Switch';
import Chart from 'react-apexcharts';
import merge from 'lodash/merge';
import { defaultDiagChartOptions } from './ChartsData';

const userStyles = makeStyles({
  valueLabel: {
    '& span': {
      '& span': {
        // color: 'white',
        fontSize: 8
      }
    }
  }
});

const TabSummaryDiagYear = (props) => {
  const { appMgr } = props;
  const classes = userStyles();

  const diagChartOptions = merge(
    {},
    defaultDiagChartOptions,
    { xaxis: { categories: appMgr.summaryDataMgr.diagYearPlotData.chartCategories } }
  );

  const handleDiagYearFilterSwitchChange = e =>
    appMgr.summaryDataMgr.setDiagYearFilterApply(e.target.checked);

  const handleDiagYearFilterYearChange = (e, value) => {
    appMgr.summaryDataMgr.setDiagYearFilterMinYear(value[0]);
    appMgr.summaryDataMgr.setDiagYearFilterMaxYear(value[1]);
  };

  const diagChart = (
    <Chart
      options={diagChartOptions}
      series={appMgr.summaryDataMgr.diagYearPlotData.chartData}
      type='bar'
      height={200}
    />
  )

  return (
    <React.Fragment>
      <Grid item xs={3}>
        <Typography color='textSecondary'>
          Filter data on year of diagnosis
        </Typography>
        <FormControlLabel
          control={
            <Switch
              checked={appMgr.summaryDataMgr.diagYearPlotData.filter.applyInAdjustments}
              onChange={handleDiagYearFilterSwitchChange}
              color='primary'
              size='small'
            />
          }
          label='Apply filter in adjustments'
        />
      </Grid>
      <Grid item xs={9}>
        <Paper style={{ padding: 10 }}>
          <Typography variant='overline'>Diagnosis year</Typography>
          <div style={{ padding: '40px 105px 0 65px' }}>
            <Slider
              min={appMgr.summaryDataMgr.diagYearPlotData.filter.scaleMinYear}
              max={appMgr.summaryDataMgr.diagYearPlotData.filter.scaleMaxYear}
              marks={true}
              value={[
                appMgr.summaryDataMgr.diagYearPlotData.filter.valueMinYear,
                appMgr.summaryDataMgr.diagYearPlotData.filter.valueMaxYear
              ]}
              onChange={handleDiagYearFilterYearChange}
              valueLabelDisplay='on'
              valueLabelFormat={value => value.toFixed()}
              classes={{
                valueLabel: classes.valueLabel
              }}
              aria-labelledby='range-slider'
              getAriaLabel={index => index.toFixed()}
              getAriaValueText={value => value.toFixed()}
              color='secondary'
            />
          </div>
          {diagChart}
        </Paper>
      </Grid>
    </React.Fragment>
  )
};

export default observer(TabSummaryDiagYear);
