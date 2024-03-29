import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@mui/material/Grid';
import Typography from '@mui/material/Typography';
import TextField from '@mui/material/TextField';
import FormGroup from '@mui/material/FormGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import Button from '@mui/material/Button';
import Checkbox from '@mui/material/Checkbox';

const TabAdjustmentsInputsRDWithout = (props) => {
  const { appMgr } = props;

  const handleRDWithoutStartYearChange = (e) => appMgr.adjustMgr.setRDWithoutStartYear(e.target.value);
  const handleRDWithoutStartYearBlur = (e) =>
    appMgr.adjustMgr.setRDWithoutStartYear(Math.min(Math.max(e.target.value, 1975), 2030));

  const handleRDWithoutEndYearChange = (e) => appMgr.adjustMgr.setRDWithoutEndYear(e.target.value);
  const handleRDWithoutEndYearBlur = (e) =>
    appMgr.adjustMgr.setRDWithoutEndYear(Math.min(Math.max(e.target.value, 1975), 2030));

  const handleRDWithoutEndQrtChange = (e) => appMgr.adjustMgr.setRDWithoutEndQrt(e.target.value);
  const handleRDWithoutEndQrtBlur = (e) =>
    appMgr.adjustMgr.setRDWithoutEndQrt(Math.min(Math.max(e.target.value, 1), 4));

  const handleRDWithoutStratGenderChange = (e, value) => appMgr.adjustMgr.setRDWithoutStratGender(value);
  const handleRDWithoutStratTransChange = (e, value) => appMgr.adjustMgr.setRDWithoutStratTrans(value);
  const handleRDWithoutStratMigrChange = (e, value) => appMgr.adjustMgr.setRDWithoutStratMigr(value);
  const handleRDRestoreDefaults = (e) => appMgr.adjustMgr.restoreRDDefaults('withoutTrend');

  return (
    <React.Fragment>
      <Typography variant='overline'>Reporting Delays - without trend parameters</Typography>
      <form noValidate autoComplete='off'>
        <Grid container spacing={2}>
          <Grid item xs={4}>
            <TextField
              label='Diagnosis start year'
              helperText='Enter the start year for diagnosis'
              type='number'
              value={appMgr.adjustMgr.rdWithoutTrendSettings.startYear}
              onChange={handleRDWithoutStartYearChange}
              onBlur={handleRDWithoutStartYearBlur}
              fullWidth
              InputProps={{
                inputProps: {
                  min: 1975, max: 2030
                }
              }}
              style={{ marginBottom: 20 }}
            />
          </Grid>
          <Grid item xs={4}>
            <TextField
              label='Notification end year'
              helperText='Enter the end year for notification'
              type='number'
              value={appMgr.adjustMgr.rdWithoutTrendSettings.endYear}
              onChange={handleRDWithoutEndYearChange}
              onBlur={handleRDWithoutEndYearBlur}
              fullWidth
              InputProps={{
                inputProps: {
                  min: 1975, max: 2030
                }
              }}
              style={{ marginBottom: 20 }}
            />
          </Grid>
          <Grid item xs={4}>
            <TextField
              label='Notification end quarter (integer between 1 and 4)'
              type='number'
              value={appMgr.adjustMgr.rdWithoutTrendSettings.endQrt}
              onChange={handleRDWithoutEndQrtChange}
              onBlur={handleRDWithoutEndQrtBlur}
              fullWidth
              InputProps={{
                inputProps: {
                  min: 1, max: 4
                }
              }}
              style={{ marginBottom: 20 }}
            />
          </Grid>
          <Grid item xs={4}>
            Stratify by:
            <FormGroup row>
              <FormControlLabel
                control={<Checkbox checked={appMgr.adjustMgr.rdWithoutTrendSettings.stratGender} onChange={handleRDWithoutStratGenderChange} name='check' color='primary' />}
                label='Gender'
              />
              <FormControlLabel
                control={<Checkbox checked={appMgr.adjustMgr.rdWithoutTrendSettings.stratTrans} onChange={handleRDWithoutStratTransChange} name='check' color='primary' />}
                label='Transmission'
              />
              <FormControlLabel
                control={<Checkbox checked={appMgr.adjustMgr.rdWithoutTrendSettings.stratMigr} onChange={handleRDWithoutStratMigrChange} name='check' color='primary' />}
                label='Migration'
              />
            </FormGroup>
          </Grid>
          <Grid item xs={12}>
            <Button color='primary' onClick={handleRDRestoreDefaults}>Restore defaults</Button>
          </Grid>
        </Grid>
      </form>
    </React.Fragment>
  )
};

export default observer(TabAdjustmentsInputsRDWithout);
