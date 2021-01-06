import React from 'react';
import { observer } from 'mobx-react';
import Typography from '@material-ui/core/Typography';
import TextField from '@material-ui/core/TextField';
import Slider from '@material-ui/core/Slider';
import FormGroup from '@material-ui/core/FormGroup';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Button from '@material-ui/core/Button';
import Checkbox from '@material-ui/core/Checkbox';

const TabAdjustmentsInputsMIMice = (props) => {
  const { appMgr } = props;

  const handleMIMiceNimpChange = (e) => appMgr.adjustMgr.setMIMiceNimp(e.target.value);
  const handleMIMiceNitChange = (e) => appMgr.adjustMgr.setMIMiceNit(e.target.value);
  const handleMIMiceNsdfChange = (e, value) => appMgr.adjustMgr.setMIMiceNsdf(value);
  const handleMIMiceImputeRDChange = (e, value) => appMgr.adjustMgr.setMIMiceImputeRD(value);
  const handleMIRestoreDefaults = (e) => appMgr.adjustMgr.restoreMIDefaults('mice');

  return (
    <React.Fragment>
      <Typography variant='overline'>Chained Equations - MICE parameters</Typography>
      <form noValidate autoComplete='off' style={{ width: 400 }}>
        <TextField
          label='Number of imputations'
          helperText='Type the number of data sets to input'
          type='number'
          value={appMgr.adjustMgr.miMiceSettings.nimp}
          onChange={handleMIMiceNimpChange}
          fullWidth
          variant='filled'
          style={{ marginBottom: 20 }}
        />
        <TextField
          label='Number of mice iterations'
          helperText='Type the number of mice iterations'
          type='number'
          value={appMgr.adjustMgr.miMiceSettings.nit}
          onChange={handleMIMiceNitChange}
          fullWidth
          variant='filled'
          style={{ marginBottom: 20 }}
        />
        <Typography id="discrete-slider" gutterBottom>
          Number of degrees of freedom for spline of diagnosis calendar year
          </Typography>
        <Slider
          min={3}
          max={5}
          value={appMgr.adjustMgr.miMiceSettings.nsdf}
          onChange={handleMIMiceNsdfChange}
          marks={[{ value: 3, label: 3 }, { value: 4, label: 4 }, { value: 5, label: 5 }]}
        />
        <FormGroup row>
          <FormControlLabel
            control={<Checkbox checked={appMgr.adjustMgr.miMiceSettings.imputeRD} onChange={handleMIMiceImputeRDChange} name='check' color='primary' />}
            label='Impute reporting delays inputs'
          />
        </FormGroup>
        <Button color='primary' onClick={handleMIRestoreDefaults}>Restore defaults</Button>
      </form>
    </React.Fragment>
  )
};

export default observer(TabAdjustmentsInputsMIMice);
