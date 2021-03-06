import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@material-ui/core/Grid';
import Box from '@material-ui/core/Box';
import Button from '@material-ui/core/Button';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import Divider from '@material-ui/core/Divider';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableRow from '@material-ui/core/TableRow';
import CloudUploadIcon from '@material-ui/icons/CloudUpload';
import TabPanel from '../TabPanel';
import Btn from '../Btn';
import DiagnosisRates from './DiagnosisRates';
import DiagnosisRatesSelect from './DiagnosisRatesSelect';

const TabModellingInputs = props => {
  const { appMgr } = props;

  const handleModelUploadChange = e => {
    const file = e.target.files[0];
    appMgr.modelMgr.setModelsParamFile(file);
    e.target.value = null;
  }

  const handleNextpageBtnClick = () => appMgr.uiStateMgr.setActivePageId(4, 2);

  return (
    <TabPanel>
      <Grid container spacing={2}>
        <Grid item xs={12}>
          <Box display='flex' justifyContent='flex-end'>
            <Button
              size='small'
              color='primary'
              onClick={handleNextpageBtnClick}
            >
              Next step
            </Button>
          </Box>
        </Grid>
        <Grid item xs={12}>
          <Typography variant='h6'>
            Upload model parameters file
          </Typography>
        </Grid>
        <Grid item xs={2}>
          <input
            style={{ display: 'none' }}
            id='modelUploadBtn'
            type='file'
            onChange={handleModelUploadChange}
          />
          <label htmlFor='modelUploadBtn'>
            <Btn><CloudUploadIcon />&nbsp;Upload model</Btn>
          </label>
          <Typography variant='body2' color='textSecondary' style={{ marginTop: 10 }}>
            Parameters loaded from model file override those determined from data.<br />
            Supported files types: xml (uncompressed and zip archives)
          </Typography>
        </Grid>
        <Grid item xs={10}>
          <Paper style={{ padding: 10 }}>
            <Typography variant='overline'>Uploaded file details</Typography>
            <Grid container spacing={2}>
              <Grid item xs={12}>
                <Table>
                  <TableBody>
                    <TableRow hover>
                      <TableCell width={100}>File name</TableCell>
                      <TableCell>{appMgr.modelMgr.modelsParamFileName}</TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </Grid>
            </Grid>
          </Paper>
        </Grid>
        <Grid item xs={12}>
          <Divider light style={{ margin: '30px 0' }} />
        </Grid>
        <Grid item xs={12}>
          <Typography variant='h6'>
            Time intervals and diagnosis rates modelling
          </Typography>
        </Grid>
        <Grid item xs={2}>
          <DiagnosisRatesSelect timeIntCollMgr={appMgr.modelMgr.timeIntCollMgr} />
        </Grid>
        <Grid item xs={10}>
          <DiagnosisRates timeIntCollMgr={appMgr.modelMgr.timeIntCollMgr} />
        </Grid>
      </Grid>
    </TabPanel>
  );
};

export default observer(TabModellingInputs);
