import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import HIVChart from '../Charts/HIVChart';
import IsNull from '../../utilities/IsNull';
import SmallTable from './SmallTable';

const TabModellingOutputsGOF = props => {

  const { appMgr } = props;
  if (IsNull(appMgr.modelMgr.plotData)) {
    return (null);
  }

  return (
    <Grid container spacing={2} style={{ marginTop: 20 }}>
      <Grid item xs={12}>
        <Typography variant='h6'>
          A. HIV diagnoses, total
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData ={appMgr.modelMgr.gofTable1Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_HIV_D}
          model={appMgr.modelMgr.plotData.N_HIV_Obs_M}
          min={appMgr.modelMgr.plotData.N_HIV_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_HIV_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          B. HIV diagnoses, CD4 {'\u2265'} 500
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable2Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_CD4_1_D}
          model={appMgr.modelMgr.plotData.N_CD4_1_Obs_M}
          min={appMgr.modelMgr.plotData.N_CD4_1_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_CD4_1_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          C. HIV diagnoses, CD4 350 - 499
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable3Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_CD4_2_D}
          model={appMgr.modelMgr.plotData.N_CD4_2_Obs_M}
          min={appMgr.modelMgr.plotData.N_CD4_2_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_CD4_2_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          D. HIV diagnoses, CD4 200 - 349
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable4Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_CD4_3_D}
          model={appMgr.modelMgr.plotData.N_CD4_3_Obs_M}
          min={appMgr.modelMgr.plotData.N_CD4_3_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_CD4_3_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          E. HIV diagnoses, CD4 {'<'} 200
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable5Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_CD4_4_D}
          model={appMgr.modelMgr.plotData.N_CD4_4_Obs_M}
          min={appMgr.modelMgr.plotData.N_CD4_4_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_CD4_4_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          F. HIV/AIDS diagnoses
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable6Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_HIVAIDS_D}
          model={appMgr.modelMgr.plotData.N_HIVAIDS_Obs_M}
          min={appMgr.modelMgr.plotData.N_HIVAIDS_Obs_M_LB}
          range={appMgr.modelMgr.plotData.N_HIVAIDS_Obs_M_Range}
        />
      </Grid>

      <Grid item xs={12}>
        <Typography variant='h6'>
          G. AIDS diagnoses, total
        </Typography>
      </Grid>
      <Grid item xs={5}>
        <SmallTable tableData={appMgr.modelMgr.gofTable7Data} />
      </Grid>
      <Grid item xs={7}>
        <HIVChart
          year={appMgr.modelMgr.plotData.Year}
          data={appMgr.modelMgr.plotData.N_AIDS_D}
          model={appMgr.modelMgr.plotData.N_AIDS_M}
          min={appMgr.modelMgr.plotData.N_AIDS_M_LB}
          range={appMgr.modelMgr.plotData.N_AIDS_M_Range}
        />
      </Grid>
    </Grid>
  );
};

export default observer(TabModellingOutputsGOF);
