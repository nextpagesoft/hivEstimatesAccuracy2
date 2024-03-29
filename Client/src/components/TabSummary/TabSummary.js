import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@mui/material/Grid';
import Button from '@mui/material/Button';
import Divider from '@mui/material/Divider';
import Box from '@mui/material/Box';
import Typography from '@mui/material/Typography';
import TabPanel from '../TabPanel';
import TabSummaryDiagYear from './TabSummaryDiagYear';
import TabSummaryNotifQuarter from './TabSummaryNotifQuarter';
import TabSummaryMissingness from './TabSummaryMissingness';
import TabSummaryReportingDelays from './TabSummaryReportingDelays';
import FormatPercentage from '../../utilities/FormatPercentage';

const TabSummary = (props) => {

  const { appMgr } = props;

  const handleNextStepBtnClick = () => appMgr.uiStateMgr.setActiveStepId(3);

  return (
    <TabPanel>
      <Grid container spacing={2}>
        <Grid item xs={12}>
          <Box display='flex' justifyContent='flex-end'>
            <Button
              size='small'
              color='primary'
              disabled={!appMgr.uiStateMgr.caseBasedAdjustmentsStageEnabled}
              onClick={handleNextStepBtnClick}
            >
              Next step
            </Button>
          </Box>
        </Grid>
        <Grid item xs={12}>
          <Typography variant='h6'>
            Select case-based data for summary
          </Typography>
        </Grid>
        <TabSummaryDiagYear {...props} />
        <TabSummaryNotifQuarter {...props} />
        <Grid item xs={12}>
          <Typography variant='body1'>
            Number of records in the selection: {appMgr.summaryDataMgr.selectedCount} (out of {appMgr.summaryDataMgr.totalCount}, {FormatPercentage(appMgr.summaryDataMgr.selectedCount / appMgr.summaryDataMgr.totalCount)})
          </Typography>
        </Grid>
        <Grid item xs={12}>
          <Divider light sx={{ margin: '30px 0px' }} />
        </Grid>
        <TabSummaryMissingness {...props} />
        <Grid item xs={12}>
          <Divider light sx={{ margin: '30px 0px' }} />
        </Grid>
        <TabSummaryReportingDelays {...props} />
      </Grid>
    </TabPanel>
  )
};

export default observer(TabSummary);
