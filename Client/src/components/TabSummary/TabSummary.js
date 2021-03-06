import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';
import Divider from '@material-ui/core/Divider';
import Box from '@material-ui/core/Box';
import Typography from '@material-ui/core/Typography';
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
          <Divider light style={{ margin: '30px 0' }} />
        </Grid>
        <TabSummaryMissingness {...props} />
        <Grid item xs={12}>
          <Divider light style={{ margin: '30px 0' }} />
        </Grid>
        <TabSummaryReportingDelays {...props} />
      </Grid>
    </TabPanel>
  )
};

export default observer(TabSummary);
