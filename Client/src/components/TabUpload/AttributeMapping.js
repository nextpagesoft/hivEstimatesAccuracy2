import React from 'react';
import { observer } from 'mobx-react';
import Grid from '@material-ui/core/Grid';
import Table from '@material-ui/core/Table';
import TableHead from '@material-ui/core/TableHead';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableRow from '@material-ui/core/TableRow';
import Typography from '@material-ui/core/Typography';
import Paper from '@material-ui/core/Paper';
import Select from '@material-ui/core/Select';
import Input from '@material-ui/core/Input';
import MenuItem from '@material-ui/core/MenuItem';
import AssignmentIcon from '@material-ui/icons/Assignment';
import CheckIcon from '@material-ui/icons/Check';
import ErrorIcon from '@material-ui/icons/Error';
import Btn from '../Btn';

const AttributeMapping = (props) => {
  const { appMgr } = props;

  let attrMappingSelectOptions = [];
  if (appMgr.caseBasedDataMgr.columnNames !== null) {
    attrMappingSelectOptions = appMgr.caseBasedDataMgr.columnNames.slice().sort().map(colName => (
      <MenuItem key={colName} value={colName} dense>{colName}</MenuItem>
    ));
  }

  const onApplyBtnClick = () => appMgr.attrMappingMgr.applyMapping();

  const onOrigColSelect = attribute => e => {
    appMgr.attrMappingMgr.setOrigCol(attribute, e.target.value);
  }

  const onDefValChange = attribute => e => {
    appMgr.attrMappingMgr.setDefVal(attribute, e.target.value);
  }

  const attrMappingTableRows = appMgr.attrMappingMgr.mapping.map((entry, idx) => (
    <TableRow hover key={idx}>
      <TableCell>{entry.attribute}</TableCell>
      <TableCell style={{ padding: '4px 16px 0px 16px' }}>
        <Select
          style={{ width: '100%', fontSize: '0.75rem' }}
          value={entry.origColName || ''}
          onChange={onOrigColSelect(entry.attribute)}
          disableUnderline
        >
          <MenuItem value='' dense>&nbsp;</MenuItem>
          {attrMappingSelectOptions}
        </Select>
      </TableCell>
      <TableCell style={{ padding: '4px 16px 0px 16px' }}>
        <Input
          style={{ width: '100%', fontSize: '0.75rem' }}
          onChange={onDefValChange(entry.attribute)}
          disabled={!!entry.origColName}
        />
      </TableCell>
    </TableRow>
  ));

  const validIcon = appMgr.attrMappingMgr.valid ?
    <CheckIcon style={{ width: '0.75rem', height: '0.75rem' }} /> :
    <ErrorIcon style={{ width: '0.75rem', height: '0.75rem' }} />

  return (
    <Grid container spacing={2}>
      <Grid item xs={3}>
        <Btn
          style={{ marginBottom: 6 }}
          onClick={onApplyBtnClick}
          disabled={!appMgr.attrMappingMgr.valid}
        >
          <AssignmentIcon />&nbsp;Apply mapping
        </Btn>
        <Typography variant='body2' color='textSecondary'>
          Input data set to be mapped to internal attributes.<br />
          Adjust mapping and press 'Apply mapping' button.
        </Typography>
        <Typography variant='body2' style={{marginTop: 10}}>
          {validIcon}&nbsp;
          {appMgr.attrMappingMgr.msg}
        </Typography>
      </Grid>
      <Grid item xs={9}>
        <Paper style={{ padding: 10 }}>
          <Typography variant='overline'>Attribute mapping</Typography>
          <Grid container spacing={2}>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell width='30%'>Attribute</TableCell>
                  <TableCell width='40%'>Uploaded data column</TableCell>
                  <TableCell width='30%'>Override value</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {attrMappingTableRows}
              </TableBody>
            </Table>
          </Grid>
        </Paper>
      </Grid>
    </Grid>
  );
};

export default observer(AttributeMapping);
