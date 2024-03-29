import React from 'react';
import { observer } from 'mobx-react';
import Table from '@mui/material/Table';
import TableHead from '@mui/material/TableHead';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableRow from '@mui/material/TableRow';
import Typography from '@mui/material/Typography';
import Alert from '@mui/material/Alert';
import Divider from '@mui/material/Divider';
import Grid from '@mui/material/Grid';
import Paper from '@mui/material/Paper';
import Select from '@mui/material/Select';
import Checkbox from '@mui/material/Checkbox';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import FormHelperText from '@mui/material/FormHelperText';
import ListSubheader from '@mui/material/ListSubheader';
import OriginGroupingRow from './OriginGroupingRow';
import EnhancedTableToolbar from '../EnhancedTableToolbar';
import MessageAlert from '../MessageAlert';
import RemoveValuesFromArray from '../../utilities/RemoveValuesFromArray';

const OriginGroupingsWidget = (props) => {
  const { appMgr } = props;
  const [selected, setSelected] = React.useState([]);
  const distribution = appMgr.origGroupMgr.distributionArray;
  const groupings = appMgr.origGroupMgr.groupingsJS;

  const handleGroupingPresetChange = e => {
    const preset = e.target.value;
    appMgr.inputValueSet('groupingPresetSelect', preset);
    appMgr.origGroupMgr.setPreset(preset);
  };

  const handleSelectAllClick = e => {
    if (e.target.checked) {
      const newSelectedIds = groupings.map((el, i) => i);
      setSelected(newSelectedIds);
      return;
    }
    setSelected([]);
  }

  const handleSelectClick = i => e => {
    const selectedIndex = selected.indexOf(i);

    let newSelected = [];
    if (selectedIndex === -1) {
      newSelected = newSelected.concat(selected, i);
    } else {
      newSelected = RemoveValuesFromArray(selected, i);
    }
    setSelected(newSelected);
  };

  const handleAddClick = () => {
    appMgr.origGroupMgr.addEmptyGrouping();
  };

  const handleDeleteClick = () => {
    appMgr.origGroupMgr.removeGroupings(selected);
    setSelected([]);
  };

  const rowCount = groupings.length;
  const selectedCount = selected.length;
  const isSelected = i => selected.indexOf(i) !== -1;

  return (
    <Paper sx={{ padding: '10px' }}>
      <Grid container spacing={2}>
        <Grid item xs={2}>
          <Typography variant='overline'>Distribution of region of origin</Typography>
          <Table size='small'>
            <TableHead>
              <TableRow hover={false}>
                <TableCell>FullRegionOfOrigin</TableCell>
                <TableCell align='right'>Count</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {
                distribution.map((el, i) => (
                  <TableRow key={i}>
                    <TableCell>{el.origin}</TableCell>
                    <TableCell align='right'>{el.count}</TableCell>
                  </TableRow>
                ))
              }
            </TableBody>
          </Table>
        </Grid>
        <Grid item xs={10}>
          <Typography variant='overline'>Migrant variable regrouping</Typography>
          <FormControl sx={{ width: '100%', fontSize: '0.75rem' }}>
            <Select
              value={appMgr.origGroupMgr.preset}
              onChange={handleGroupingPresetChange}
              sx={{
                width: '100%',
                fontSize: '0.75rem'
              }}
            >
              <ListSubheader><Typography variant='overline'>General presets</Typography></ListSubheader>
              <MenuItem value='REPCOUNTRY + UNK + OTHER' dense>REPCOUNTRY + UNK + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + SUB-SAHARAN AFRICA + OTHER' dense>REPCOUNTRY + UNK + SUB-SAHARAN AFRICA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' dense>REPCOUNTRY + UNK + 3 most prevalent regions + OTHER</MenuItem>
              <MenuItem value='Custom' dense>Custom</MenuItem>
              <ListSubheader><Divider /></ListSubheader>
              <ListSubheader><Typography variant='overline'>Migrant-module specific presets</Typography></ListSubheader>
              <MenuItem value='REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + OTHER' dense>REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + OTHER' dense>REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-NORTH AMERICA-OTHER + AFRICA + ASIA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' dense>REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' dense>REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' dense>REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-NORTH AMERICA-OTHER + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' dense>REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-NORTH AMERICA-OTHER + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' dense>REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' dense>REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-NORTH AMERICA-OTHER + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER</MenuItem>
            </Select>
            <FormHelperText>Select regrouping preset</FormHelperText>
          </FormControl>
          <Table>
            <TableHead>
              <TableRow hover={false}>
                <TableCell padding='checkbox'>
                  <Checkbox
                    inputProps={{ 'aria-label': 'select all' }}
                    color='primary'
                    onClick={handleSelectAllClick}
                    checked={rowCount > 0 && selectedCount === rowCount}
                  />
                </TableCell>
                <TableCell width={250} sx={{padding: '0px 20px 0px 0px'}}>Grouped Region Of Origin</TableCell>
                <TableCell width={250} padding='none'>Region For Migration Module Parameter</TableCell>
                <TableCell>FullRegionOfOrigin</TableCell>
                <TableCell align='right' width='10%'>Count</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {
                groupings.map((el, i) => (
                  <OriginGroupingRow
                    key={i}
                    i={i}
                    el={el}
                    appMgr={appMgr}
                    isSelected={isSelected(i)}
                    onSelectClick={handleSelectClick(i)}
                  />
                ))
              }
            </TableBody>
          </Table>
          <EnhancedTableToolbar
            selectedCount={selectedCount}
            onAddClick={handleAddClick}
            onDeleteClick={handleDeleteClick}
          />
          <Alert severity='info'>
            Records with value "UNK" are removed from the dataset processed for migration.<br />
            Region "CARIBBEAN-LATIN AMERICA" is combined with region "OTHER" for migration module parameter.
          </Alert>
          <MessageAlert
            valid={appMgr.origGroupMgr.migrantCompatibleStatus}
            msg={appMgr.origGroupMgr.migrantCompatibleMessage}
          />
        </Grid>
      </Grid>
    </Paper>
  )
};

export default observer(OriginGroupingsWidget);
