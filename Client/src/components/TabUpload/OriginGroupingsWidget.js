import React from 'react';
import { observer } from 'mobx-react';
import Table from '@material-ui/core/Table';
import TableHead from '@material-ui/core/TableHead';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableRow from '@material-ui/core/TableRow';
import Typography from '@material-ui/core/Typography';
import Grid from '@material-ui/core/Grid';
import Divider from '@material-ui/core/Divider';
import Paper from '@material-ui/core/Paper';
import Select from '@material-ui/core/Select';
import Checkbox from '@material-ui/core/Checkbox';
import MenuItem from '@material-ui/core/MenuItem';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import InputLabel from '@material-ui/core/InputLabel';
import OriginGroupingRow from './OriginGroupingRow';
import EnhancedTableToolbar from '../EnhancedTableToolbar';

const OriginGroupingsWidget = (props) => {
  const { appMgr } = props;
  const [selected, setSelected] = React.useState([]);

  const distribution = appMgr.origGroupMgr.distributionArray;
  const groupings = appMgr.origGroupMgr.groupingsJS;

  const handleGroupingPresetChange = e => {
    appMgr.inputValueSet('groupingPresetSelect', e.target.value);
    appMgr.origGroupMgr.setType(e.target.value);
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
    } else if (selectedIndex === 0) {
      newSelected = newSelected.concat(selected.slice(1));
    } else if (selectedIndex === selected.length - 1) {
      newSelected = newSelected.concat(selected.slice(0, -1));
    } else if (selectedIndex > 0) {
      newSelected = newSelected.concat(
        selected.slice(0, selectedIndex),
        selected.slice(selectedIndex + 1),
      );
    }

    setSelected(newSelected);
  };

  const handleDeleteClick = () => {
    appMgr.origGroupMgr.removeGroupings(selected);
    setSelected([]);
  }

  const handleAddClick = () => {
    appMgr.origGroupMgr.addEmptyGrouping();
  }

  const rowCount = groupings.length;
  const selectedCount = selected.length;
  const isSelected = i => selected.indexOf(i) !== -1;

  return (
    <Paper style={{ padding: 10 }}>
      <Grid container spacing={2}>
        <Grid item xs={3}>
          <Typography variant='overline'>Distribution of region of origin</Typography>
          <Table size='small' style={{ backgroundColor: 'rgba(0, 0, 0, 0.02)'}}>
            <TableHead>
              <TableRow>
                <TableCell>FullRegionOfOrigin</TableCell><TableCell align='right'>Count</TableCell>
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
        <Grid item xs={9}>
          <Typography variant='overline'>Migrant variable regrouping</Typography>
          <FormControl style={{ width: '100%', fontSize: '0.75rem' }}>
            <InputLabel>
              Preset
             </InputLabel>
            <Select value={appMgr.origGroupMgr.type} onChange={handleGroupingPresetChange}>
              <MenuItem value='REPCOUNTRY + UNK + OTHER' dense>REPCOUNTRY + UNK + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + SUBAFR + OTHER' dense>REPCOUNTRY + UNK + SUBAFR + OTHER</MenuItem>
              <MenuItem value='REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' dense>REPCOUNTRY + UNK + 3 most prevalent regions + OTHER</MenuItem>
              <MenuItem value='Custom' dense>Custom</MenuItem>
            </Select>
            <FormHelperText>Select regrouping preset</FormHelperText>
          </FormControl>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell padding='checkbox'>
                  <Checkbox
                    inputProps={{ 'aria-label': 'select all' }}
                    color='primary'
                    onClick={handleSelectAllClick}
                    checked={rowCount > 0 && selectedCount === rowCount}
                  />
                </TableCell>
                <TableCell padding='none'>GroupedRegionOfOrigin</TableCell>
                <TableCell width='60%'>FullRegionOfOrigin</TableCell>
                <TableCell align='right' width='10%'>Count</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {
                groupings.map((el, i) => (
                  <OriginGroupingRow
                    key={i}
                    i={i}
                    grouping={el}
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
        </Grid>
      </Grid>
    </Paper>
  )
};

export default observer(OriginGroupingsWidget);
