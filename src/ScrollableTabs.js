import React from 'react';
import { 
  useQueryParam,
  NumberParam,
  withDefault
 } from 'use-query-params';
import _ from "lodash";
import { makeStyles } from '@material-ui/core/styles';
import {
  AppBar,
  Tab,
  Tabs,
  Typography,
  Box,
} from '@material-ui/core';

const TabPanel = (props) => {
  const { children, value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`scrollable-auto-tabpanel-${index}`}
      aria-labelledby={`scrollable-auto-tab-${index}`}
      {...other}
    >
      {value === index && (
        <Box p={3}>
          <Typography>{children}</Typography>
        </Box>
      )}
    </div>
  );
}

function a11yProps(index) {
  return {
    id: `scrollable-auto-tab-${index}`,
    'aria-controls': `scrollable-auto-tabpanel-${index}`,
  };
}

const useStyles = makeStyles((theme) => ({
  root: {
    flexGrow: 1,
    width: '100%',
    backgroundColor: theme.palette.background.paper,
    marginTop: theme.spacing(4),
  },
}));

export const ScrollableTabs = (props) => {
  const { tabEntries, tabIdx } = props;
  const classes = useStyles();
  const [value, setValue] = useQueryParam(tabIdx,withDefault(NumberParam,0));
  
  if (value >= _.size(tabEntries)) {setValue(0)}

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  return (
    <div className={classes.root}>
      <AppBar position="static" color="default">
        <Tabs
          value={value}
          onChange={handleChange}
          indicatorColor="primary"
          textColor="primary"
          variant="scrollable"
          scrollButtons="auto"
          aria-label="scrollable auto tabs example"
        >
          {tabEntries.map((entry,idx)=>
            <Tab label={entry.label} {...a11yProps(idx)}/>
          )}
        </Tabs>
      </AppBar>
      {tabEntries.map((entry,idx)=>
        <TabPanel value={value} index={idx}>
          {entry.component}
        </TabPanel>
      )}
    </div>
  );
}
