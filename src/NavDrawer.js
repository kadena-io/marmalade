import React from 'react';
import { 
  useQueryParams,
  StringParam,
  withDefault
 } from 'use-query-params';
import _ from 'lodash';
import {
  AppBar,
  Collapse,
  Divider,
  Drawer,
  Button,
  Hidden,
  IconButton,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  Menu,
  MenuItem,
  Toolbar,
} from '@material-ui/core';
import { makeStyles, useTheme, withStyles } from '@material-ui/core/styles';

import AccountBalanceWalletIcon from '@material-ui/icons/AccountBalanceWallet';
import ExpandLess from '@material-ui/icons/ExpandLess';
import ExpandMore from '@material-ui/icons/ExpandMore';
import MenuIcon from '@material-ui/icons/Menu';

import logo from "./kadena_r_rev_3_whi_lor.png";

import { usePactWalletContext } from './PactWallet.js';

const drawerWidth = 240;

const useStyles = makeStyles((theme) => ({
  root: {
    display: 'flex',
  },
  drawer: {
    [theme.breakpoints.up('sm')]: {
      width: drawerWidth,
      flexShrink: 0,
    },
  },
  appBar: {
    [theme.breakpoints.up('sm')]: {
      zIndex: theme.zIndex.drawer + 1,
    },
  },
  menuButton: {
    marginRight: theme.spacing(2),
    [theme.breakpoints.up('sm')]: {
      display: 'none',
    },
  },
  // necessary for content to be below app bar
  toolbar: theme.mixins.toolbar,
  drawerPaper: {
    width: drawerWidth,
  },
  content: {
    flexGrow: 1,
    padding: theme.spacing(3),
  },
  nested: {
    paddingLeft: theme.spacing(4),
  },
  logo: {
    height: "60px",
  },
  logoDiv: {
    flexGrow: 1,
  }
}));

const ListItemLink = (props) => {
  const { icon, primary, to, subList } = props;
  const hasSubList = subList && Array.isArray(subList) ;
  const [open, setOpen] = React.useState(true);
  const classes = useStyles();
  //Top level UI Routing Params
  const [,setAppRoute] = useQueryParams({
    "app": withDefault(StringParam,"init"),
    "ui": withDefault(StringParam,"guardians")
  });


  return (
    <React.Fragment>
      <li>
        <ListItem button onClick={()=> {return (to ? setAppRoute(to) : setOpen(!open))}}>
          {icon ? <ListItemIcon>{icon}</ListItemIcon> : null}
          <ListItemText primary={primary} />
          {hasSubList ? (
            open ? <ExpandLess onClick={()=>setOpen(!open)} />
                 : <ExpandMore onClick={()=>setOpen(!open)} />
            ) : null}
        </ListItem>
      </li>
        {hasSubList ?
          <Collapse in={open} timeout="auto" unmountOnExit>
            <List component="div" disablePadding className={classes.nested} dense>
              {subList.map(entry =>
                <ListItemLink
                  icon={entry.icon}
                  primary={entry.primary}
                  to={entry.to}
                  subList={entry.subList}
                  />
              )}
            </List>
          </Collapse>
        : null }
    </React.Fragment>
  );
};

const StyledMenuItem = withStyles((theme) => ({
  root: {
    '&:focus': {
      backgroundColor: theme.palette.warning.main,
      '& .MuiListItemIcon-root, & .MuiListItemText-primary': {
        color: theme.palette.common.white,
      },
    },
  },
}))(MenuItem);

export const NavDrawer = (props) => {
  const { window, entriesList } = props;
  const {wallet: {current, otherWallets}, walletDispatch} = usePactWalletContext();
  const classes = useStyles();
  const theme = useTheme();
  const [mobileOpen, setMobileOpen] = React.useState(false);
  const [anchorEl, setAnchorEl] = React.useState(null);
  const openWalletMenu = Boolean(anchorEl);

  const handleWalletMenu = (event) => {
    setAnchorEl(event.currentTarget);
  };

  const handleWalletMenuClose = (newWallet) => {
    setAnchorEl(null);
    if (typeof newWallet === 'string') {
      walletDispatch({type:'updateWallet',newWallet:otherWallets[newWallet]})
    }
  };

  const handleDrawerToggle = () => {
    setMobileOpen(!mobileOpen);
  };

  const drawer = (
    <div>
      <div className={classes.toolbar} />
      {entriesList.map(entries =>
        <React.Fragment>
          <Divider />
          { entries !== null && <List>
            {entries.map(entry =>
              <>
              { entry && 
                <ListItemLink
                icon={entry.icon}
                primary={entry.primary}
                to={entry.to}
                subList={entry.subList}
                /> 
                }
              </>
            )}
          </List>}
        </React.Fragment>
       )}
    </div>
  );

  const container = window !== undefined ? () => window().document.body : undefined;

  return (
    <div className={classes.root}>
      <AppBar position="fixed" className={classes.appBar}>
        <Toolbar>
          <IconButton
            color="inherit"
            aria-label="open drawer"
            edge="start"
            onClick={handleDrawerToggle}
            className={classes.menuButton}
          >
            <MenuIcon />
          </IconButton>
          <div className={classes.logoDiv}>
            <img src={logo} alt="logo" className={classes.logo}/>
          </div>
          {current.walletName && (
            <div>
              <Button
                variant="outlined"
                color="secondary"
                onClick={handleWalletMenu}
                startIcon={<AccountBalanceWalletIcon edge="end" />}
              >
                {current.walletName}
              </Button>
              <Menu
                id="menu-appbar"
                anchorEl={anchorEl}
                anchorOrigin={{
                  vertical: 'top',
                  horizontal: 'right',
                }}
                keepMounted
                transformOrigin={{
                  vertical: 'top',
                  horizontal: 'right',
                }}
                open={openWalletMenu}
                onClose={handleWalletMenuClose}
              >
                {_.keys(otherWallets).map((name)=><MenuItem onClick={()=>handleWalletMenuClose(name)}>{name}</MenuItem>)}
              </Menu>
            </div>
          )}
        </Toolbar>
      </AppBar>
      <nav className={classes.drawer} aria-label="mailbox folders">
        {/* The implementation can be swapped with js to avoid SEO duplication of links. */}
        <Hidden smUp implementation="css">
          <Drawer
            container={container}
            variant="temporary"
            anchor={theme.direction === 'rtl' ? 'right' : 'left'}
            open={mobileOpen}
            onClose={handleDrawerToggle}
            classes={{
              paper: classes.drawerPaper,
            }}
            ModalProps={{
              keepMounted: true, // Better open performance on mobile.
            }}
          >
            {drawer}
          </Drawer>
        </Hidden>
        <Hidden xsDown implementation="css">
          <Drawer
            classes={{
              paper: classes.drawerPaper,
            }}
            variant="permanent"
            open
          >
            {drawer}
          </Drawer>
        </Hidden>
      </nav>
      <main className={classes.content}>
        <div className={classes.toolbar} />
        {props.children}
      </main>
    </div>
  );
}
