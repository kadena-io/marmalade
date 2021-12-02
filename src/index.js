import React from 'react';
import ReactDOM from 'react-dom';
import {
  BrowserRouter as Router,
  Route,
} from 'react-router-dom';
import { 
  QueryParamProvider,
 } from 'use-query-params';
import {
  createMuiTheme,
  ThemeProvider,
 } from '@material-ui/core/styles';
//semantic ui for styling
import {
  CssBaseline, NoSsr
} from '@material-ui/core';
import App from './App';
import * as serviceWorker from './serviceWorker';

import { hftAPI, manifestAPI, gtpAPI, fqpAPI, globalConfig } from './kadena-config.js';
import { PactWallet } from './PactWallet.js';

const Main = () => {
  const theme = React.useMemo(
      () =>
        createMuiTheme({
          palette: {
            primary: {
              light: '#cb4584',
              main: '#F78D1E',
              dark: '#cb4584',
              contrastText: '#fff',
            },
            secondary: {
              light: '#ffffff',
              main: '#e3e8ed',
              dark: '#b1b6bb',
              contrastText: '#000',
            },
          },
        }),
      [],
    );
  return (
  <React.StrictMode>
    <ThemeProvider theme={theme}>
      <NoSsr>
        <CssBaseline/>
        <PactWallet contractConfigs={{hftAPI, manifestAPI, gtpAPI, fqpAPI}} globalConfig={globalConfig}>
          <Router basename={process.env.PUBLIC_URL}>
          <QueryParamProvider ReactRouterRoute={Route}>
            <App />
          </QueryParamProvider>
          </Router>
        </PactWallet>
      </NoSsr>
    </ThemeProvider>
  </React.StrictMode>
  )
};

ReactDOM.render(<Main/>,document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
