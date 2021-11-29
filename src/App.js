//basic React api imports
import React, { useState, useEffect } from "react";
import {
  Switch,
} from 'react-router-dom';
import { 
  useQueryParams,
  StringParam,
  withDefault
 } from 'use-query-params';
import createPersistedState from 'use-persisted-state';
import _ from 'lodash';
//semantic ui for styling
import {
  Container} from '@material-ui/core';
import { NavDrawer } from "./NavDrawer.js";

import { 
  walletDrawerEntries,
  WalletApp,
 } from "./PactWallet.js";

import {
  hftDrawerEntries,
  HftApp
} from "./HFT/Hft.js";
import { getHftState } from "./HFT/HftState.js";

const App = () => {
  //Top level UI Routing Params
  const [appRoute,setAppRoute] = useQueryParams({
    "app": withDefault(StringParam,"wallet"),
    "ui": withDefault(StringParam,"config")
  });

  // Tx Status Top Level State
  const [txStatus, setTxStatus] = useState("");
  const [tx, setTx] = useState({});
  const [txRes, setTxRes] = useState({});
  const pactTxStatus = {
    tx:tx,setTx:setTx,
    txStatus:txStatus,setTxStatus:setTxStatus,
    txRes:txRes,setTxRes:setTxRes,
  };

  //HFT Top Level States
  const [hftLedger,setHftLedger] = createPersistedState("hftLedger4")({});
  const [hftTokens,setHftTokens] = createPersistedState("hftTokens4")({});
  const [mfCache,setMfCache] = createPersistedState("mfCache4")([]);

  const getHftLedger = async () => {
    const res = await getHftState("view-ledger");
    setHftLedger(res);
  }

  const getHftTokens = async () => {
    const res = await getHftState("view-tokens");
    setHftTokens(res);
  }

  const refresh = {
    getHftLedger: getHftLedger,
    getHftTokens: getHftTokens
  };

  const refreshAll = async () => _.forIn(refresh,(k,v) => v());

  useEffect(() => {
    getHftLedger();
    getHftTokens();
    console.debug('App.useEffect[] fired');
  }, []);

  return (
          <NavDrawer
            entriesList={[
                [ walletDrawerEntries
                , hftDrawerEntries
                ]
          ]}>
          <Container>
            <Switch>
  { appRoute.app === "wallet" ?
    <WalletApp
      appRoute={appRoute}
      setAppRoute={setAppRoute}
    />
    : appRoute.app === "hft" ?
    <HftApp 
      appRoute={appRoute}
      setAppRoute={setAppRoute}
      hftLedger={hftLedger}
      hftTokens={hftTokens}
      mfCache={mfCache}
      setMfCache={setMfCache}
      pactTxStatus={pactTxStatus}
      refresh={refresh}
      />
    : <React.Fragment>
        {() => {
          console.log("Redirecting, this didn't match:", appRoute);
          setAppRoute({app:"wallet",ui:"config"})}
        }
      </React.Fragment>
    }
            </Switch>
          </Container>
          </NavDrawer>
  );
};

export default App;
