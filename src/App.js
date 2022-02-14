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
import { syncEventsFromCWData, onlyOrderBookEvents } from "./HFT/HftEvents.js"
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
  const [hftLedger,setHftLedger] = createPersistedState("hftLedger6")({});
  const [hftTokens,setHftTokens] = createPersistedState("hftTokens6")({});
  const [mfCache,setMfCache] = createPersistedState("mfCache4")([]);
  const [hftEvents, setHftEvents] = createPersistedState("hftEvents0")([]);
  const [orderBook, setOrderBook] = createPersistedState("orderBook0")([]);

  const getHftEvents = async () => {
    const res = await syncEventsFromCWData('marmalade.ledger', 100, 4, true);
    setHftEvents(res);
  };

  const getHftLedger = async () => {
    const res = await getHftState("get-ledger");
    setHftLedger(res);
  };

  const getHftTokens = async () => {
    const res = await getHftState("get-tokens");
    setHftTokens(res);
  };

  const refresh = {
    getHftLedger: getHftLedger,
    getHftTokens: getHftTokens,
    getHftEvents: getHftEvents
  };

  const refreshAll = async () => _.forIn(refresh,(k,v) => v());

  useEffect(() => {
    getHftLedger();
    getHftTokens();
    getHftEvents();
    console.debug('App.useEffect[] fired');
  }, []);

  useEffect(()=>{
    setOrderBook(onlyOrderBookEvents(hftEvents))
    console.debug('App.useEffect[hftEvents,setOrderBook] fired');
  },[hftEvents]);

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
      hftEvents={hftEvents}
      orderBook={orderBook}
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
