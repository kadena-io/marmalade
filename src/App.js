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

// TODO: switch this to use pact wallet
import { fqpAPI, fqrpAPI } from "./kadena-config.js";

import {
  hftDrawerEntries,
  HftApp
} from "./HFT/Hft.js";
import { getHftState } from "./HFT/HftState.js";
import { syncEventsFromCWData, onlyOrderBookEvents, onlyQuoteEvents } from "./HFT/HftEvents.js"
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
  const [hftLedger,setHftLedger] = createPersistedState("hftLedger6")([]);
  const [hftTokens,setHftTokens] = createPersistedState("hftTokens6")([]);
  const [mfCache,setMfCache] = createPersistedState("mfCache4")([]);
  const [hftEvents, setHftEvents] = createPersistedState("hftEvents0")([]);
  const [orderBook, setOrderBook] = createPersistedState("orderBook0")([]);
  const [quotes, setQuotes] = createPersistedState("quotes0")([]);

  const getHftEvents = async () => {
    const moduleHashBlacklist = ["WSIFGtnAlLCHFcFEHaKGrGeAG4qnTsZRj9BdvzzGa6w", "4m9KUKUzbd9hVZoN9uIlJkxYaf1NTz9G7Pc9C9rKTo4"]
    const fqpQuotes = await syncEventsFromCWData(`${fqpAPI.contractAddress}`, 100, 4, true, moduleHashBlacklist);
    const fqrpQuotes = await syncEventsFromCWData(`${fqrpAPI.contractAddress}`, 100, 4, true, moduleHashBlacklist);
    const marmEvents = await syncEventsFromCWData('marmalade.ledger', 100, 4, true, moduleHashBlacklist);
    const res = _.flatten([fqpQuotes, fqrpQuotes, marmEvents]);
    console.debug("getHftEvents", {fqpQuotes, fqrpQuotes, marmEvents});
    setHftEvents(res);
  };

  const getHftLedger = async () => {
    let res = [];
    try {
      const ledgerKeys = await getHftState("get-ledger-keys");
      for (const ledgerKey of ledgerKeys) {
        try {
          getHftState(`get-ledger-entry "${ledgerKey}"`).then(v=>{
            res.push(v);
          });
        } catch (e) {
          console.debug("get-ledger failed", {ledgerKey, e});
        }
      };
    } catch (e) {
      console.debug("get-ledger-keys failed", e);
    };
    Promise.all(res);
    setHftLedger(res);
  };

  const getHftTokens = async () => {
    let res = [];
    try {
      const tokenKeys = await getHftState("get-token-keys");
      for (const tokenKey of tokenKeys) {
        try {
          getHftState(`get-token "${tokenKey}"`).then(v=>{
            res.push(v);
          });
        } catch (e) {
          console.debug("get-token failed", {tokenKey, e});
        }
      };
    } catch (e) {
      console.debug("get-token-keys failed", e);
    };
    Promise.all(res);
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
    setOrderBook(onlyOrderBookEvents(hftEvents));
    setQuotes(onlyQuoteEvents(hftEvents));
    console.debug('App.useEffect[hftEvents,setOrderBook] fired', {quotes, orderBook});
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
      quotes={quotes}
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
