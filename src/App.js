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
import { syncEventsFromCWData, onlyOrderBookEvents, onlyQuoteEvents, onlyTransferEvents, onlyAcctGrdEvents, isPactContinuation, onlyMintEvents, onlyTokenEvents } from "./HFT/HftEvents.js"
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
  const [hftLedger,setHftLedger] = useState([]);
  const [hftTokens,setHftTokens] = useState([]);
  const [hftEvents, setHftEvents] = useState([]);
  const [orderBook, setOrderBook] = useState([]);
  const [quotes, setQuotes] = useState([]);
  const [mfCache,setMfCache] = createPersistedState("mfCache0")([]);

  const getHftEvents = async () => {
    const moduleHashBlacklist = [
        "LKQj2snGFz7Y8iyYlSm3uIomEAYb0C9zXCkTIPtzkPU",
        "F7tD1QlT8dx8BGyyq-h22OECYS7C3FfcYaRyxt6D1YQ",
        "WSIFGtnAlLCHFcFEHaKGrGeAG4qnTsZRj9BdvzzGa6w",
        "4m9KUKUzbd9hVZoN9uIlJkxYaf1NTz9G7Pc9C9rKTo4",
        "_1rbpI8gnHqflwb-XqHsYEFBCrLNncLplikh9XFG-y8",
        "dhIGiZIWED2Rk6zIrJxG8DeQn8n7WDKg2b5cZD2w4CU",
        "cOJgr8s3j3p5Vk0AAqjdf1TzvWZlFsAiq4pMiOzUo1w",
        "HsePyFCyYUPEPJqG5VymbQkkI3gsPAQn218uWEF_dbs",
        "lWqEvH5U20apKfBn27HOBaW46vQlxhkiDtYHZ5KoYp0",
        "uvtUnp96w2KnxnneYa4kUN1kTvYSa8Ma33UDkQXV0NA",
        "78ngDzxXE8ZyHE-kFm2h7-6Xm8N8uwU_xd1fasO8gWU"
    ]
    // Some hacky "streaming" until the streaming api is back in
    // TODO: hacky orphan detection
    const forkDepth = 100;
    const newEventHeight = hftEvents.length ? hftEvents[0]["blockHeight"] - forkDepth : 0;
    const oldHftEvents = _.dropWhile(hftEvents,({height})=>height>newEventHeight);
    const marmEvents = await syncEventsFromCWData('marmalade.', 100, 4, true, moduleHashBlacklist);
    const newHftEvents = _.takeWhile(marmEvents,({height})=>height>=newEventHeight);
    const mergedEvents = [...newHftEvents, ...oldHftEvents];
    console.debug("getHftEvents", {mergedEvents, newHftEvents, oldHftEvents});
    setHftEvents(mergedEvents);
  };

  const jankyStreaming = async () => {
    // this is just a hack until we get the steaming API online for events
    while (true) {
      await new Promise(r => setTimeout(r, 30000));
      await getHftEvents();
      console.debug('janky streaming fired...');
    }
  };

  const getHftLedgerFromEvents= (evs) => {
    // this function misses account's made via `create-account`
    let existingEntries = new Set(hftLedger.map(({ledgerKey})=>ledgerKey));
    let newEntries = [];
    const acctEvs = onlyAcctGrdEvents(evs);
    for (const {params} of acctEvs) {
      const acct = params.account;
      const tokenId = params.id;
      const acctLedgerKey = `${tokenId}:${acct}`;
      if (!existingEntries.has(acctLedgerKey) && !isPactContinuation(acct)) {
        existingEntries.add(acctLedgerKey);
        newEntries.push({
          id: tokenId,
          account: acct,
          ledgerKey: acctLedgerKey,
        });
      };
    };

    if (_.size(newEntries)) {
      const newHftLedger = [...hftLedger, ...newEntries];
      console.debug("getHftLedger", {newHftLedger});
      setHftLedger(newHftLedger);
    } else {
      console.debug("getHftLedger fired w/o new entries", {existingEntries, acctEvs, hftLedger});
    };
  };

  const getHftTokensFromEvents = (evs) => {
    let existingEntries = new Set(hftTokens.map(({id})=>id));
    let newEntries = [];
    const tokenEvs = onlyTokenEvents(evs);
    for (const {params} of tokenEvs) {
      const tokenId = params.id;
      const precision = params.precision;
      const supply = params.supply;
      const policy = params.policy;
      if (!existingEntries.has(tokenId)) {
        existingEntries.add(tokenId);
        newEntries.push({
          id: tokenId
         ,precision: precision
         ,supply: supply
         ,policy: policy
        })
      }
    };
    if (_.size(newEntries)) {
      const newHftTokens = [...hftTokens, ...newEntries];
      console.debug("getHftTokens", {newHftTokens});
      setHftTokens(newHftTokens);
    } else {
      console.debug("getHftTokens fired w/o new entries", {existingEntries, tokenEvs, hftLedger});
    }
  };

  const refresh = {
    getHftLedger: getHftEvents,
    getHftTokens: getHftEvents,
    getHftEvents: getHftEvents
  };

  useEffect(() => {
    getHftEvents();
    jankyStreaming();
    console.debug('App.useEffect[] fired');
  }, []);

  useEffect(()=>{
    setOrderBook(onlyOrderBookEvents(hftEvents));
    setQuotes(onlyQuoteEvents(hftEvents));
    getHftTokensFromEvents(hftEvents);
    getHftLedgerFromEvents(hftEvents);
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
