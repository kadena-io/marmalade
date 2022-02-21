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
import { syncEventsFromCWData, onlyOrderBookEvents, onlyQuoteEvents, onlyTransferEvents, isPactContinuation, onlyMintEvents, onlyTokenEvents } from "./HFT/HftEvents.js"
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
  const [mfCache,setMfCache] = useState([]);
  const [hftEvents, setHftEvents] = useState([]);
  const [orderBook, setOrderBook] = useState([]);
  const [quotes, setQuotes] = useState([]);

  const getHftEvents = async () => {
    const moduleHashBlacklist = ["WSIFGtnAlLCHFcFEHaKGrGeAG4qnTsZRj9BdvzzGa6w", "4m9KUKUzbd9hVZoN9uIlJkxYaf1NTz9G7Pc9C9rKTo4"]
    const fqpQuotes = await syncEventsFromCWData(`${fqpAPI.contractAddress}`, 100, 4, true, moduleHashBlacklist);
    const fqrpQuotes = await syncEventsFromCWData(`${fqrpAPI.contractAddress}`, 100, 4, true, moduleHashBlacklist);
    const marmEvents = await syncEventsFromCWData('marmalade.ledger', 100, 4, true, moduleHashBlacklist);
    const res = _.flatten([fqpQuotes, fqrpQuotes, marmEvents]);
    console.debug("getHftEvents", {fqpQuotes, fqrpQuotes, marmEvents});
    setHftEvents(res);
  };

  const getHftLedgerFromEvents= (evs) => {
    // this function misses account's made via `create-account`
    let existingEntries = new Set(hftLedger.map(({ledgerKey})=>ledgerKey));
    let newEntries = []; 
    const transferEvs = onlyTransferEvents(evs);
    for (const {params} of transferEvs) {
      const sender = params.sender;
      const receiver = params.receiver;
      const tokenId = params.id;
      const senderLedgerKey = `${tokenId}:${sender}`;
      const receiverLedgerKey = `${tokenId}:${receiver}`;
      if (!existingEntries.has(senderLedgerKey) && !isPactContinuation(sender)) {
        existingEntries.add(senderLedgerKey);
        newEntries.push({
          id: tokenId,
          account: sender, 
          ledgerKey: senderLedgerKey,
        });
      }; 
      if (!existingEntries.has(receiverLedgerKey) && !isPactContinuation(receiver)) {
        existingEntries.add(receiverLedgerKey);
        newEntries.push({
          id: tokenId,
          account: receiver, 
          ledgerKey: receiverLedgerKey,
        });
      };
    };

    if (_.size(newEntries)) {
      const newHftLedger = [...hftLedger, ...newEntries];
      console.debug("getHftLedger", {newHftLedger});
      setHftLedger(newHftLedger);
    } else {
      console.debug("getHftLedger fired w/o new entries", {existingEntries, transferEvs, hftLedger});
    };
  };

  const getHftTokensFromEvents = (evs) => {
    let existingEntries = new Set(hftTokens.map(({id})=>id));
    let newEntries = [];
    const tokenEvs = onlyTokenEvents(evs);
    for (const {params} of tokenEvs) {
      const tokenId = params.id;
      if (!existingEntries.has(tokenId)) {
        existingEntries.add(tokenId);
        newEntries.push({
          id: tokenId
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
