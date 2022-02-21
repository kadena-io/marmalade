import React from "react";
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
import Chainweb from "chainweb";
import { hftAPI, fqpAPI, fqrpAPI, globalConfig } from "../kadena-config.js";
import { PactJsonListAsTable, dashStyleNames2Text } from "../util.js";

/** takes a pactRep and makes it more friendly */
const convertPactRep = (val) => {
  if (typeof val === 'object' && val !== null) {
    const ks = Object.keys(val);
    if (ks.length > 1) {
      return val;
    } else if (ks[0] === "int" || ks[0] === "decimal") {
      if (typeof val[ks[0]] === 'number') { return val[ks[0]].toString(); }
      else { return val[ks[0]]}
    } else if (ks[0] === "time" || ks[0] === "timep") {
      return val[ks[0]];
    } else {
      throw new Error(`Pact base type converstion match failed: ${val}`);
    }
  } else { return val; }
}

const convertMarmaladeParams = (name, params) => {
  const ps = _.map(params, convertPactRep);
  switch (name) {
    case `${hftAPI.namespace}.${hftAPI.contractName}.TRANSFER`:
      return _.zipObject(["id", "sender", "receiver", "amount"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.SUPPLY`:
      return _.zipObject(["id", "supply"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.TOKEN`:
      return _.zipObject(["id"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.ROTATE`:
      return _.zipObject(["id", "account"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.MINT`:
      return _.zipObject(["id", "account", "amount"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.BURN`:
      return _.zipObject(["id", "account", "amount"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.SALE`:
      return _.zipObject(["id", "seller", "amount", "timeout", "sale-id"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.OFFER`:
      return _.zipObject(["id", "seller", "amount", "timeout"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.WITHDRAW`:
      return _.zipObject(["id", "seller", "amount", "timeout", "sale-id"], ps);
    case `${hftAPI.namespace}.${hftAPI.contractName}.BUY`:
      return _.zipObject(["id", "buyer", "seller", "amount", "timeout", "sale-id"], ps);
    case `${fqpAPI.namespace}.${fqpAPI.contractName}.QUOTE`:
      return _.zipObject(["sale-id", "token-id", "amount", "price", "sale-price", "spec"], ps);
    case `${fqrpAPI.namespace}.${fqrpAPI.contractName}.QUOTE`:
      return _.zipObject(["sale-id", "token-id", "amount", "price", "sale-price", "royalty-payout", "creator", "spec"], ps);
    default:
      throw new Error(`Event converstion match failed: ${name} -- ${ps}`);
  }
};

const parseEventParams = (convertParams, {name, params, ...rest}) => {
  return {
    name,
    params: convertParams(name, params),
    ...rest
  }
};

const getCWDataEvents = async (name, offset, limit=50) => {
  console.debug('fetching marm events', {limit, offset})
  const raw = fetch(`http://data.testnet.chainweb.com:8080/txs/events\?name\=${name}\&limit\=${limit}\&offset\=${offset}`);
  //const raw = fetch(`http://${globalConfig.dataHost}/txs/events\?name\=${name}\&limit\=${limit}\&offset\=${offset}`);
  const rawRes = await raw;
  const res = await rawRes;
  if (res.ok){
     const resJSON = await rawRes.json();
     return resJSON;
   } else {
     const resTEXT = await rawRes.text();
     return resTEXT;
   }
};

const sortEvents = (ev1, ev2, newestToOldest=false) => {
  return newestToOldest ? ev2.height-ev1.height : ev1.height-ev2.height;
};

export const syncEventsFromCWData = async (name, limit=50, threads=4, newestToOldest=false, moduleHashBlacklist=[]) => {
  console.debug(`starting to get ${name} events`)
  var offset = 0;
  var promisedResults = [];
  var completedResults = [];
  var continueSync = true;
  while (continueSync) {
    console.debug(`${name} events, doing batch`, {offset, limit, threads});
    for (var i = 0; i < threads; i++) {
      promisedResults.push(getCWDataEvents(name, offset, limit));
      offset = (offset + limit);
    };
    completedResults = await Promise.all(promisedResults);
    // once a batch comes back empty, we're caught up
    continueSync = _.every(_.map(completedResults, (v) => v.length >= limit));
  };
  // console.debug(`${name} raw events`, _.flatten(completedResults));
  completedResults = _.filter(_.flatten(completedResults), ({moduleHash}) => {return !moduleHashBlacklist.includes(moduleHash);});
  const stateObj = _.map(completedResults, (ev) => parseEventParams(convertMarmaladeParams, ev)).sort((a,b)=>sortEvents(a,b,newestToOldest));
  console.debug(`${name}'s events`, stateObj);
  return stateObj;
};

const orderBookRE = new RegExp(String.raw`${hftAPI.contractAddress}.(SALE|WITHDRAW|BUY)`);
export const onlyOrderBookEvents = (evs) => evs.filter(({name})=>orderBookRE.test(name));

const saleEvRE = new RegExp(String.raw`${hftAPI.contractAddress}.SALE`);
export const onlySaleEvents = (evs) => evs.filter(({name})=>saleEvRE.test(name));

const quoteEvRE = new RegExp(String.raw`(${fqpAPI.contractAddress}|${fqrpAPI.contractAddress}).QUOTE`);
export const onlyQuoteEvents = (evs) => evs.filter(({name})=>quoteEvRE.test(name));

const transferEvRE = new RegExp(String.raw`${hftAPI.contractAddress}.TRANSFER`);
export const onlyTransferEvents = (evs) => evs.filter(({name})=>transferEvRE.test(name));

const mintEvRE = new RegExp(String.raw`${hftAPI.contractAddress}.MINT`);
export const onlyMintEvents = (evs) => evs.filter(({name})=>mintEvRE.test(name));

const tokenEvRE = new RegExp(String.raw`${hftAPI.contractAddress}.TOKEN`);
export const onlyTokenEvents = (evs) => evs.filter(({name})=>tokenEvRE.test(name));

export const getSaleForQuote = (orderBook, saleId) => _.find(onlySaleEvents(orderBook), {requestKey: saleId});

const pactTxRe = new RegExp('sale-.*')
export const isPactContinuation = (ledgerKey) => pactTxRe.test(ledgerKey); 

// TODO: finish this, blocked on bug in Chainweb.js that defaults host back to mainnet. This is for id-ing orphaned events
// export const getQuotesForSaleEvents = async (evs) => {
//   let p = {};
//   for (const {name, requestKey, blockHash} of evs) {
//     console.debug([hftAPI.meta.chainId, blockHash, hftAPI.meta.networkId, hftAPI.meta.apiHost]);
//     debugger;
//     p[requestKey] = await Chainweb.transaction.blockHash(hftAPI.meta.chainId, blockHash, hftAPI.meta.networkId, hftAPI.meta.apiHost);
//   };
//   // await Promise.allSettled(p);
//   console.debug("getQuotesForSaleEvents", p);
//   return p;
// }