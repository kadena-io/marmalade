import React from "react";
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
import Chainweb from "chainweb";
import { hftAPI } from "../kadena-config.js";
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
    default:
      throw new Error(`Event converstion match failed: ${name}`);
  }
};

const parseEventParams = (convertParams, events) => {
  return _.map(events, ({name, params, ...rest}) => {
    return {
      name,
      params: convertParams(name, params),
      ...rest
    }
  })
};

const getCWDataEvents = async (name, offset, limit=50) => {
  console.debug('fetching marm events', {limit, offset})
  const raw = fetch(`http://data.testnet.chainweb.com:8080/txs/events\?name\=${name}\&limit\=${limit}\&offset\=${offset}`);
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

export const syncEventsFromCWData = async (name, limit=50, threads=4, newestToOldest=false) => {
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
  const stateObj = _.concat(..._.map(completedResults, (evs) => parseEventParams(convertMarmaladeParams, evs))).sort((a,b)=>sortEvents(a,b,newestToOldest));
  console.debug("event state obj", stateObj);
  return stateObj;
};

const orderBookRE = /marmalade.ledger.(SALE|WITHDRAW|BUY)/;

export const onlyOrderBookEvents = (evs) => evs.filter(({name})=>orderBookRE.test(name));