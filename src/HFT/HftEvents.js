import React from "react";
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
import Chainweb from "chainweb";
import { hftAPI } from "../kadena-config.js";
import { PactJsonListAsTable, dashStyleNames2Text } from "../util.js";

export const saleBlocks = [1896893, 1896912];

export const getHftEvents = async (blocks) => {
  const hftEvents = await Promise.all(
     blocks.map(b => {
       return getHftBlockEvents(b);
     })
   );
  console.log("HFT Events:", {"events": hftEvents.flat()})
  return hftEvents;
}

export const getSaleEvents = async (blocks) => {
  const saleEvents = await Promise.all(
     blocks.map(b => {
       return getSaleBlockEvents(b);
     })
   );
  const flattenedEvents = saleEvents.flat();
  const saleEventsInfo = flattenedEvents.map(e => saleEvent(e));
  console.log("Sale Events:", {"rawEvents": flattenedEvents, "saleEvents": saleEventsInfo})
  return flattenedEvents;
}

export const getSaleBlockEvents = async (block) => {
  const events = await getBlockEvents(block);
  return filterSaleEvent(events);
};

export const getHftBlockEvents = async (block) => {
  const events = await getBlockEvents(block);
  return filterHftEvent(events);
};

export const filterHftEvent = (events) => {
    return filterEvents(events, hftAPI.namespace, hftAPI.contractName);
};

export const filterSaleEvent = (events) => {
    return filterEvents(events, hftAPI.namespace, hftAPI.contractName, "SALE");
};

export const getBlockEvents = async (block) => {
    const res = await Pact.event.height(hftAPI.meta.chainId, block, hftAPI.meta.networkId, hftAPI.meta.apiHost)
    console.debug('getBlockEvents', {block, res});
    return(res);
};

const wrapSync = async (start) => {
    const t2 = performance.now();
    console.debug(`testBlockEventsSync starting on ${start}, with params:`, hftAPI.meta.chainId, start, (start + 999), hftAPI.meta.networkId, hftAPI.meta.apiHost);
    const res = await Chainweb.block.range(hftAPI.meta.chainId, start, (start + 999), hftAPI.meta.networkId, hftAPI.meta.apiHost);
    console.debug(`testBlockEventsSync finished [${start},${start+999}] taking ${((performance.now()-t2)/1000)} seconds for ${res.length} blocks`);
    return res
}

export const testBlockEventsSync = async () => {
  let start = 1673987;
  let end = 1933987;
  const t0 = performance.now();
  var res = [];
  while (start < end) {
    res.push(wrapSync(start));
    start = (start + 1000);
  };
  const results = await Promise.all(res);
  const t1 = performance.now();
  console.debug(`testBlockEventsSync ended takeing ${((t1-t0)/1000)} seconds for ${results.length} results`);
};

export const filterEvents = (events, ns, mod, evName) => {
  return events.filter(ev => {
    if (ns && ns !== ev.module.namespace) return false;
    if (mod && mod !== ev.module.name) return false;
    if (evName && evName !== ev.name) return false;
    else return true;
  })
};

const saleEvent = (event) => {
  return {
    "eventName": `${event.module.namespace}.${event.module.name}.${event.name}`,
    "token": event.params[0],
    "seller": event.params[1],
    "amount": event.params[2],
    "timeout": event.params[3].int,
    "sale-id": event.params[4]
  }
}



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

const rawEventToStateRep = (convertParams, {blockHash, name, params, idx, ...rest}) => {
  return {
    name,
    params: convertParams(name, params),
    rest: {blockHash, ...rest}
  }
};

const eventsToStateObj = (convertParams, events) => {
  const s = _.map(events, (ev) => {
    return {
      [ev.height]: {
        [ev.blockHash]: {
          [ev.idx] : rawEventToStateRep(convertParams, ev)
        }
      }
    }
  })
  return _.merge(...s);
}

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

const countEventsInStateObj = (obj) => {
  var cnt = 0;
  _.forEach(obj, (hashObj, bheight) => _.forEach(hashObj, (idxs) => cnt = cnt + _.size(idxs)));
  return cnt
}

export const syncEventsFromCWData = async (name, limit=50, threads=4) => {
  console.debug("starting to get marm events")
  var offset = 0;
  var promisedResults = [];
  var completedResults = [];
  var stateObj = {};
  var continueSync = true;
  while (continueSync) {
    console.log("doing batch", {offset, limit, threads});
    for (var i = 0; i < threads; i++) {
      promisedResults.push(getCWDataEvents(name, offset, limit));
      offset = (offset + limit);
    };
    completedResults = await Promise.all(promisedResults);
    // once a batch comes back empty, we're caught up
    continueSync = _.every(_.map(completedResults, (v) => v.length));
  };
  _.merge(stateObj, ..._.map(completedResults, (evs) => eventsToStateObj(convertMarmaladeParams, evs)));
  return stateObj;
};