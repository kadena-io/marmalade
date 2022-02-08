import React from "react";
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
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
    return(res);
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
