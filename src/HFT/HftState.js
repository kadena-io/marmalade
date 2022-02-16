//basic React api imports
import React from "react";
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
import { hftAPI } from "../kadena-config.js";
import { PactJsonListAsTable, dashStyleNames2Text } from "../util.js";
import { getSaleForQuote } from "./HftEvents.js";

export const getHftState = async (cmd) => {
  //calling get-all() function from smart contract
    const res = await Pact.fetch.local(
      {
        pactCode: `(${hftAPI.contractAddress}.${cmd})`,
        //pact-lang-api function to construct transaction meta data
        meta: Pact.lang.mkMeta(
          hftAPI.meta.sender,
          hftAPI.meta.chainId,
          hftAPI.meta.gasPrice,
          hftAPI.meta.gasLimit,
          hftAPI.meta.creationTime(),
          hftAPI.meta.ttl
        ),
      },
      hftAPI.meta.host
    );
    const all = res.result.data;
    //sorts memories by least recent
    console.debug(`local query data: (${hftAPI.contractAddress}.${cmd})`,all);
    return(all);
};

export const RenderHftLedger = ({hftLedger}) => {
  const pretty = _.map(hftLedger,v=> {return {id: v.id, contents: v};});
  console.debug("renderHFTLedger", {hftLedger,pretty});
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["ID", "Contents"]}
    keyOrder={["id","contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={1}
          enableClipboard={false}
          displayDataTypes={false}
          displayObjectSize={false}
        />}
      }
    }
    keyFormatter={dashStyleNames2Text}
    />
  )
};

export const RenderHftTokens = ({hftTokens}) => {
  const pretty = _.map(hftTokens,v=> {return {id: v.id, contents: v};});
  console.debug("renderHFTLedger", {hftTokens,pretty});
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["ID", "Contents"]}
    keyOrder={["id","contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={1}
          enableClipboard={false}
          displayDataTypes={false}
          displayObjectSize={false}
        />}
      }
    }
    keyFormatter={dashStyleNames2Text}
    />
  )
};

export const RenderHftOrderBook = ({orderBook}) => {
  const pretty = _.map(orderBook,v=> {
    return {"blockTime":v.blockTime,
            "token-id": v["params"]["id"],
            type:v.name.substring("marmalade.ledger.".length),
            amount:v["params"]["amount"].toString(),
            contents: v};});
  console.debug("renderHftOrderBook", {orderBook,pretty});
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["Block Time", "Token ID", "Type", "Amount", "Details"]}
    keyOrder={["blockTime", "token-id", "type", "amount", "contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={0}
          enableClipboard={false}
          displayDataTypes={false}
          displayObjectSize={false}
        />}
      }
    }
    keyFormatter={dashStyleNames2Text}
    />
  )
};

export const RenderHftQuotes = ({orderBook, quotes}) => {
  const pretty = _.map(quotes,v=> {
    return {"blockTime":v.blockTime,
            "token-id": v["params"]["token-id"],
            "sale-id": v["params"]["sale-id"],
            price:v["params"]["price"].toString(),
            contents: {
              sale: getSaleForQuote(orderBook,v),
              quote: v}
            };});
  console.debug("renderHftQuotes", {quotes,pretty});
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["Block Time", "Token ID", "Type", "price", "Details"]}
    keyOrder={["blockTime", "token-id", "sale-id", "price", "contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={0}
          enableClipboard={false}
          displayDataTypes={false}
          displayObjectSize={false}
        />}
      }
    }
    keyFormatter={dashStyleNames2Text}
    />
  )
};
