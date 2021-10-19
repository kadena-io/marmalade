//basic React api imports
import React from "react";
//config file for blockchain calls
import Pact from "pact-lang-api";
import { hftAPI } from "../kadena-config.js";
import { PactJsonListAsTable, dashStyleNames2Text } from "../util.js";

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
  console.debug("renderHFTLedger", hftLedger);
  return (
   <PactJsonListAsTable
    json={hftLedger}
    header={["Account", "Token", "Balance", "Guard"]}
    keyOrder={["account", "token","balance","guard"]}
    keyFormatter={dashStyleNames2Text}
    />
  )
};

export const RenderHftTokens = ({hftTokens}) => {
  return (
   <PactJsonListAsTable
    json={hftTokens}
    header={["Token", "URI", "Precision", "Guard"]}
    keyOrder={["token","uri","minimum-precision","guard"]}
    keyFormatter={dashStyleNames2Text}
    />
  )
};
