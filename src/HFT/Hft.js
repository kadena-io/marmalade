//basic React api imports
import React, {  } from "react";
//semantic ui for styling
import {
  Card, CardHeader, CardContent} from '@material-ui/core';
import { ScrollableTabs } from "../ScrollableTabs.js";

import { hftAPI } from "../kadena-config.js";
import { HftConfig } from "./HftConfig.js"
import { RenderHftLedger, RenderHftTokens } from "./HftState.js";
import { LedgerForms, TokenForms } from "./HftTransactions.js";

export const hftDrawerEntries = {
  primary:`${hftAPI.namespace}.${hftAPI.contractName}`,
  subList:
    [{
      primary:"Config & State",
      subList:[{
        primary:"Config",
        to:{app:"hft", ui: "config"}
      },{
        primary:"HFT Tokens",
        to:{app:"hft", ui: "tokens"}
      },{
        primary:"HFT Ledger",
        to:{app:"hft", ui: "ledger"}
      }]
    }
  ]
};

export const HftApp = ({
  appRoute, 
  setAppRoute,
  hftLedger,
  hftTokens,
  pactTxStatus,
  refresh}) => {

  const {getHftLedger, getHftTokens} = refresh;
  return (
    appRoute.ui === "config" ?
    <Card>
      <CardHeader title="Contract and UI Configuration"/>
      <CardContent>
        <HftConfig/>
      </CardContent>
    </Card>
  : appRoute.ui === "tokens" ?
    <Card>
      <CardHeader title="Tokens State"/>
      <CardContent>
        <RenderHftTokens hftTokens={hftTokens}/>
        <TokenForms pactTxStatus={pactTxStatus} tabIdx={"hftTabT"} hftTokens={hftTokens} refresh={refresh}/>
      </CardContent>
    </Card>
  : appRoute.ui === "ledger" ?
    <Card>
      <CardHeader title="Ledger State"/>
      <CardContent>
        <RenderHftLedger hftLedger={hftLedger}/>
        <LedgerForms pactTxStatus={pactTxStatus} tabIdx={"hftTabL"} hftTokens={hftTokens} hftLedger={hftLedger} refresh={refresh}/>
      </CardContent>
    </Card>
  : <React.Fragment>
      {setAppRoute({app:"wallet", ui:"config"})}
  </React.Fragment>
  )
};