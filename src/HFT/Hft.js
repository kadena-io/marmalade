//basic React api imports
import React, {  } from "react";
//semantic ui for styling
import {
  Card, CardHeader, CardContent, Button} from '@material-ui/core';
import { ScrollableTabs } from "../ScrollableTabs.js";

import { hftAPI } from "../kadena-config.js";
import { HftConfig } from "./HftConfig.js";
import { syncEventsFromCWData } from "./HftEvents.js";
import { RenderHftLedger, RenderHftTokens } from "./HftState.js";
import { LedgerForms, TokenForms } from "./HftTransactions.js";
import { RenderUri, RenderManifest, RenderDatum, ManifestForms } from "./Manifest.js";

export const hftDrawerEntries = {
  primary:"Marmalade",
  subList:
    [{
      primary:"Config & State",
      subList:[{
        primary:"Config",
        to:{app:"hft", ui: "config"}
      },{
        primary:"Tokens",
        to:{app:"hft", ui: "tokens"}
      },{
        primary:"Ledger",
        to:{app:"hft", ui: "ledger"}
      },{
        primary:"Manifest",
        to:{app:"hft", ui: "manifest"}
      }]
    }
  ]
};

export const HftApp = ({
  appRoute,
  setAppRoute,
  hftLedger,
  hftTokens,
  mfCache,
  setMfCache,
  pactTxStatus,
  refresh}) => {

  const {getHftLedger, getHftTokens} = refresh;
  
  return (
    appRoute.ui === "config" ?
    <Card>
      <CardHeader title="Contract and UI Configuration"/>
      <CardContent>
      <Button onClick={() => { syncEventsFromCWData('marmalade.ledger', 50, 4) }}>Test Block Event Sync</Button>
        <HftConfig/>
      </CardContent>
    </Card>
  : appRoute.ui === "tokens" ?
    <Card>
      <CardHeader title="Tokens State"/>
      <CardContent>
        <RenderHftTokens hftTokens={hftTokens}/>
        <TokenForms pactTxStatus={pactTxStatus} tabIdx={"hftTabT"} hftTokens={hftTokens} mfCache={mfCache} refresh={refresh}/>
      </CardContent>
    </Card>
  : appRoute.ui === "ledger" ?
    <Card>
      <CardHeader title="Ledger State"/>
      <CardContent>
        <RenderHftLedger hftLedger={hftLedger}/>
        <LedgerForms pactTxStatus={pactTxStatus} tabIdx={"hftTabL"} hftTokens={hftTokens} hftLedger={hftLedger} mfCache={mfCache} refresh={refresh}/>
      </CardContent>
    </Card>
  : appRoute.ui === "manifest" ?
    <>
    <Card>
      <CardHeader title="URI Local Cache"/>
      <CardContent>
        <RenderUri mfCache={mfCache} />
      </CardContent>
    </Card>
    <Card>
      <CardHeader title="Datum Local Cache"/>
      <CardContent>
        <RenderDatum mfCache={mfCache} />
      </CardContent>
    </Card>
    <Card>
      <CardHeader title="Manifest Local Cache"/>
      <CardContent>
        <RenderManifest mfCache={mfCache} />
      </CardContent>
    </Card>
    <Card>
      <CardContent>
        <ManifestForms mfCache={mfCache} setMfCache={setMfCache} tabIdx={"mfCache"}/>
      </CardContent>
    </Card>
    </>
  : <React.Fragment>
      {setAppRoute({app:"wallet", ui:"config"})}
  </React.Fragment>
  )
};
