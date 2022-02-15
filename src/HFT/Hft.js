//basic React api imports
import React, { useEffect, useState } from "react";
//semantic ui for styling
import {
  Card, CardHeader, CardContent, Button} from '@material-ui/core';
import RefreshIcon from '@material-ui/icons/Refresh';
import { ScrollableTabs } from "../ScrollableTabs.js";

import { hftAPI } from "../kadena-config.js";
import { HftConfig } from "./HftConfig.js";
import { syncEventsFromCWData } from "./HftEvents.js";
import { RenderHftLedger, RenderHftTokens, RenderHftOrderBook } from "./HftState.js";
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
        primary:"Order Book",
        to:{app:"hft", ui: "orderBook"}
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
  hftEvents,
  orderBook,
  mfCache,
  setMfCache,
  pactTxStatus,
  refresh}) => {
  const [obRefreshing,setObRefreshing] = useState(false);

  useEffect(()=>setObRefreshing(false), [orderBook]);
  console.log(hftEvents)
  return (
    appRoute.ui === "config" ?
    <Card>
      <CardHeader title="Contract and UI Configuration"/>
      <CardContent>
        <HftConfig/>
      </CardContent>
    </Card>
  : appRoute.ui === "orderBook" ?
    <Card>
      <CardHeader title="Order Book"/>
      <CardContent>
        <Button
          variant="contained"
          color="secondary"
          disabled={obRefreshing}
          onClick={() => {
            setObRefreshing(true);
            refresh.getHftEvents();
          } }
          startIcon={<RefreshIcon />}
        >
          Refresh Order Book
        </Button>
        <RenderHftOrderBook orderBook={orderBook}/>
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
