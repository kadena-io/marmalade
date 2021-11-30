//basic React api imports
import React, { useState, useEffect } from "react";
import { 
  useQueryParams,
  StringParam,
 } from 'use-query-params';
//make JS less terrible
import _ from "lodash";
//Material Stuff
import {
  makeStyles,
} from '@material-ui/styles';
//pact-lang-api for blockchain calls
import Pact from "pact-lang-api";
//config file for blockchain calls
import { gtpAPI } from "../kadena-config.js";
import {
  PactJsonListAsTable,
  MakeForm,
 } from "../util.js";
import { ScrollableTabs } from "../ScrollableTabs.js";
import { usePactWallet, addGasCap } from "../PactWallet.js";

const useStyles = makeStyles(() => ({
  formControl: {
    margin: "5px auto",
    minWidth: 120,
  },
  selectEmpty: {
    marginTop: "10px auto",
  },
}));

export const sendGtpCommand = async (
  setTx,
  setTxStatus,
  setTxRes,
  refresh,
  signingKey,
  networkId,
  gasPrice,
  cmd, envData={}, caps=[]
) => {
    try {
      //creates transaction to send to wallet
      const toSign = {
          pactCode: cmd,
          caps: addGasCap(caps),
          signingPubKey: signingKey,
          networkId: networkId,
          gasPrice: gasPrice,
          gasLimit: gtpAPI.meta.gasLimit,
          chainId: gtpAPI.meta.chainId,
          ttl: gtpAPI.meta.ttl,
          sender: signingKey,
          envData: envData
      }
      console.log("toSign", toSign)
      //sends transaction to wallet to sign and awaits signed transaction
      const signed = await Pact.wallet.sign(toSign)
      console.log("signed", signed)
      if ( typeof signed === 'object' && 'hash' in signed ) {
        setTx(signed);
      } else {
        throw new Error("Signing API Failed");
      }

      try {
        //sends signed transaction to blockchain
        const txReqKeys = await Pact.wallet.sendSigned(signed, gtpAPI.meta.host)
        console.log("txReqKeys", txReqKeys)
        //set html to wait for transaction response
        //set state to wait for transaction response
        setTxStatus('pending')
        //listens to response to transaction sent
        //  note method will timeout in two minutes
        //    for lower level implementations checkout out Pact.fetch.poll() in pact-lang-api
        let retries = 8;
        let res = {};
        while (retries > 0) {
          //sleep the polling
          await new Promise(r => setTimeout(r, 15000));
          res = await Pact.fetch.poll(txReqKeys, gtpAPI.meta.host);
          try {
            if (res[signed.hash].result.status) {
              retries = -1;
            } else {
              retries = retries - 1;
            }
          } catch(e) {
              retries = retries - 1;
          }
        };
        //keep transaction response in local state
        setTxRes(res)
        if (res[signed.hash].result.status === "success"){
          console.log("tx status set to success");
          //set state for transaction success
          setTxStatus('success');
          refresh();
        } else if (retries === 0) {
          console.log("tx status set to timeout");
          setTxStatus('timeout');
          refresh();
        } else {
          console.log("tx status set to failure");
          //set state for transaction failure
          setTxStatus('failure');
        }
      } catch(e) {
        // TODO: use break in the while loop to capture if timeout occured
        console.log("tx api failure",e);
        setTxRes(e);
        setTxStatus('failure');
      }
    } catch(e) {
      console.log("tx status set to validation error",e);
      //set state for transaction construction error
      setTxStatus('validation-error');
    }
};

const InitGuards = ({
  refresh,
  pactTxStatus:{txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes}
  }) => {
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [tokenId,setTokenId] = useState("");
  const [mintGrd,setMintGrd] = useState("");
  const [burnGrd,setBurnGrd] = useState("");
  const [saleGrd,setSaleGrd] = useState("");
  const [transferGrd,setTransferGrd] = useState("");
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        sendGtpCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${gtpAPI.contractAddress}.init-guards "${tokenId}" (read-keyset 'mint) (read-keyset 'burn) (read-keyset 'sale) (read-keyset 'transfer))`
          ,{
            mint: JSON.parse(mintGrd),
            burn: JSON.parse(burnGrd),
            sale: JSON.parse(saleGrd),
            transfer: JSON.parse(transferGrd),
          }
          );
      } catch (e) {
        console.log("create-token Submit Error",typeof e, e, {tokenId,mintGrd,saleGrd,burnGrd,transferGrd});
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  const inputFields = [
    {
      type:'textFieldSingle',
      label:'Token Name',
      className:classes.formControl,
      value:tokenId,
      onChange:setTokenId
    },{
      type:'textFieldMulti',
      label:'Mint Keyset',
      className:classes.formControl,
      placeholder:"",
      value:mintGrd,
      onChange:setMintGrd,
    },{
      type:'textFieldMulti',
      label:'Burn Keyset',
      className:classes.formControl,
      placeholder:"",
      value:burnGrd,
      onChange:setBurnGrd,
    },{
      type:'textFieldMulti',
      label:'Sale Keyset',
      className:classes.formControl,
      placeholder:"",
      value:saleGrd,
      onChange:setSaleGrd,
    },{
      type:'textFieldMulti',
      label:'Transfer Keyset',
      className:classes.formControl,
      placeholder:"",
      value:transferGrd,
      onChange:setTransferGrd,
    }
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={tx} txStatus={txStatus} txRes={txRes}
      setTxStatus={setTxStatus}/>
  );
};

export const GtpForms = ({
  hftLedger,
  hftTokens,
  tabIdx,
  pactTxStatus,
  refresh: {
    getHftLedger,
    getHftTokens,
  },
}) => {
  return (
    <ScrollableTabs
      tabIdx={tabIdx}
      tabEntries={[
          {
            label:"Init Guards",
            component:
              <InitGuards
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={()=>null}/>
          }
      ]}/>
  );
};
