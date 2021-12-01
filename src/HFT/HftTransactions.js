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
import { hftAPI, gtpAPI, fqpAPI } from "../kadena-config.js";
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

export const sendHftCommand = async (
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
          gasLimit: hftAPI.meta.gasLimit,
          chainId: hftAPI.meta.chainId,
          ttl: hftAPI.meta.ttl,
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
        const txReqKeys = await Pact.wallet.sendSigned(signed, hftAPI.meta.host)
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
          res = await Pact.fetch.poll(txReqKeys, hftAPI.meta.host);
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

const CreateGuardPolicyToken = (props) => {
  const {refresh, mfCache} = props;
  const {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes} = props.pactTxStatus;
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [id,setId] = useState("");
  const [manifest,setManifest] = useState("");
  const [precision,setPrecision] = useState(12);
  const [mintGrd,setMintGrd] = useState("");
  const [burnGrd,setBurnGrd] = useState("");
  const [saleGrd,setSaleGrd] = useState("");
  const [transferGrd,setTransferGrd] = useState("");
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${gtpAPI.contractAddress}.init-guards "${id}" (read-keyset 'mint) (read-keyset 'burn) (read-keyset 'sale) (read-keyset 'transfer))
          (${hftAPI.contractAddress}.create-token "${id}" ${precision} (read-msg 'manifest) ${gtpAPI.contractAddress})`
          ,{manifest: JSON.parse(manifest),
            mint: JSON.parse(mintGrd),
            burn: JSON.parse(burnGrd),
            sale: JSON.parse(saleGrd),
            transfer: JSON.parse(transferGrd),
          }
          );
      } catch (e) {
        console.log("create-token Submit Error",typeof e, e, {id, manifest: JSON.parse(manifest), precision,mintGrd,saleGrd,burnGrd,transferGrd});
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  const inputFields = [
    {
      type:'textFieldSingle',
      label:'Token Name',
      className:classes.formControl,
      value:id,
      onChange:setId
    },
    {
      type:'textFieldSingle',
      label:'Precision',
      className:classes.formControl,
      value:precision,
      onChange:setPrecision
    },
    {
      type:'select',
      label:'Manifest',
      className:classes.formControl,
      options:_.map(_.filter(mfCache,{type:'manifest'}),v=> JSON.stringify(v.value)),
      value:manifest,
      onChange:setManifest
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

const CreateFixedQuotePolicyToken = (props) => {
  const {refresh, mfCache} = props;
  const {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes} = props.pactTxStatus;
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [id,setId] = useState("");
  const [manifest,setManifest] = useState("");
  const [precision,setPrecision] = useState(12);
  const [mintGrd,setMintGrd] = useState("");
  const [maxSupply,setMaxSupply] = useState("1.0");
  const [minAmount,setMinAmount] = useState("0.0");
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${fqpAPI.contractAddress}.init-fqp "${id}" (read-keyset 'mint) (read-decimal 'maxSupply) (read-decimal 'minAmount))
          (${hftAPI.contractAddress}.create-token "${id}" ${precision} (read-msg 'manifest) ${fqpAPI.contractAddress})`
          ,{manifest: JSON.parse(manifest),
            mint: JSON.parse(mintGrd),
            maxSupply: Number.parseFloat(maxSupply),
            minAmount: Number.parseFloat(minAmount),
          }
          );
      } catch (e) {
        console.log("create-token Submit Error",typeof e, e, {id, manifest: JSON.parse(manifest), precision,mintGrd,maxSupply,minAmount});
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  const inputFields = [
    {
      type:'textFieldSingle',
      label:'Token Name',
      className:classes.formControl,
      value:id,
      onChange:setId
    },{
      type:'textFieldSingle',
      label:'Precision',
      className:classes.formControl,
      value:precision,
      onChange:setPrecision
    },{
      type:'select',
      label:'Manifest',
      className:classes.formControl,
      options:_.map(_.filter(mfCache,{type:'manifest'}),v=> JSON.stringify(v.value)),
      value:manifest,
      onChange:setManifest
    },{
      type:'textFieldMulti',
      label:'Mint Keyset',
      className:classes.formControl,
      placeholder:"",
      value:mintGrd,
      onChange:setMintGrd,
    },{
      type:'textFieldSingle',
      label:'Max Supply',
      className:classes.formControl,
      value:maxSupply,
      onChange:setMaxSupply
    },{
      type:'textFieldSingle',
      label:'Min Amount',
      className:classes.formControl,
      value:minAmount,
      onChange:setMinAmount
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

const Mint = ({hftTokens, refresh, 
  pactTxStatus: {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes}}) => {
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [token,setToken] = useState("");
  const [account,setAccount] = useState("");
  const [newKs,setNewKs] = useState({});
  const [amount,setAmount] = useState(100.0);
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      console.log(Pact.lang.mkCap("MINT Cap"
              , "Authenticates that you can mint"
              , `${hftAPI.contractAddress}.MINT`
              , [token, account, Number.parseInt(amount)]));
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${hftAPI.contractAddress}.mint "${token}" "${account}" (read-keyset 'ks) (read-decimal 'amount))`
          ,{ks: JSON.parse(newKs),
            amount}
          , [Pact.lang.mkCap("MINT Cap"
              , "Authenticates that you can mint"
              , `${hftAPI.contractAddress}.MINT`
              , [token, account, Number.parseFloat(amount)])
            ]
        );
      } catch (e) {
        console.log("mint Submit Error",typeof e, e, token, account, newKs, amount);
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  const inputFields = [
    {
      type:'select',
      label:'Select Token',
      className:classes.formControl,
      onChange:setToken,
      options:hftTokens.map((g)=>g['id']),
    },
    {
      type:'textFieldSingle',
      label:'Account Name',
      className:classes.formControl,
      value:account,
      onChange:setAccount
    },
    {
      type:'textFieldMulti',
      label:'Keyset',
      className:classes.formControl,
      placeholder:JSON.stringify({"pred":"keys-all","keys":["8c59a322800b3650f9fc5b6742aa845bc1c35c2625dabfe5a9e9a4cada32c543"]},undefined,2),
      value:newKs,
      onChange:setNewKs,
    },
    {
      type:'textFieldSingle',
      label:'Amount',
      className:classes.formControl,
      value:amount,
      onChange:setAmount
    },
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={tx} txStatus={txStatus} txRes={txRes}
      setTxStatus={setTxStatus}/>
  );
};



const TransferCreate = (props) => {
  const {hftTokens, hftLedger, refresh} = props;
  const {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes} = props.pactTxStatus;
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [token,setToken] = useState("");
  const [sender,setSender] = useState("");
  const [receiver,setReceiver] = useState("");
  const [newKs,setNewKs] = useState({});
  const [amount,setAmount] = useState(0.0);
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${hftAPI.contractAddress}.transfer-create "${token}" "${sender}" "${receiver}" (read-keyset 'ks) (read-decimal 'amount))`
          ,{ks: JSON.parse(newKs), amount: amount}
          , [Pact.lang.mkCap("Transfer Cap"
              , "Authenticates that you can transfer"
              , `${hftAPI.contractAddress}.TRANSFER`
              , [token, sender, receiver, Number.parseFloat(amount)])
            ]
        );
      } catch (e) {
        console.log("transfer-create Submit Error",typeof e, e, token, sender, receiver, newKs, amount);
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  
  const inputFields = [
    {
      type:'select',
      label:'Select Token',
      className:classes.formControl,
      onChange:setToken,
      options:hftTokens.map((g)=>g['id']),
    },
    {
      type:'select',
      label:'Sender Account',
      className:classes.formControl,
      onChange:setSender,
      options:_.sortedUniq(hftLedger.map((g)=>g['account']))
    },
    {
      type:'textFieldSingle',
      label:'Receiver Account',
      className:classes.formControl,
      value:receiver,
      onChange:setReceiver
    },
    {
      type:'textFieldMulti',
      label:'Receiver Account Keyset',
      className:classes.formControl,
      placeholder:JSON.stringify({"pred":"keys-all","keys":["8c59a322800b3650f9fc5b6742aa845bc1c35c2625dabfe5a9e9a4cada32c543"]},undefined,2),
      value:newKs,
      onChange:setNewKs,
    },
    {
      type:'textFieldSingle',
      label:'Transfer Amount',
      className:classes.formControl,
      value:amount,
      onChange:setAmount
    },
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={tx} txStatus={txStatus} txRes={txRes}
      setTxStatus={setTxStatus}/>
  );
};


const Transfer = (props) => {
  const {hftTokens, hftLedger, refresh} = props;
  const {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes} = props.pactTxStatus;
  const {current: {signingKey, networkId, gasPrice}} = usePactWallet();
  const [token,setToken] = useState("");
  const [sender,setSender] = useState("");
  const [receiver,setReceiver] = useState("");
  const [amount,setAmount] = useState(0.0);
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${hftAPI.contractAddress}.transfer "${token}" "${sender}" "${receiver}" (read-decimal 'amount))`
          ,{amount: amount}
          , [Pact.lang.mkCap("Transfer Cap"
              , "Authenticates that you can transfer"
              , `${hftAPI.contractAddress}.TRANSFER`
              , [token, sender, receiver, Number.parseFloat(amount)])
            ]
        );
      } catch (e) {
        console.log("transfer Submit Error",typeof e, e, token, sender, receiver, amount);
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  
  const inputFields = [
    {
      type:'select',
      label:'Select Token',
      className:classes.formControl,
      onChange:setToken,
      options:hftTokens.map((g)=>g['id']),
    },
    {
      type:'select',
      label:'Sender Account',
      className:classes.formControl,
      onChange:setSender,
      options:_.sortedUniq(hftLedger.map((g)=>g['account']))
    },
    {
      type:'select',
      label:'Receiver Account',
      className:classes.formControl,
      onChange:setReceiver,
      options:_.sortedUniq(hftLedger.map((g)=>g['account']))
    },
    {
      type:'textFieldSingle',
      label:'Transfer Amount',
      className:classes.formControl,
      value:amount,
      onChange:setAmount
    },
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={tx} txStatus={txStatus} txRes={txRes}
      setTxStatus={setTxStatus}/>
  );
};


const CreateAccount = (props) => {
  const {hftTokens, hftLedger, refresh} = props;
  const {txStatus, setTxStatus,
    tx, setTx,
    txRes, setTxRes} = props.pactTxStatus;
  const {current: {signingKey, networkId, gasPrice}, allKeys} = usePactWallet();
  const [token,setToken] = useState("");
  const [account,setAccount] = useState("");
  const [grdKeys,setGrdKeys] = useState([]);
  const [grdPred,setGrdPred] = useState("keys-all");
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      const newKeys = _.map(grdKeys, (k) => k.inputValue ? k.inputValue : k);
      console.debug("create-account", token, account, grdPred, grdKeys, {ks:{pred:grdPred, keys:newKeys}});
      try {
        sendHftCommand(setTx,setTxStatus,setTxRes,refresh
          ,signingKey, networkId, Number.parseFloat(gasPrice)
          ,`(${hftAPI.contractAddress}.create-account "${token}" "${account}" (read-keyset 'ks))`
          ,{ks:{pred:grdPred, keys:newKeys}}
        );
      } catch (e) {
        console.log("create-account Submit Error",typeof e, e, token, account, grdPred, grdKeys);
        setTxRes(e);
        setTxStatus("validation-error");
      }
      };
  
  const inputFields = [
    {
      type:'select',
      label:'Select Token',
      className:classes.formControl,
      onChange:setToken,
      options:hftTokens.map((g)=>g['id']),
    },
    {
      type:'textFieldSingle',
      label:'Account Name',
      className:classes.formControl,
      value:account,
      onChange:setAccount
    },
    {
      type:'select',
      label:'Select Keyset Predicate',
      className:classes.formControl,
      onChange:setGrdPred,
      options:["keys-any", "keys-all", "keys-2"],
    },
    {
      type:'keySelector',
      label:'Set Guard Keys',
      className:classes.formControl,
      onChange:setGrdKeys,
      value:grdKeys,
      options:allKeys,
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


export const LedgerForms = ({
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
            label:"Transfer Create",
            component:
              <TransferCreate
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={()=>getHftLedger()}/>
          },{
            label:"Transfer",
            component:
              <Transfer
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={()=>getHftLedger()}/>
          },{
            label:"Create Account",
            component:
              <CreateAccount
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={()=>getHftLedger()}/>
          }
      ]}/>
  );
};

export const TokenForms = ({
  hftTokens,
  mfCache,
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
            label:"Create Guard Policy HFT",
            component:
              <CreateGuardPolicyToken
                pactTxStatus={pactTxStatus}
                mfCache={mfCache}
                refresh={()=>getHftTokens()}/>
          },{
            label:"Create Fixed Quote Policy HFT",
            component:
              <CreateFixedQuotePolicyToken
                pactTxStatus={pactTxStatus}
                mfCache={mfCache}
                refresh={()=>getHftTokens()}/>
          },{
            label:"Mint HFT Token",
            component:
              <Mint
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                refresh={()=>getHftLedger()}/>
          }
      ]}/>
  );
};
