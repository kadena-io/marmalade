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
import { hftAPI, gtpAPI, fqpAPI, fqrpAPI } from "../kadena-config.js";
import {
  PactJsonListAsTable,
  MakeForm,
 } from "../util.js";
import { ScrollableTabs } from "../ScrollableTabs.js";
import { usePactWallet } from "../PactWallet.js";
import { SigData } from '../Pact.SigBuilder.js';
import { signNewPactTx } from '../PactTxStatus.js';
import { getSaleForQuote } from "./HftEvents.js";

const useStyles = makeStyles(() => ({
  formControl: {
    margin: "5px auto",
    minWidth: 120,
  },
  selectEmpty: {
    marginTop: "10px auto",
  },
}));

export const signExecHftCommand = (
  sender,
  signingKey,
  pactTxStatus,
  networkId,
  gasPrice,
  gasLimit,
  cmd, envData={}, caps=[]
) => {
  const meta = Pact.lang.mkMeta(sender, hftAPI.meta.chainId, Number.parseFloat(gasPrice), Number.parseFloat(gasLimit), SigData.util.autoCreationTime(), hftAPI.meta.ttl);
  const capsWithGas = SigData.util.addGasCap(caps);
  console.log("signExecHftCommand", capsWithGas);
  const signers = SigData.mkSignerCList(signingKey, capsWithGas);
  const cmdJSON = SigData.mkExecPayload(
    cmd,
    signers,
    networkId,
    meta,
    {data: envData}
  );
  const execSigData = SigData.mkSigData(cmdJSON);
  signNewPactTx(execSigData, pactTxStatus);
};

export const signContHftCommand = (
  pactId,
  step,
  rollback,
  sender,
  signingKey,
  pactTxStatus,
  networkId,
  gasPrice,
  gasLimit,
  envData={}, caps=[]
) => {
  const meta = Pact.lang.mkMeta(sender, hftAPI.meta.chainId, Number.parseFloat(gasPrice), Number.parseFloat(gasLimit), SigData.util.autoCreationTime(), hftAPI.meta.ttl);
  const capsWithGas = SigData.util.addGasCap(caps);
  console.log("signContHftCommand", capsWithGas);
  const signers = SigData.mkSignerCList(signingKey, capsWithGas);
  const cmdJSON = SigData.mkContPayload(
    pactId,
    step,
    signers,
    networkId,
    meta,
    { data: envData,
      rollback: rollback}
  );
  const execSigData = SigData.mkSigData(cmdJSON);
  signNewPactTx(execSigData, pactTxStatus);
};

const CreateGuardPolicyToken = ({
  refresh,
  mfCache,
  pactTxStatus
}) => {
  const {setTxRes, setTxStatus} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
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
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.create-token "${id}" ${precision} (read-msg 'manifest) ${gtpAPI.contractAddress})`,
          {"manifest": JSON.parse(manifest),
            "mint-guard": JSON.parse(mintGrd),
            "burn-guard": JSON.parse(burnGrd),
            "sale-guard": JSON.parse(saleGrd),
            "transfer-guard": JSON.parse(transferGrd)}
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
      />
  );
};

const CreateFixedQuotePolicyToken = ({
  refresh,
  mfCache,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
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
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.create-token "${id}" ${precision} (read-msg 'manifest) ${fqpAPI.contractAddress})`,
          {"manifest": JSON.parse(manifest),
            "mint-guard": JSON.parse(mintGrd),
            "max-supply": Number.parseFloat(maxSupply),
            "min-amount": Number.parseFloat(minAmount),
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};

const CreateFixedQuoteRoyaltyPolicyToken = ({
  refresh,
  mfCache,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
  const [id,setId] = useState("");
  const [manifest,setManifest] = useState("");
  const [precision,setPrecision] = useState(12);
  const [fungible, setFungible] = useState("");
  const [mintGrd,setMintGrd] = useState("");
  const [creator, setCreator] = useState("");
  const [creatorGrd,setCreatorGrd] = useState("");
  const [maxSupply,setMaxSupply] = useState("1.0");
  const [minAmount,setMinAmount] = useState("0.0");
  const [royaltyRate,setRoyaltyRate] = useState("0.0");
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.create-token "${id}" ${precision} (read-msg 'manifest) ${fqrpAPI.contractAddress})`,
          {"manifest": JSON.parse(manifest),
            "token_spec": {
              "fungible": {
                "refName": {
                  "namespace":null,
                  "name":fungible
                },
                "refSpec": [
                  {
                  "namespace":null,
                  "name":"fungible-v2"
                }]
              },
              "creator": creator,
              "creator-guard": JSON.parse(creatorGrd),
              "mint-guard": JSON.parse(mintGrd),
              "max-supply": Number.parseFloat(maxSupply),
              "min-amount": Number.parseFloat(minAmount),
              "royalty-rate": Number.parseFloat(royaltyRate)
          }}
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
    },
    {
      type:'textFieldSingle',
      label:'Creator',
      className:classes.formControl,
      value:creator,
      onChange:setCreator
    },
    {
      type:'textFieldMulti',
      label:'Creator Keyset',
      className:classes.formControl,
      placeholder:"",
      value:creatorGrd,
      onChange:setCreatorGrd,
    },
    {
      type:'textFieldSingle',
      label:'Fungible',
      className:classes.formControl,
      value:fungible,
      onChange:setFungible
    },
    {
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
    },{
      type:'textFieldSingle',
      label:'Royalty Rate',
      className:classes.formControl,
      value:royaltyRate,
      onChange:setRoyaltyRate
    }
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};

const Mint = ({
  hftTokens,
  refresh,
  pactTxStatus
  }) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
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
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.mint "${token}" "${account}" (read-keyset 'ks) (read-decimal 'amount))`,
          {ks: JSON.parse(newKs), amount},
          [SigData.mkCap(`${hftAPI.contractAddress}.MINT`,[token, account, Number.parseFloat(amount)])]
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};



const TransferCreate = ({
  hftTokens,
  hftLedger,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
  const [token,setToken] = useState("");
  const [sender,setSender] = useState("");
  const [receiver,setReceiver] = useState("");
  const [newKs,setNewKs] = useState({});
  const [amount,setAmount] = useState(0.0);
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.transfer-create "${token}" "${sender}" "${receiver}" (read-keyset 'ks) (read-decimal 'amount))`,
          {ks: JSON.parse(newKs), amount: amount},
          [SigData.mkCap(`${hftAPI.contractAddress}.TRANSFER`, [token, sender, receiver, Number.parseFloat(amount)])]
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};


const Transfer = ({
  hftTokens,
  hftLedger,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
  const [token,setToken] = useState("");
  const [sender,setSender] = useState("");
  const [receiver,setReceiver] = useState("");
  const [amount,setAmount] = useState(0.0);
  const classes = useStyles();

  const handleSubmit = (evt) => {
      evt.preventDefault();
      try {
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.transfer "${token}" "${sender}" "${receiver}" (read-decimal 'amount))`,
          {amount: amount},
          [SigData.mkCap(`${hftAPI.contractAddress}.TRANSFER`, [token, sender, receiver, Number.parseFloat(amount)])]
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};


const CreateAccount = ({
  hftTokens,
  hftLedger,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}, allKeys} = usePactWallet();
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
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.create-account "${token}" "${account}" (read-keyset 'ks))`,
          {ks:{pred:grdPred, keys:newKeys}}
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
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};

const filterTokensByPolicy = (hftTokens, {namespace, names}) => {
  return _.filter(hftTokens,
    ({policy}) => names.includes(policy.refName.name) && namespace === policy.refName.namespace)
};

const getPossibleAccounts = (hftLedger, tokenId) => {
  return _.filter(hftLedger, ({guard, id}) => {
    if (guard.pred && guard.keys && id === tokenId) {return true} else {return false}
  });
};

const SaleFixedQuotePolicy = ({
  hftTokens,
  hftLedger,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}, allKeys} = usePactWallet();
  const [token,setToken] = useState("");
  const [seller,setSeller] = useState("");
  const [possibleSellers,setPossibleSellers] = useState([]);
  const [amount,setAmount] = useState("0.0");
  const [timeLimit,setTimeLimit] = useState("");
  const [fungible, setFungible] = useState("coin")
  const [price, setPrice] = useState("0.0");
  // recipient should be the seller
  const [recipient, setRecipient] = useState("");
  const [recipientGrd, setRecipientGrd] = useState("");
  const [possibleTokens, setPossibleTokens] = useState([]);
  const classes = useStyles();

  useEffect(()=>{
    setPossibleTokens(hftTokens);
    // setPossibleTokens(filterTokensByPolicy(hftTokens, {names: ["fixed-quote-royalty-policy","fixed-quote-policy"], namespace: "marmalade"}));
  },[hftTokens]);

  useEffect(()=>{
    setPossibleSellers(hftLedger.map(({account})=>account));
    // setPossibleSellers(_.filter(getPossibleAccounts(hftLedger,token),({balance})=> {
    //   return balance > 0;
    // }).map(({account})=>account));
  },[token,hftLedger,seller]);

  const handleSubmit = (evt) => {
      evt.preventDefault();
      console.debug("sale", token, seller, fungible);
      try {

        const quote = {
          "price": Number.parseFloat(price),
          "recipient": recipient,
          "recipient-guard": JSON.parse(recipientGrd)
        };
        if (_.find(possibleTokens, {id: token})["policy"]["refName"]["name"] === "fixed-quote-policy") {
          quote["fungible"] = {
            "refName": {
              "namespace":null,
              "name":fungible
            },
            "refSpec": [
              {
              "namespace":null,
              "name":"fungible-v2"
            }]
          };
        }
        signExecHftCommand(accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          `(${hftAPI.contractAddress}.sale "${token}" "${seller}" (read-decimal 'amount) (read-integer 'timeout))`,
          { amount: amount,
            timeout: timeLimit,
            quote
          },
          [SigData.mkCap(`${hftAPI.contractAddress}.OFFER`,[token, seller, Number.parseFloat(amount), {int: timeLimit}])]
        );
      } catch (e) {
        console.log("Sale Submit Error",typeof e, e, token, seller, amount, timeLimit);
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
      options:possibleTokens.map((g)=>g['id']),
    },
    {
      type:'select',
      label:'Select Seller',
      className:classes.formControl,
      onChange:setSeller,
      options:possibleSellers
    },
    {
      type:'textFieldSingle',
      label:'Sale Amount',
      className:classes.formControl,
      value:amount,
      onChange:setAmount
    },
    {
      type:'textFieldSingle',
      label:'Timeout (Block height)',
      className:classes.formControl,
      value:timeLimit,
      onChange:setTimeLimit
    },
    {
      type:'textFieldSingle',
      label:'fungible',
      className:classes.formControl,
      value:fungible,
      onChange:setFungible
    },
    {
      type:'textFieldSingle',
      label:'Price',
      className:classes.formControl,
      value:price,
      onChange:setPrice
    },
    {
      type:'textFieldSingle',
      label:'Recipient',
      className:classes.formControl,
      onChange:setRecipient,
      value:recipient
    },
    {
      type:'textFieldMulti',
      label:'Recipient Keyset',
      className:classes.formControl,
      placeholder:JSON.stringify({"pred":"keys-all","keys":["8c59a322800b3650f9fc5b6742aa845bc1c35c2625dabfe5a9e9a4cada32c543"]},undefined,2),
      value:recipientGrd,
      onChange:setRecipientGrd,
    }
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};

const BuyFixedQuotePolicy = ({
  hftLedger,
  hftTokens,
  orderBook,
  quotes,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}} = usePactWallet();
  const [saleId,setSaleId] = useState("");
  const [buyer,setBuyer] = useState("");
  const [buyerGrd,setBuyerGrd] = useState("");
  const classes = useStyles();

  useEffect(()=>{
    try {
      const quote = _.find(quotes,{params: {'sale-id': saleId}});
      const tokenId = quote["params"]["token-id"];
    } catch (e) {
      console.debug("Getting applicable buyers failed with", e);
    }
  },[saleId, hftLedger, quotes]);

  const handleSubmit = (evt) => {
      evt.preventDefault();
      const quote = _.find(quotes,{params: {'sale-id': saleId}});
      const sale = getSaleForQuote(orderBook,saleId);
      const amount = sale.params.amount;
      const price = quote.params.spec.price;
      const seller = sale.params.seller;
      const recipient = quote.params.spec.recipient;
      const timeout = sale.params.timeout;
      const tokenId = quote["params"]["token-id"];
      try {
        signContHftCommand(saleId, 1, false, accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
          { buyer: buyer,
            "buyer-guard": JSON.parse(buyerGrd),
          },
          [
            SigData.mkCap(`${hftAPI.contractAddress}.BUY`,[tokenId, seller, buyer, amount, {"int": timeout}, saleId]),
            SigData.mkCap(`coin.TRANSFER`, [buyer, recipient, amount*price])
          ]
        );
      } catch (e) {
        console.log("Buy Submit Error",typeof e, e, {quote, sale, amount, price, transfer: (amount*price), seller, timeout});
        setTxRes(e);
        setTxStatus("validation-error");
      }
    };

  const inputFields = [
    {
      type:'select',
      label:'Select Sale',
      className:classes.formControl,
      onChange:setSaleId,
      options:quotes.map((g)=> g['params']['sale-id'])
    },
    {
      type:'textFieldSingle',
      label:'Buyer',
      className:classes.formControl,
      onChange:setBuyer,
      value:buyer
    },
    {
      type:'textFieldMulti',
      label:'Buyer Keyset',
      className:classes.formControl,
      placeholder:JSON.stringify({"pred":"keys-all","keys":["8c59a322800b3650f9fc5b6742aa845bc1c35c2625dabfe5a9e9a4cada32c543"]},undefined,2),
      value:buyerGrd,
      onChange:setBuyerGrd,
    }
    // {
    //   type:'textFieldMulti',
    //   label:'Buyer Keyset',
    //   className:classes.formControl,
    //   placeholder:JSON.stringify({"pred":"keys-all","keys":["8c59a322800b3650f9fc5b6742aa845bc1c35c2625dabfe5a9e9a4cada32c543"]},undefined,2),
    //   value:buyerGrd,
    //   onChange:setBuyerGrd,
    // }
  ];

  return (
    <MakeForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      pactTxStatus={pactTxStatus}
      refresh={refresh}
    />
  );
};

const WithdrawFixedQuotePolicy = ({
  hftLedger,
  hftTokens,
  orderBook,
  quotes,
  refresh,
  pactTxStatus
}) => {
  const {setTxStatus, setTxRes} = pactTxStatus;
  const {current: {signingKey, networkId, gasPrice, gasLimit, accountName}, allKeys} = usePactWallet();
  const [saleId,setSaleId] = useState("");
  const classes = useStyles();

    useEffect(()=>{
      try {
        const quote = _.find(quotes,{params: {'sale-id': saleId}});
        const tokenId = quote["params"]["token-id"];
      } catch (e) {
        console.debug("Getting applicable buyers failed with", e);
      }
    },[saleId, hftLedger, quotes]);

    const handleSubmit = (evt) => {
        evt.preventDefault();
        const quote = _.find(quotes,{params: {'sale-id': saleId}});
        const sale = getSaleForQuote(orderBook,saleId);
        const amount = sale.params.amount;
        const price = quote.params.spec.price;
        const seller = sale.params.seller;
        const recipient = quote.params.spec.recipient;
        const timeout = sale.params.timeout;
        const tokenId = quote["params"]["token-id"];
        try {
          signContHftCommand(saleId, 0, true, accountName, signingKey, pactTxStatus, networkId, gasPrice, gasLimit,
            []
          );
        } catch (e) {
          console.log("Withdraw Submit Error",typeof e, e, {quote, sale, amount, price, transfer: (amount*price), seller, timeout});
          setTxRes(e);
          setTxStatus("validation-error");
        }
      };

    const inputFields = [
      {
        type:'select',
        label:'Select Sale',
        className:classes.formControl,
        onChange:setSaleId,
        options:orderBook.map((g)=> g['params']['sale-id'])
      }
    ];

    return (
      <MakeForm
        inputFields={inputFields}
        onSubmit={handleSubmit}
        pactTxStatus={pactTxStatus}
        refresh={refresh}
      />
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
            label:"Create Fixed Quote Royalty Policy HFT",
            component:
              <CreateFixedQuoteRoyaltyPolicyToken
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

export const OrderForms = ({
  hftTokens,
  hftLedger,
  orderBook,
  quotes,
  mfCache,
  tabIdx,
  pactTxStatus,
  refresh: {
    getHftLedger,
    getHftTokens,
    getHftEvents,
  },
}) => {
  const refresh = () => {
    getHftEvents();
    getHftLedger();
  };
  return (
    <ScrollableTabs
      tabIdx={tabIdx}
      tabEntries={[
          {
            label:"Sale Fixed Quote Policy Token",
            component:
              <SaleFixedQuotePolicy
                pactTxStatus={pactTxStatus}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={refresh}/>
          },
          {
            label:"Buy Fixed Quote Policy Token",
            component:
              <BuyFixedQuotePolicy
                pactTxStatus={pactTxStatus}
                orderBook={orderBook.filter((g)=>g['name'] === "marmalade.ledger.SALE")}
                quotes={quotes}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={refresh}/>
          },
          {
            label:"Withdraw Fixed Quote Policy Token",
            component:
              <WithdrawFixedQuotePolicy
                pactTxStatus={pactTxStatus}
                orderBook={orderBook.filter((g)=>g['name'] === "marmalade.ledger.SALE")}
                quotes={quotes}
                hftTokens={hftTokens}
                hftLedger={hftLedger}
                refresh={refresh}/>
          },
      ]}/>
  );
};
