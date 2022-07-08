//basic React api imports
import React, { useState, useEffect } from "react";
import {
  useQueryParams,
  StringParam,
 } from 'use-query-params';
import _ from 'lodash';
import ReactJson from 'react-json-view'
//config file for blockchain calls
import Pact from "pact-lang-api";
import { manifestAPI } from "../kadena-config.js";
import { PactJsonListAsTable, dashStyleNames2Text, MakeLocalForm } from "../util.js";
import { ScrollableTabs } from "../ScrollableTabs.js";
import { usePactWallet, addGasCap } from "../PactWallet.js";
import {
  makeStyles,
} from '@material-ui/styles';

const useStyles = makeStyles(() => ({
  formControl: {
    margin: "5px auto",
    minWidth: 120,
  },
  selectEmpty: {
    marginTop: "10px auto",
  },
}));

export const createUri = async (scheme, data) => {
  //calling get-all() function from smart contract
    const res = await Pact.fetch.local(
      {
        pactCode: `(${manifestAPI.contractAddress}.uri (read-string 'scheme) (read-string 'data))`,
        //pact-lang-api function to construct transaction meta data
        envData: {scheme, data},
        meta: Pact.lang.mkMeta(
          manifestAPI.meta.sender,
          manifestAPI.meta.chainId,
          manifestAPI.meta.gasPrice,
          manifestAPI.meta.gasLimit,
          manifestAPI.meta.creationTime(),
          manifestAPI.meta.ttl
        ),
      },
      manifestAPI.meta.host
    );
    const all = res.result.data;
    //sorts memories by least recent
    console.debug(`local query data: (${manifestAPI.contractAddress}.uri)`, {scheme,data}, all);
    return({'type':'uri', 'value': all});
};


export const createDatum = async (uri, datum) => {
  //calling get-all() function from smart contract
    const res = await Pact.fetch.local(
      {
        pactCode: `(${manifestAPI.contractAddress}.create-datum (read-msg 'uri) (read-msg 'datum))`,
        //pact-lang-api function to construct transaction meta data
        envData: {uri, datum},
        meta: Pact.lang.mkMeta(
          manifestAPI.meta.sender,
          manifestAPI.meta.chainId,
          manifestAPI.meta.gasPrice,
          manifestAPI.meta.gasLimit,
          manifestAPI.meta.creationTime(),
          manifestAPI.meta.ttl
        ),
      },
      manifestAPI.meta.host
    );
    const all = res.result.data;
    //sorts memories by least recent
    console.debug(`local query data: (${manifestAPI.contractAddress}.create-datum)`, {uri,datum}, all);
    return({'type':'datum', 'value': all});
};

export const createManifest = async (uri, data) => {
  //calling get-all() function from smart contract
    const res = await Pact.fetch.local(
      {
        pactCode: `(${manifestAPI.contractAddress}.create-manifest (read-msg 'uri) (read-msg 'data))`,
        //pact-lang-api function to construct transaction meta data
        envData: {uri, data},
        meta: Pact.lang.mkMeta(
          manifestAPI.meta.sender,
          manifestAPI.meta.chainId,
          manifestAPI.meta.gasPrice,
          manifestAPI.meta.gasLimit,
          manifestAPI.meta.creationTime(),
          manifestAPI.meta.ttl
        ),
      },
      manifestAPI.meta.host
    );
    const all = res.result.data;
    //sorts memories by least recent
    console.debug(`local query data: (${manifestAPI.contractAddress}.create-manifest)`, {uri,data}, all);
    return({'type':'manifest', 'value': all});
};

export const RenderUri = ({mfCache}) => {
  console.debug("renderUri", mfCache);
  return (
   <PactJsonListAsTable
    json={_.map(_.filter(mfCache,{type:'uri'}),'value')}
    header={["Scheme", "Data"]}
    keyOrder={["scheme","data"]}
    keyFormatter={dashStyleNames2Text}
    />
  )
};

export const RenderManifest = ({mfCache}) => {
  const pretty = _.map(_.filter(mfCache,{type:'manifest'}),v=> {return {hash: v.value.hash, contents: v.value};});
  console.debug("renderManifest", pretty);
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["Hash", "Contents"]}
    keyOrder={["hash","contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={2}
          enableClipboard={false}
          displayDataTypes={false}
          displayObjectSize={false}
        />}
      }
    }
    />
  )
};

export const RenderDatum = ({mfCache}) => {
  const pretty = _.map(_.filter(mfCache,{type:'datum'}),v=> {return {hash: v.value.hash, contents: v.value};});
  console.debug("renderDatum", pretty);
  return (
   <PactJsonListAsTable
    json={pretty}
    header={["Hash", "Contents"]}
    keyOrder={["hash","contents"]}
    kvFunc={
      {'contents': v => {
        return <ReactJson
          src={v.contents}
          name={false}
          collapsed={2}
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

const CreateUri = ({mfCache, setMfCache}) => {
  const [scheme,setScheme] = useState("");
  const [data,setData] = useState("");
  const classes = useStyles();

  const handleSubmit = async (evt) => {
      evt.preventDefault();
      try {
        const res = await createUri(scheme, data);
        console.debug('create-uri result', res)
        setMfCache(_.uniq(_.concat([res],mfCache)));
        console.debug('updated mf', mfCache )
      } catch (e) {
        console.log("create-uri Submit Error",typeof e, e, scheme, data);
      }
      };
  const inputFields = [
    {
      type:'textFieldSingle',
      label:'Scheme',
      className:classes.formControl,
      value:scheme,
      onChange:setScheme
    },
    {
      type:'textFieldSingle',
      label:'Data',
      className:classes.formControl,
      value:data,
      onChange:setData
    }
  ];

  return (
    <MakeLocalForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={"stateless tx"} txStatus={"stateless tx"} txRes={"stateless tx"}
      setTxStatus={() => null}/>
  );
};

const CreateDatum = ({mfCache, setMfCache}) => {
  const [uri,setUri] = useState("");
  const [datum,setDatum] = useState("");
  const classes = useStyles();

  const handleSubmit = async (evt) => {
      evt.preventDefault();
      const uriObj = null;
      const datumObj = null;
      try {
        uriObj = JSON.parse(uri);
        datumObj = JSON.parse(datum);
      } catch(e) {
        console.log("JSON parsing error: " + e);
        return;
      }
      try {
        const res = await createDatum(uriObj, datumObj);
        console.debug('create-datum result', res)
        setMfCache(_.uniq(_.concat([res],mfCache)));
        console.debug('updated mf', mfCache )
      } catch (e) {
        console.log("create-datum Submit Error",typeof e, e, uriObj, datumObj);
      }
      };
  const inputFields = [
    {
      type:'select',
      label:'URI',
      className:classes.formControl,
      options:_.map(_.filter(mfCache,{type:'uri'}),v=> JSON.stringify(v.value)),
      value:uri,
      onChange:setUri
    },
    {
      type:'textFieldMulti',
      label:'Datum',
      className:classes.formControl,
      value:datum,
      onChange:setDatum
    }
  ];

  return (
    <MakeLocalForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={"stateless tx"} txStatus={"stateless tx"} txRes={"stateless tx"}
      setTxStatus={() => null}/>
  );
};

const CreateManifest = ({mfCache, setMfCache}) => {
  const [uri,setUri] = useState("");
  const [data,setData] = useState([]);
  const classes = useStyles();
  console.debug('CreateManifest', {mfCache});

  const handleSubmit = async (evt) => {
      evt.preventDefault();
      const uriObj = JSON.parse(uri);
      const dataObjList = _.map(data,(v)=>v.value);
      console.debug('create manifest submit', {uriObj,dataObjList});
      try {
        const res = await createManifest(uriObj, dataObjList);
        console.debug('create-manifest result', res)
        setMfCache(_.uniq(_.concat([res],mfCache)));
        console.debug('updated mf', mfCache )
      } catch (e) {
        console.log("create-manifest Submit Error",typeof e, e, uriObj, dataObjList);
      }
      };
  const inputFields = [
    {
      type:'select',
      label:'URI',
      className:classes.formControl,
      options:_.map(_.filter(mfCache,{type:'uri'}),v=> JSON.stringify(v.value)),
      value:uri,
      onChange:setUri
    },
    {
      type:'fixedGroupMultiSelector',
      label:'Data',
      className:classes.formControl,
      options:_.filter(mfCache,{type:'datum'}),
      value:data,
      onChange:setData
    }
  ];

  return (
    <MakeLocalForm
      inputFields={inputFields}
      onSubmit={handleSubmit}
      tx={"stateless tx"} txStatus={"stateless tx"} txRes={"stateless tx"}
      setTxStatus={() => null}/>
  );
};

export const ManifestForms = ({
  mfCache,
  setMfCache,
  tabIdx,
}) => {
  return (
    <ScrollableTabs
      tabIdx={tabIdx}
      tabEntries={[
          {
            label:"Create URI",
            component:
              <CreateUri mfCache={mfCache} setMfCache={setMfCache}/>
          },{
            label:"Create Datum",
            component:
              <CreateDatum mfCache={mfCache} setMfCache={setMfCache}/>
          },{
            label:"Create Manifest",
            component:
              <CreateManifest mfCache={mfCache} setMfCache={setMfCache}/>
          }
        ]}/>
  );
};
