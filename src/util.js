// For util functions
import React, {useState, useEffect, useMemo} from "react";
//make JS less terrible
import _ from "lodash";
import { makeStyles } from '@material-ui/core/styles';
//Table Stuff
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Checkbox,
  FormControl,
} from '@material-ui/core';
import {
  Button,
  LinearProgress,
  TextField,
  MenuItem,
  CardActions,
} from '@material-ui/core';
//pact-lang-api for blockchain calls
//config file for blockchain calls
import { PactTxStatus } from "./PactTxStatus.js";
import { MDEditor } from "./Markdown";
import { KeySelector, usePactWallet } from "./PactWallet.js";

export const useInputStyles = makeStyles((theme) => ({
  root: {
    display: 'flex',
    flexWrap: 'wrap',
    '& .MuiTextField-root': {
      margin: theme.spacing(1),
      width: '25ch',
    },
  },
  margin: {
    margin: theme.spacing(1),
  },
  withoutLabel: {
    marginTop: theme.spacing(3),
  },
  textField: {
    width: '25ch',
  },
  formControl: {
    margin: theme.spacing(1),
    minWidth: 120,
  },
  selectEmpty: {
    marginTop: theme.spacing(2),
  },
}));

//config file for blockchain calls

export const dashStyleNames2Text = str => str.split("-").map(k=>k.replace(new RegExp("^.","gm"),a=>a.toUpperCase())).join(' ');

const isRootPactValue = (val) => {
  if (val && typeof val === 'object' ) {
    if ('timep' in val || 'int' in val || 'decimal' in val || 'time' in val ) {
      return true;
    } else {
      return false;
    }
  } else {
    return true;
  }
};

export const isPactKeyset = (val) => {
  if (val && typeof val === 'object' ) {
    if (Object.keys(val).length === 2 &&'pred' in val && 'keys' in val) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
};


export const renderPactValue = (val) => {
  if (val && typeof val === 'object') {
    if ('time' in val) {
      return val['time'];
    } else if ('timep' in val) {
      return val['timep'];
    } else if ('int' in val) {
      return typeof val['int'] === 'string' ? val['int'] : val['int'].toLocaleString();
    } else if ('decimal' in val) {
      return typeof val['decimal'] === 'string' ? val['decimal'] : val['decimal'].toLocaleString();
    } else if ('pred' in val && 'keys' in val) {
      return JSON.stringify(val);
    } else {
      return JSON.stringify(val);
    }
  } else if (typeof val === 'boolean') {
    return val.toString();
  } else if (typeof val === 'string') {
    return val;
  } else if (typeof val === 'number'){
    return val.toLocaleString()
  } else {
    return JSON.stringify(val);
  }
};

export const FlatPaper = ({...rest}) => <Paper elevation={0} {...rest}/>;

const useToplevelTableStyles = makeStyles({
  table: {
    minWidth: 650,
  },
});

const useNestedTableStyles = makeStyles({
  table: {
    minWidth: 650,
  },
  root: {
    '& > *': {
      borderBottom: 'unset',
    },
  },
});

export const PactSingleJsonAsTable = (props) => {
  const json = props.json || {};
  const isNested = props.isNested || false;
  const classes = isNested ? useNestedTableStyles : useToplevelTableStyles;
  const header = props.header || [];
  const keyFormatter = props.keyFormatter ? props.keyFormatter : (k) => {return (k)};
  const valFormatter = props.valFormatter ? props.valFormatter : (str) => <code>{renderPactValue(str)}</code>;
  const internals = () =>
    <React.Fragment>
      <TableHead>
        <TableRow>
        {header.map((val) => {
          return <TableCell>{val}</TableCell>;
        })}
        </TableRow>
      </TableHead>

      <TableBody>
        {Object.entries(json).map(([k,v]) => {
          return (
          <TableRow key={k}>
            { Array.isArray(json) === false ? (
              <TableCell>{keyFormatter(k)}</TableCell>
            ) : (
              <React.Fragment></React.Fragment>
            )}
            { isRootPactValue(v) ? (
              <TableCell>{valFormatter(v)}</TableCell>
            ) : typeof v === "object" ? (
              <PactSingleJsonAsTable
                json={v}
                keyFormatter={keyFormatter}
                valFormatter={valFormatter}
                isNested={true}/>
            ) : typeof v === "function" ? (
              <TableCell>{valFormatter(v.toString())}</TableCell>
            ) : (
              <TableCell>{valFormatter(v)}</TableCell>
            )}
          </TableRow>
          )
        })}
      </TableBody>
    </React.Fragment>;

  return (
    isNested ? (
      <Table className={classes.table} size='small' aria-label="simple table">
        {internals()}
      </Table>
    ) : (
    <TableContainer component={FlatPaper}>
      <Table className={classes.table} size='small' aria-label="simple table">
        {internals()}
      </Table>
    </TableContainer>
    )
)};

export const PactJsonListAsTable = (props) => {
  const json = _.isArray(props.json) ? props.json : [];
  const isNested = props.isNested || false;
  const classes = isNested ? useNestedTableStyles : useToplevelTableStyles;
  const header = props.header || [];
  let keyOrder = [];
  if (props.keyOrder) {
    keyOrder = props.keyOrder;
  } else if (Array.isArray(props.json)) {
    if ( json.length > 0 ) {
      keyOrder = Object.keys(json[0]);
    }
  }
  const keyFormatter = props.keyFormatter ? props.keyFormatter : (k) => {return (k)};
  const valFormatter = props.valFormatter ? props.valFormatter : (str) => <code>{renderPactValue(str)}</code>;
  const kvFunc = props.kvFunc || {};

  const internals = () =>
    <React.Fragment>
        <TableHead>
          <TableRow>
          {header.map((val) => (
            <TableCell key={val}>{val}</TableCell>
          ))}
          </TableRow>
        </TableHead>
        <TableBody>
          {json.map(obj => (
            <TableRow key={obj[keyOrder[0]]}>
              { keyOrder.map(k => {
                  const v = obj[k];
                  return (
                    <TableCell key={k}>
                      { _.has(kvFunc,k) ? (
                          kvFunc[k](obj)
                      ) : isRootPactValue(v) ? (
                          valFormatter(v)
                      ) : Array.isArray(v) ? (
                          <PactJsonListAsTable
                            json={v}
                            keyFormatter={keyFormatter}
                            valFormatter={valFormatter}
                            isNested={true}/>
                      ) : typeof v === "object" ? (
                          <PactSingleJsonAsTable
                            json={v}
                            keyFormatter={keyFormatter}
                            valFormatter={valFormatter}
                            isNested={true}/>
                      ) : typeof v === "function" ? (
                          valFormatter(v.toString())
                      ) : (
                          valFormatter(v)
                      )}
                    </TableCell>
                  )
                }
            )}
            </TableRow>
          ))}
      </TableBody>
    </React.Fragment>;

  return (
    isNested ? (
      <Table className={classes.table} size='small' aria-label="simple table">
        {internals()}
      </Table>
    ) : (
    <TableContainer component={FlatPaper}>
      <Table className={classes.table} size='small' aria-label="simple table">
        {internals()}
      </Table>
    </TableContainer>
    )
)};

export const MakeInputField = (props) => {
  const {
    type,
    label,
    options,
    placeholder,
    className,
    onChange,
    value
  } = props.inputField;

  const field = ( type === 'select'
    ? <TextField
        id="outlined-multiline-static"
        select
        required
        fullWidth
        className={className}
        variant="outlined"
        label={label}
        value={value}
        onChange={e => {
          // console.log(`Selected ${e.target.value}`,e); 
          onChange(e.target.value)}
          }
        >
        
        { options.map(k =>
            <MenuItem key={k} value={k}>
              {k}
            </MenuItem>
          )
        }
      </TextField>
    : type === 'textFieldSingle' ?
      <TextField
        required
        fullWidth
        value={value}
        className={className}
        placeholder={placeholder}
        variant='outlined'
        label={label}
        onChange={e => onChange(e.target.value)}
      />
    : type === 'textFieldMulti' ?
      <TextField
        required
        fullWidth
        label={label}
        className={className}
        multiline
        rows={4}
        variant="outlined"
        placeholder={placeholder}
        onChange={e => onChange(e.target.value)}
      />
    : type === 'checkbox' ? 
        <Checkbox
          checked={value}
          onChange={e=>onChange(e.target.value)}
          color="primary"
          label={label}
        />
    : type === 'markdown' ?
        <MDEditor
          value={value}
          onChange={onChange}/>
    : type === 'keySelector' ?
        <KeySelector
          label={label}
          getVal={value}
          setVal={onChange}
          allOpts={options}
          />
    : null
  );

  return field;

};

export const MakeForm = (props) => {
  const {
    inputFields,
    onSubmit,
    tx, txRes, txStatus, setTxStatus
  } = props;
  const {current: {walletName, signingKey}} = usePactWallet();
  const [wasSubmitted,setWasSubmitted] = useState(false);
  useEffect(()=>setWasSubmitted(false),[inputFields]);
  useEffect(()=>txStatus !== "" ? setWasSubmitted(true) : setWasSubmitted(wasSubmitted), [txStatus])

  return (
    <div>
      <form
        autoComplete="off"
        onSubmit={e => onSubmit(e)}>
        {inputFields.map(f =>
          <MakeInputField inputField={f}/>
        )}
        <CardActions>
          {txStatus === 'pending'
            ? null
            : <Button variant="outlined" color="default" type="submit" disabled={wasSubmitted}>
                {walletName ? 
                  (wasSubmitted ? "Complete Signing in Wallet": `Sign with ${walletName} using key ${signingKey.substring(0,4)}...${signingKey.substring(signingKey.length - 4)}`)
                  : "Configure and select a wallet before proceeding"}
              </Button>
          }
        </CardActions>
      </form>
      { txStatus === 'pending' ? <LinearProgress /> : null }
      <PactTxStatus tx={tx} txRes={txRes} txStatus={txStatus} setTxStatus={setTxStatus}/>
    </div>
  )
};
