//basic React api imports
import React, {useState, useEffect} from "react";
import { makeStyles } from '@material-ui/core/styles';
import { Alert, AlertTitle } from '@material-ui/lab';
import CloseIcon from '@material-ui/icons/Close';
import {
  Container,
  Paper,
  Box,
  Grid,
  Divider,
  List,
  ListItem,
  ListItemText,
  Typography,
  Dialog,
  AppBar,
  Toolbar,
  Slide,
} from '@material-ui/core';
import { Modal, Button } from '@material-ui/core';
import Collapse from '@material-ui/core/Collapse';
import IconButton from '@material-ui/core/IconButton';

import { hftAPI } from "./kadena-config.js";
import {dashStyleNames2Text, PactSingleJsonAsTable} from "./util.js";
import Pact from "pact-lang-api";

var debugMode = false;

const debug = (...args) => {
  if (debugMode && args.length) {
    console.debug("[PactTxStatus]", ...args);
  };
};

export const toggleDebug = () => {
  debugMode = !debugMode;
  console.log("[PactTxStatus] debugMode set to", debugMode);
};

const useStyles = makeStyles((theme) => ({
  root: {
    width: '100%',
    '& > * + *': {
      marginTop: theme.spacing(2),
    },
  },
}));

export const signNewPactTx = (
  sigData, 
  {setTx, setTxRes, setTxStatus}
) => {
  setTx(sigData);
  setTxRes({});
  setTxStatus('do-signing');
};

const trackSigDataResult = async (
  sigData,
  setTxStatus,
  setTxRes,
  host,
  refresh=(()=>{})
) => {
    debug("trackSigDataResults start", {sigData, host});
    try {
      //sends signed transaction to blockchain
      const txReqKey = sigData.hash 
      //set html to wait for transaction response
      //set state to wait for transaction response
      setTxStatus('pending')
      try {
        //listens to response to transaction sent
        //  note method will timeout in two minutes
        //    for lower level implementations checkout out Pact.fetch.poll() in pact-lang-api
        let retries = 8;
        let res = {};
        while (retries > 0) {
          //sleep the polling
          await new Promise(r => setTimeout(r, 15000));
          res = await Pact.fetch.poll({requestKeys:[txReqKey]}, host);
          debug("trackSigDataResult", {res});
          try {
            if (res[txReqKey].result.status) {
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
        if (res[txReqKey].result.status === "success"){
          debug("tx status set to success");
          //set state for transaction success
          setTxStatus('success');
          refresh();
        } else if (retries === 0) {
          debug("tx status set to timeout");
          setTxStatus('timeout');
          refresh();
        } else {
          debug("tx status set to failure");
          //set state for transaction failure
          setTxStatus('failure');
        }
      } catch(e) {
        // TODO: use break in the while loop to capture if timeout occured
        debug("tx api failure",e);
        setTxRes(e);
        setTxStatus('failure');
      }
    } catch(e) {
      setTxRes(e.toString());
      debug("tx status set to validation error",e);
      //set state for transaction construction error
      setTxStatus('validation-error');
    }
};

export const PactTxStatus = ({
  pactTxStatus, 
  host, 
  refresh=(()=>{})
}) => {
  const {tx, txRes, setTxRes, txStatus, setTxStatus} = pactTxStatus;
  const [open,setOpen] = useState(true);
  const [modalOpen,setModalOpen] = useState(false);
  const classes = useStyles();
  const severity = txStatus === "pending" ? 'info'
                   : txStatus === "success" ? 'success'
                   : txStatus === "timeout" ? 'warning'
                   : 'error' ;
  useEffect(()=>
    setOpen(true)
  ,[txStatus]);

  useEffect(()=>{
    if (txStatus === "submitted") {
      console.log("[PactTxStatus] trackSigDataResults", {tx, host});
      trackSigDataResult(tx,setTxStatus,setTxRes,host,refresh);
    }
  }, [txStatus])

  return (
    (
      txStatus === "do-signing" ?
        <TxSigner pactTxStatus={pactTxStatus}/> 
    : txStatus === "" || txStatus === "submitted" ? 
        <React.Fragment/> 
    : <div className={classes.root}>
        <Collapse in={open}>
          <Alert
            severity={severity}
            action={
              <IconButton
                aria-label="close"
                color="inherit"
                size="small"
                onClick={() => setOpen(false)}>
                <CloseIcon fontSize="inherit" />
              </IconButton>

            }
          >

          <AlertTitle>
            <Grid
              container
              direction="row"
              justify="flex-start"
              alignItems="center"
            >
              <Grid item>
                {dashStyleNames2Text(txStatus)}
              </Grid>
              {txStatus === 'validation-error' || txStatus === 'failure'
                ?
                  <Grid item>
                    <Divider orientation="vertical" flexItem />
                    <Button size='small' onClick={()=>setModalOpen(true)}>
                      View Error
                    </Button>
                  </Grid>
                : <React.Fragment/>}
            </Grid>

          </AlertTitle>
            { txStatus === "pending" ? (
              <React.Fragment>
                <p>Awaiting Confirmation</p>
                <a href={`${hftAPI.explorerURL}/tx/${tx.hash}`}>
                  Eventual Block Explorer Link
                </a>
              </React.Fragment>
            ) : txStatus === "success" ? (
              <React.Fragment>
                <a href={`${hftAPI.explorerURL}/tx/${tx.hash}`}>
                  View transaction in Block Explorer
                </a>
              </React.Fragment>
            ) : txStatus === "failure" ? (
              <React.Fragment>
                <a href={`${hftAPI.explorerURL}/tx/${tx.hash}`}>
                  View transaction in Block Explorer
                </a>
                <div>
                  <Modal
                    open={modalOpen}
                    onClose={() => setModalOpen(false)}
                  >
                    <Container maxWidth="lg">
                      <PactSingleJsonAsTable json={txRes}/>
                    </Container>
                  </Modal>
                </div>
              </React.Fragment>
            ) : txStatus === "timeout" ? (
              <React.Fragment>
                <p>...but your tx was sent.</p>
                <a href={`${hftAPI.explorerURL}/tx/${tx.hash}`}>
                  View transaction in Block Explorer
                </a>
              </React.Fragment>
            ) : txStatus === "validation-error" ? (
              <React.Fragment>
                <Box>
                  Transaction was not sent to Blockchain. Check your keys or metadata.
                </Box>
                <div>
                  <Modal
                    open={modalOpen}
                    onClose={() => setModalOpen(false)}
                  >
                    <Container maxWidth="md">
                        <Paper>{JSON.stringify(txRes)}</Paper>
                    </Container>
                  </Modal>
                </div>
              </React.Fragment>
            ) : (
              <React.Fragment/>
            )}
          </Alert>
        </Collapse>
      </div>
    )
  );
};

const txSignerStyles = makeStyles((theme) => ({
  appBar: {
    position: 'relative',
  },
  title: {
    marginLeft: theme.spacing(2),
    flex: 1,
  },
}));

const TxSignerTransition = React.forwardRef(function Transition(props, ref) {
  return <Slide direction="up" ref={ref} {...props} />;
});

export const TxSigner = ({
  pactTxStatus: {tx, setTxStatus}
}) => {
  const [open,setOpen] = useState(true);

  const handleClose = () => {
    setTxStatus("");
    setOpen(false);
  };

  const handleTxSubmit = () => {
    setTxStatus("submitted");
    setOpen(false);
  };

  const classes = txSignerStyles();

  return (
      <Dialog fullScreen open={open} onClose={handleClose} TransitionComponent={TxSignerTransition}>
        <AppBar className={classes.appBar}>
          <Toolbar>
            <IconButton edge="start" color="inherit" onClick={handleClose} aria-label="close">
              <CloseIcon />
            </IconButton>
            <Typography variant="h6" className={classes.title}>
              New Transaction
            </Typography>
            <Button autoFocus variant="outlined" color="default" onClick={handleTxSubmit}>
              I Submitted the Transaction 
            </Button>
          </Toolbar>
        </AppBar>
        <List>
          <ListItem>
            <ListItemText primary="Copy The Transaction's SigData To Clipboard" secondary={JSON.stringify(tx)} />
          </ListItem>
          <Divider />
          <ListItem>
            <ListItemText primary="Transaction Hash" secondary={tx.hash} />
          </ListItem>
        </List>
      </Dialog>
  );
}