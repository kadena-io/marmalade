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
} from '@material-ui/core';
import { Modal, Button } from '@material-ui/core';
import Collapse from '@material-ui/core/Collapse';
import IconButton from '@material-ui/core/IconButton';

import { daoAPI } from "./kadena-config.js";
import {dashStyleNames2Text, PactSingleJsonAsTable} from "./util.js";

const useStyles = makeStyles((theme) => ({
  root: {
    width: '100%',
    '& > * + *': {
      marginTop: theme.spacing(2),
    },
  },
}));

export const PactTxStatus = (props) => {
  // TODO: make these msgs hideable
  const tx = props.tx;
  const txRes = props.txRes;
  const txStatus = props.txStatus;
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

  return (
    txStatus ?
      <div className={classes.root}>
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
                <a href={`${daoAPI.explorerURL}/tx/${tx.hash}`}>
                  Eventual Block Explorer Link
                </a>
              </React.Fragment>
            ) : txStatus === "success" ? (
              <React.Fragment>
                <a href={`${daoAPI.explorerURL}/tx/${tx.hash}`}>
                  View transaction in Block Explorer
                </a>
              </React.Fragment>
            ) : txStatus === "failure" ? (
              <React.Fragment>
                <a href={`${daoAPI.explorerURL}/tx/${tx.hash}`}>
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
                <a href={`${daoAPI.explorerURL}/tx/${tx.hash}`}>
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
    : <React.Fragment/>
  );
};
