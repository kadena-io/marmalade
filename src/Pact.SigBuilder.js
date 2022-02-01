import { ContactsOutlined } from '@material-ui/icons';
import Pact from 'pact-lang-api';

// DELTE ME BEFORE MERGE TO pact-lang-api
var enforceType = function(val, type, msg) {
    if (typeof val !== type) {
      throw new TypeError(
        msg + " must be a " + type + ": " + JSON.stringify(val)
      );
    }
  };

// DELTE ME BEFORE MERGE TO pact-lang-api
var enforceArray = function(val, msg) {
    if (!Array.isArray(val)) {
        throw new TypeError(msg + " must be an array: " + JSON.stringify(val));
    }
};

/**
 * DELTE ME BEFORE MERGE TO pact-lang-api
 * Prepare a chainweb-style public meta payload.
 * @param sender {string} gas account
 * @param chainId {string} chain identifier
 * @param gasPrice {number} desired gas price
 * @param gasLimit {number} desired gas limit
 * @param creationTime {number} desired tx's time created in UNIX epoch time as seconds
 * @param ttl {number} desired tx's time to live as seconds
 * @return {object} of arguments, type-checked and properly named.
 */
 var mkMeta = function(sender, chainId, gasPrice, gasLimit, creationTime, ttl) {
    enforceType(sender, "string", "sender");
    enforceType(chainId, "string", "chainId");
    enforceType(gasPrice, "number", "gasPrice");
    enforceType(gasLimit, "number", "gasLimit");
    enforceType(creationTime, "number", "creationTime");
    enforceType(ttl,  "number", "ttl");
    return {
      creationTime: creationTime,
      ttl: ttl,
      gasLimit: gasLimit,
      chainId: chainId,
      gasPrice: gasPrice,
      sender: sender
    };
  };

/**
 * Prepares a capability object for use in mkSignerCList.
 * @param {string} name of pact capability to be signed
 * @param {array} args - array of arguments used in pact capability, default to empty array.
 * @return {object} A properly formatted cap object required in SigBuilder
 */
const mkCap = (name, args=[]) => {
  enforceType(name,"string","name in mkCap must be a string");
  enforceArray(args,"args in mkCap must be an array");
  return {
      name: name,
      args: args
  };
};

/**
 * Standard gas cap, created by mkCap, for use in mkGasSigner or mkSignerCList
 * @return {array} A properly formatted element for the `signers` array field in SigBuilder
 */
const gasCap = mkCap("coin.GAS", []);

/**
 * Convinence function to make the gas cap "signer" element
 * @param {string} publicKey a ED25519 public key for the caps argument
 * @return {array} A properly formatted element for the `signers` array field in SigBuilder
 */
const mkSignerGas = (publicKey) => {
    enforceType(publicKey,"string","mkSignerGas' publicKey must be a string");
    return mkSignerCList(publicKey, [gasCap]);
};

/**
 * Make a Capabilites "signer" array for inclusion in a SigBuilder.
 * @param {string} publicKey a ED25519 public key for the caps argument
 * @param {array} caps an array of caps created with SigBuilder.mkCap
 * @return {array} A properly formatted element for the `signers` array field in SigBuilder
 */
const mkSignerCList = (publicKey, caps) => {
    enforceType(publicKey,"string","mkSigner's publicKey must be a string");
    enforceArray(caps,"mkSigner's caps must be an array");
    return [{
      clist: caps,
      pubKey: publicKey
    }]
};

/**
 * Make an ED25519 (aka unrestricted) "signer" array for inclusion in SigBuilder.
 * @param {string} publicKey a ED25519 public key for the caps argument
 * @return {array} A properly formatted `signers` array field in SigBuilder
 */
const mkSignerUnrestricted = (publicKey) => {
    enforceType(publicKey,"string","mkSigner's publicKey must be a string");
    return [{
      pubKey: publicKey
    }]
};

/**
 * Combine multiple signer arrays created by mkSigner* functions 
 * @param  {array} arrayOfSigners of pact capability to be signed
 * @return {object} A properly formatted cap object required in SigBuilder
 */
const mergeSigners = (arrayOfSigners) => {
    enforceArray(arrayOfSigners, "mergeSigners's arrayOfSigners must be an array");
    return arrayOfSigners.flat();
};

/**
 * Convinence function to get a creation time set to system's local time
 * @return {number} seconds since epoch
 */
const autoCreationTime = () => Math.round(new Date().getTime() / 1000) - 15;

/**
 * Convinence function to get a nonce for use in CmdJSON
 * @return {string} the string "SigBuilder:".concat(Date.toISOString) 
 */
const autoNonce = () => JSON.stringify(new Date().toISOString());

/**
 * Generates a correctly formatted exec `cmd` field for use in the SigData type
 * FYI SigData is compatible with:
 *  - `pact -u` on the command line
 *  - SigBuilder in Chainweaver
 * @param {string} pactCode the pact code of the command
 * @param {object} envData the environmental data of the executing pact code
 * @param {array} signers the output of mkSigner-class functions
 * @param {string} networkId
 * @param {object} meta output of mkMeta
 * @param {string} nonce for the tx (`autoNonce` is used if empty)
 * @return {object} cmdJSON object for an exec tx
 */
const mkExecCmdJSON = (
  pactCode, 
  envData, 
  signers,
  networkId,
  meta,
  nonce=""
) => {
  enforceType(pactCode, "string", "mkCmdJSON's pactCode must be a string");
  enforceType(envData, "object", "mkCmdJSON's envData must be an object");
  enforceArray(signers, "mkCmdJSON's signers must be an array");
  enforceType(networkId, "string", "mkCmdJSON's networkId must be an array");
  enforceType(meta, "object", "mkCmdJSON's must be an object");
  enforceType(nonce, "string", "mkCmdJSON's nonce must be a string");
    
  const cmdJSON = {
    networkId: networkId,
    payload: {
      exec: {
        data: envData || {},
        code: pactCode
      }
    },
    signers: signers,
    meta,
    nonce: nonce || autoNonce()
  };
  console.debug('cmdJSON', cmdJSON);
  return cmdJSON;
};

/**
 * Get the pubKeys from a signers field
 * @param {array} signers the signers field from cmdJSON
 * @return {array} signer keys used in tx
 */
const pubKeysFromSigners = (signers) => {
  enforceArray(signers, "pubKeysFromSigners' signers must be an array");
  return signers.map(v=>v.pubKey);
};


/**
 * Create the SigData Object for `exec` functions
 * @param {object} cmdJSON returned from mkExecCmdJSON 
 * @param {array} signers is an optional array of pubKeys, overrides those found in signers's caps
 * @return {object} the SigData object for use in SigBuilder 
 */
const mkExecSigData = (cmdJSON, signers=[]) => {
  var unsignedSigs = {};
  if (signers.length) {
    signers.map(pubKey => {
        unsignedSigs[pubKey] = null;
        return null;
    });
  } else {
    pubKeysFromSigners(cmdJSON.signers).map(pubKey => {
        unsignedSigs[pubKey] = null;
        return null;
    });
  }
  const cmdJSONasString = JSON.stringify(cmdJSON);
  const sigDataExec = {hash: Pact.crypto.hash(cmdJSONasString), cmd: cmdJSONasString, sigs: unsignedSigs};
  console.debug("mkExecSigData", sigDataExec);
  return sigDataExec;
};


const mkWalletTestCmd1 = ({
  user, 
  signingPubKey, 
  networkId,
  gasPrice,
  gasLimit
}) => {
  //creates transaction to send to wallet
  const caps = mkSignerGas(signingPubKey);
  const ms = {
      sender: user,
      chainId: "0",
      gasPrice: gasPrice,
      gasLimit: gasLimit,
      creationTime: autoCreationTime(),
      ttl: 28800};
  const meta = Pact.lang.mkMeta(ms.sender, ms.chainId, ms.gasPrice, ms.gasLimit, ms.creationTime, ms.ttl);

  console.debug("caps", caps);
  const cs = {
      pactCode: "(+ 1 1)",
      envData: {foo: "bar"},
      signers: caps,
      networkId: networkId,
      meta: meta
  };
  const cmdJSON = mkExecCmdJSON(
    cs.pactCode, 
    cs.envData, 
    cs.signers,
    cs.networkId,
    cs.meta,
    );
  const execSigData = mkExecSigData(cmdJSON);
//   console.debug("cmdJSON", sigDataExec);
  return execSigData;
};

export const SigData = {
                mkMeta,
                mkCap,
                gasCap,
                mkSignerGas,
                mkSignerCList,
                mkSignerUnrestricted,
                mergeSigners,
                autoCreationTime,
                autoNonce,
                pubKeysFromSigners,
                mkExecCmdJSON,
                mkWalletTestCmd1
    };

// doesn't work    
// {
//     "hash": "m2KUMFV6BlG4AS1DQBynrkm08k_68ANdtgWtIf-3T5Y",
//     "cmd": "{\"networkId\":\"testnet04\",\"payload\":{\"exec\":{\"data\":{\"foo\":\"bar\"},\"code\":\"(+ 1 1)\"}},\"signers\":[{\"clist\":[{\"cap\":{\"name\":\"coin.GAS\",\"args\":[]}}],\"pubKey\":\"188008e05779a3b9c4f417bc7887b55d5cda0f1cabdd174b7c0f727f0bb3abd7\"}],\"meta\":{\"creationTime\":1643732425,\"ttl\":28800,\"gasLimit\":10000,\"chainId\":\"0\",\"gasPrice\":0.000001,\"sender\":\"188008e05779a3b9c4f417bc7887b55d5cda0f1cabdd174b7c0f727f0bb3abd7\"},\"nonce\":\"\\\"2022-02-01T16:20:40.238Z\\\"\"}",
//     "sigs": {}
// }

// does
// {
//     "hash": "T9J57kmCyelVupyqQ5TWciRVxwe2sD93oPXUmrKOEq8",
//     "cmd": "{\"networkId\":\"testnet04\",\"payload\":{\"exec\":{\"data\":{\"foo\":\"bar\"},\"code\":\"(+ 1 1)\"}},\"signers\":[{\"clist\":[{\"name\":\"coin.GAS\",\"args\":[]}],\"pubKey\":\"188008e05779a3b9c4f417bc7887b55d5cda0f1cabdd174b7c0f727f0bb3abd7\"}],\"meta\":{\"creationTime\":1643732425,\"ttl\":28800,\"gasLimit\":1000,\"chainId\":\"0\",\"gasPrice\":0.000001,\"sender\":\"188008e05779a3b9c4f417bc7887b55d5cda0f1cabdd174b7c0f727f0bb3abd7\"},\"nonce\":\"\\\"2022-02-01T16:20:40.240Z\\\"\"}",
//     "sigs": {
//         "188008e05779a3b9c4f417bc7887b55d5cda0f1cabdd174b7c0f727f0bb3abd7": null
//     }
// }