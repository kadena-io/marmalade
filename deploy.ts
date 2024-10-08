#!/usr/bin/env tsx

import { deployFile } from "./utils/deploy";

if (process.argv.length > 1) {
  const [configFilePath, signersFilePath, pw] = process.argv.slice(2);
  if (!configFilePath) throw new Error("No config file path provided");
  if (!signersFilePath) throw new Error("No signers file path provided");
  deployFile(configFilePath, signersFilePath, pw);
}
