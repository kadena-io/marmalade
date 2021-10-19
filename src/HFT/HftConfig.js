//basic React api imports
import React from "react";
import { hftAPI, keyFormatter } from "../kadena-config.js";
import { PactSingleJsonAsTable } from "../util.js";

export const HftConfig = () => {
  return (
    <PactSingleJsonAsTable
      json={hftAPI}
      keyFormatter={keyFormatter}
      />
  )
};
