import { ClarinetClient } from "@stacks/clarinet-sdk";

const client = new ClarinetClient();

async function configure() {
  // Link trait implementations
  await client.callPublicFunction("nft-collection", "set-governance-contract", ["'SPXXXX.governance"]);
  await client.callPublicFunction("staking", "set-nft-contract", ["'SPXXXX.nft-collection"]);
  await client.callPublicFunction("impact-tracker", "set-reward-token", ["'SPXXXX.gip-token"]);
}

configure();
