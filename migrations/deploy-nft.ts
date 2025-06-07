import { ClarinetDeploy } from "@stacks/clarinet-sdk";

await ClarinetDeploy.deployContract({
  contractName: "nft-collection",
  filePath: "./contracts/nft-collection.clar",
});
