import { ClarinetDeploy } from "@stacks/clarinet-sdk";

await ClarinetDeploy.deployContract({
  contractName: "governance",
  filePath: "./contracts/governance.clar",
});
