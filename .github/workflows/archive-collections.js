const fs = require("fs-extra");
const axios = require("axios");

async function doRun() {
  // Read date file
  let dateRun;
  try {
    dateRun = fs.readFileSync("./date-run-collections").toString();
  } catch (e) {
    console.log("No previous run found for collections.");
  }
  // Query the API
  let apiCall;
  if (dateRun) {
    apiCall = await axios.get(
      // TODO: Replace with actual URL
      `https://www.researchequals.com/api/collections?from=${dateRun}`
    );
  } else {
    // TODO: Replace with actual URL
    apiCall = await axios.get("https://www.researchequals.com/api/collections");
  }
  apiCall.data.collections.map(async (collection, index) => {
    if (index === 0) {
      console.log(collection);
      // create the relevant paths
      await fs.ensureDir(`./collections/${collection.suffix}`);
      // add the metadata
      await fs.writeFile(
        `./collections/${collection.suffix}/${collection.suffix}.json`,
        JSON.stringify(collection)
      );
      await fs.writeFile(
        `./collections/${collection.suffix}/${collection.suffix}.md`,
        `
# {{ title }}

doi: {{ prefix }}.{{ suffix }}

## Summary

{{ description }}
      );
`
      );
    }
  });

  // Write out date file
  // dateRun = new Date()
  // console.log(dateRun)
  // await fs.writeFile('date-run', dateRun.toISOString(), (err) => {
  //     if (err) console.log(err)})
}

doRun();
