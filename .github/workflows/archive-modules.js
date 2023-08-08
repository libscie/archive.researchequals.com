const fs = require("fs-extra");
const axios = require("axios");

async function doRun() {
  // Read date file
  let dateRun;
  try {
    dateRun = fs.readFileSync("./date-run").toString();
  } catch (e) {
    console.log("No previous run found.");
  }
  // Query the API
  let apiCall;
  if (dateRun) {
    apiCall = await axios.get(
      // TODO: Replace with actual URL
      `http://localhost:3000/api/modules?from=${dateRun}`
    );
  } else {
    // TODO: Replace with actual URL
    apiCall = await axios.get("http://localhost:3000/api/modules");
  }
  console.log(apiCall.data.modules);
  apiCall.data.modules.map(async (module, index) => {
    if (index === 0) {
      console.log(module.main);
      // create the relevant paths
      await fs.ensureDir(`./modules/${module.suffix}`);
      // add the metadata
      await fs.writeFile(
        `./modules/${module.suffix}/${module.suffix}.json`,
        JSON.stringify(module)
      );
      await fs.writeFile(
        `./modules/${module.suffix}/${module.suffix}.md`,
        `
---
title: {{ title }}
subtitle: {{ prefix }}.{{ suffix }}
---

## Summary

{{ publishedAt }}

${module.description} {{ id }}

{{ authors }}
`
      );

      // download the main file
      axios({
        method: "get",
        url: module.main.cdnUrl,
        responseType: "stream",
      }).then(function (response) {
        response.data.pipe(
          fs.createWriteStream(`./modules/${module.suffix}/${module.main.name}`)
        );
      });
      // download the supporting files
    }
  });

  // Write out date file
  // dateRun = new Date()
  // console.log(dateRun)
  // await fs.writeFile('date-run', dateRun.toISOString(), (err) => {
  //     if (err) console.log(err)})
}

doRun();
