const fs = require("fs-extra");
const axios = require("axios");

async function doRun() {
  // Read date file
  let dateRun;
  try {
    dateRun = fs.readFileSync("./.github/date-run-collections").toString();
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

  let collectionMeta = { collection: [] };
  apiCall.data.collections.map(async (collection, index) => {
    if (index === 1) {
      collectionMeta.collection.push({
        id: collection.id,
        suffix: collection.suffix,
        title: collection.title,
      });
      // create the relevant paths
      await fs.ensureDir(`./collections/`);
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

  await fs.writeFile(
    `./collections/collections.json`,
    await JSON.stringify(collectionMeta)
  );

  await fs.writeFile(
    `./collections/collections.md`,
    `
# Collections

This is a list of all the ResearchEquals Collections.

{%- for collection in collection -%}
<li><a href="./{{ collection.suffix }}">{{ collection.title }}</a></li>
{%- endfor -%}
`
  );

  // Write out date file
  // dateRun = new Date()
  // console.log(dateRun)
  // await fs.writeFile('./.github/date-run-collections', dateRun.toISOString(), (err) => {
  //     if (err) console.log(err)})
}

doRun();
