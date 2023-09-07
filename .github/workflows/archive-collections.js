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
      `https://www.researchequals.com/api/collections?from=${dateRun}`
    );
  } else {
    apiCall = await axios.get("https://www.researchequals.com/api/collections");
  }

  let collectionMeta = { collection: [] };
  apiCall.data.collections.map(async (collection, index) => {
    if (index < 10) {
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
{% if subtitle %}
<div role="doc-subtitle">{{ subtitle }}</div>
{% endif %}

{% if type.type == "COMMUNITY" %}
Add header
{% endif %}
{% if type.type == "COLLABORATIVE" or type.type == "COMMUNITY" %}
Add icon
{% endif %}

doi: <a href="https://doi.org/10.53962/{{ suffix }}">10.53962/{{ suffix }}</a>

Created on ${collection.createdAt.substr(
  0,
  10
)}.

Last updated on  ${collection.updatedAt.substr(
  0,
  10
)}.

## Editors

<ul>
{%- for editor in editors -%}
 <li>
 {% if editor.workspace.orcid %}
 <a href="https://orcid.org/{{ editor.workspace.orcid }}">{{ editor.workspace.firstName }} {{ editor.workspace.lastName }}</a>
 {% else %}
 {{ editor.workspace.firstName }} {{ editor.workspace.lastName }}
 {% endif %}
 </li>
{%- endfor -%}
</ul>

## Summary

{{ description }}

{% if submissions[0] %}
## Collected works
<ul>
{%- for submission in submissions -%}
 {% if submission.accepted %} 
 <li>
 <a href="{{ submission.module.url }}">{{ submission.module.title }}</a>
 {% if submission.comment %}
 <blockquote>{{ submission.comment }}
 <footer>—{{ submission.editor.workspace.firstName }} {{ submission.editor.workspace.lastName }}</cite></footer></blockquote>
 {% endif %}
 </li>
 {% endif %}
{%- endfor -%}
</ul>
{% endif %}
`
      );
      if (collection.type.type != "INDIVIDUAL") {
        await axios({
          method: "get",
          url: collection.icon.cdnUrl,
          responseType: "stream",
        }).then(function (response) {
          response.data.pipe(
            fs.createWriteStream(`./collections/${collection.suffix}/icon.${collection.icon.name ? collection.icon.name.split('.').pop() : collection.icon.cdnUrl.split('.').pop()}`)
          );
        });
      }
    
      if (collection.type.type === "COMMUNITY") {
        await axios({
          method: "get",
          url: collection.header.cdnUrl,
          responseType: "stream",
        }).then(function (response) {
          response.data.pipe(
            fs.createWriteStream(`./collections/${collection.suffix}/header.${collection.header.name ? collection.header.name.split('.').pop() : collection.header.cdnUrl.split('.').pop()}`)
          );
        });
      }
    
    }
  });

  await fs.writeFile(
    `./collections/collections.json`,
    await JSON.stringify(collectionMeta)
  );

  await fs.writeFile(
    `./collections/collections.md`,
    `---
layout: mylayout.njk
---
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
