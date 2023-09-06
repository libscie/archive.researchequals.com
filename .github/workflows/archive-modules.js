const fs = require("fs-extra");
const axios = require("axios");

async function doRun() {
  // Read date file
  let dateRun;
  try {
    dateRun = fs.readFileSync("./.github/date-run-modules").toString();
  } catch (e) {
    console.log("No previous run found.");
  }
  // Query the API
  let apiCall;
  if (dateRun) {
    apiCall = await axios.get(
      // TODO: Replace with actual URL
      `https://www.researchequals.com/api/modules?from=${dateRun}`
    );
  } else {
    // TODO: Replace with actual URL
    apiCall = await axios.get("https://www.researchequals.com/api/modules");
  }
  let moduleMeta = { modules: [] };
  await apiCall.data.modules.map(async (module, index) => {
    if (module.id === 1) {
      moduleMeta.modules.push({
        id: module.id,
        prefix: module.prefix,
        suffix: module.suffix,
        title: module.title,
      });

      // create the relevant paths
      await fs.ensureDir(`./modules/`);
      await fs.ensureDir(`./modules/${module.suffix}`);
      // add the metadata
      await fs.writeFile(
        `./modules/${module.suffix}/${module.suffix}.json`,
        JSON.stringify(module)
      );
      await fs.writeFile(
        `./modules/${module.suffix}/${module.suffix}.md`,
        `
# {{ title }}

doi: <a href="https://doi.org/{{ prefix }}/{{ suffix }}">{{ prefix }}/{{ suffix }}</a>

Originally published on ${module.publishedAt.substr(
          0,
          10
        )} under a <a href="{{ license.url }}">{{ license.name }}</a> 

## Authors

<ul>
{%- for author in authors -%}
 <li>
 {% if author.workspace.orcid %}
 <a href="https://orcid.org/{{ author.workspace.orcid }}">{{ author.workspace.firstName }} {{ author.workspace.lastName }}</a>
 {% else %}
 {{ author.workspace.firstName }} {{ author.workspace.lastName }}
 {% endif %}
 </li>
{%- endfor -%}
</ul>

## Summary

{{ description }}

## Main file

<a href="{{ main.name }}">{{ main.name }}</a>

{% if supporting.files[0] %}
## Supporting files

These are the original supporting files as uploaded by the author.

{%- for file in supporting.files -%}
  <li><a href="supporting/{{ file.original_filename }}">{{ file.original_filename }}</a></li>
{%- endfor -%}
{% endif %}
`
      );

      // download the main file
      await axios({
        method: "get",
        url: module.main.cdnUrl,
        responseType: "stream",
      }).then(function (response) {
        response.data.pipe(
          fs.createWriteStream(`./modules/${module.suffix}/${module.main.name}`)
        );
      });
      // download the supporting files
      if (module.supporting.files.length > 0) {
        // create the relevant paths
        await fs.ensureDir(`./modules/${module.suffix}/supporting`);
        module.supporting.files.map(async (file) => {
          await axios({
            method: "get",
            url: file.original_file_url,
            responseType: "stream",
          }).then(function (response) {
            response.data.pipe(
              fs.createWriteStream(
                `./modules/${module.suffix}/supporting/${file.original_filename}`
              )
            );
          });
        });
      }
    }
  });

  await fs.writeFile(
    `./modules/modules.json`,
    await JSON.stringify(moduleMeta)
  );

  await fs.writeFile(
    `./modules/modules.md`,
    `
# Modules

This is a list of all the published modules

{%- for module in modules -%}
<li><a href="./{{ module.suffix }}">{{ module.title }}</a></li>
{%- endfor -%}
`
  );

  // Write out date file
  // dateRun = new Date()
  // console.log(dateRun)
  // await fs.writeFile('./.github/date-run-modules', dateRun.toISOString(), (err) => {
  //     if (err) console.log(err)})
}

doRun();
