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
  let moduleMeta = { modules: [] };
  apiCall = await axios.get("https://www.researchequals.com/api/modules");
  await apiCall.data.modules.map(async (module, index) => {
    moduleMeta.modules.push({
      id: module.id,
      prefix: module.prefix,
      suffix: module.suffix,
      title: module.title,
    });
  })
  await fs.writeFile(
    `./modules/modules.json`,
    await JSON.stringify(moduleMeta)
  );

  if (dateRun) {
    apiCall = await axios.get(
      `https://www.researchequals.com/api/modules?from=${dateRun}`
    );
  } 
  await apiCall.data.modules.map(async (module, index) => {
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
        `---
layout: module.njk
---
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

<ul>
{%- for file in supporting.files -%}
  <li><a href="supporting/{{ file.original_filename }}">{{ file.original_filename }}</a></li>
{%- endfor -%}
</ul>
{% endif %}

{% if references[0] %}
## References

<ol>
{%- for reference in references -%}
<li>{{ reference.title }}. <a href="https://doi.org/{{reference.prefix}}/{{reference.suffix}}">doi: {{reference.prefix}}/{{reference.suffix}}</a></li>
{%- endfor -%}
</ol>
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
    // }
  });

  await fs.writeFile(
    `./modules/modules.md`,
    `---
layout: mylayout.njk
---
# Modules

<div>
{%- for module in modules -%}
<div><a href="./{{ module.suffix }}">{{ module.title }}</a></div>
{%- endfor -%}
</div>
`
  );

  // Write out date file
  dateRun = new Date()
  await fs.writeFile('./.github/date-run-modules', dateRun.toISOString(), (err) => {
      if (err) console.log(err)})
}

doRun();
