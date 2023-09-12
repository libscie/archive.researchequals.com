const fs = require("fs-extra");

async function doRun() {
  // Read dirs
  const dirs = fs
    .readdirSync("./collections", { withFileTypes: true })
    .filter((item) => item.isDirectory())
    .map((item) => item.name);
  console.log(dirs);
  // For each dir, regenerate the template file
  dirs.map(async (dir) => {
    // read the json file
    let rawColl = await fs.readFile(`./collections/${dir}/${dir}.json`);
    let collection = JSON.parse(rawColl);
    // regen template
    await fs.writeFile(
      `./collections/${collection.suffix}/${collection.suffix}.md`,
      `---
layout: mylayout.njk
---
# {{ title }}
{% if subtitle %}
<div role="doc-subtitle">{{ subtitle }}</div>
{% endif %}
    
{% if type.type == "COMMUNITY" %}
<img class="header-image" src="${`header.${collection.header.name ? collection.header.name.split('.').pop() : collection.header.cdnUrl.split('.').pop()}`}" />
{% endif %}
{% if type.type == "COLLABORATIVE" or type.type == "COMMUNITY" %}
<img class="icon-image" src="${`icon.${collection.icon.name ? collection.icon.name.split('.').pop() : collection.icon.cdnUrl.split('.').pop()}`}" />
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
<p>{{ submission.module.title }}</p>
<p><a href="https://doi.org/{{ submission.module.prefix }}/{{ submission.module.suffix }}">doi: {{ submission.module.prefix }}/{{ submission.module.suffix }}</a></p>
{% if submission.comment and submission.comment != "" %}
<blockquote>{{ submission.comment }}
<div class="quote-footer">—{{ submission.editor.workspace.firstName }} {{ submission.editor.workspace.lastName }}</cite></div class="quote-footer"></blockquote>
{% endif %}
</li>
{% endif %}
{%- endfor -%}
</ul>
{% endif %}
`
    );
  });
}

doRun();
