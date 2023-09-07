const fs = require("fs-extra");

async function doRun() {
  // Read dirs
  const dirs = fs
    .readdirSync("./modules", { withFileTypes: true })
    .filter((item) => item.isDirectory())
    .map((item) => item.name);
  // For each dir, regenerate the template file
  dirs.map(async (dir) => {
    // read the json file
    let rawMod = await fs.readFile(`./modules/${dir}/${dir}.json`);
    let module = JSON.parse(rawMod);
    // regen template
    await fs.writeFile(
      `./modules/${module.suffix}/${module.suffix}.md`,
      `# {{ title }}

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

<ul>
{%- for reference in references -%}
<li><a href="https://doi.org/{{reference.prefix}}/{{reference.suffix}}">{{ reference.title }}</a></li>
{%- endfor -%}
</ul>
{% endif %}

`
    );
  });
}

doRun();
