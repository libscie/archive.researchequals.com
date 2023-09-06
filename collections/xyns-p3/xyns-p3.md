
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

Created on 2023-07-27.

Last updated on  2023-07-27.

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
