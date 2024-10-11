---
layout: mylayout.njk
---
# {{ title }}
{% if subtitle %}
<div role="doc-subtitle">{{ subtitle }}</div>
{% endif %}
    
{% if type.type == "COMMUNITY" %}
<img class="header-image" src="header.jpg" />
{% endif %}
{% if type.type == "COLLABORATIVE" or type.type == "COMMUNITY" %}
<img class="icon-image" src="icon.svg" />
{% endif %}

doi: <a href="https://doi.org/10.53962/{{ suffix }}">10.53962/{{ suffix }}</a>

Created on 2023-02-22.

Last updated on  2023-02-22.

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
