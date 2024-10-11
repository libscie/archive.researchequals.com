---
layout: mylayout.njk
---
# {{ title }}

doi: <a href="https://doi.org/{{ prefix }}/{{ suffix }}">{{ prefix }}/{{ suffix }}</a>

Originally published on 2023-01-18 under a <a href="{{ license.url }}">{{ license.name }}</a> 

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

