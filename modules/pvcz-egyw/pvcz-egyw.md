
# {{ title }}

doi: <a href="https://doi.org/{{ prefix }}/{{ suffix }}">{{ prefix }}/{{ suffix }}</a>

Authors: {%- for author in authors -%}
{{ author.workspace.firstName }} {{ author.workspace.lastName }}
{%- endfor -%}

Originally published on 2022-02-10, by <AUTHORS> under a <LICENSE>.

## Summary

{{ description }}

## Main file

<a href="{{ main.name }}">{{ main.name }}</a>

## Supporting files

These are the original supporting files as uploaded by the author.

{%- for file in supporting.files -%}
  <li><a href="supporting/{{ file.original_filename }}">{{ file.original_filename }}</a></li>
{%- endfor -%}
