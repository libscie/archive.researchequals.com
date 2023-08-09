
# {{ title }}

doi: {{ prefix }}.{{ suffix }}

## Summary

{{ description }}

## Main file

<a href="{{ main.name }}">{{ main.name }}</a>

## Supporting files

These are the original supporting files as uploaded by the author.

{%- for file in supporting.files -%}
  <li><a href="supporting/{{ file.original_filename }}">{{ file.original_filename }}</a></li>
{%- endfor -%}
