---
layout: mylayout.njk
---
# Collections

<div>
{%- for collection in collection -%}
<div><a href="./{{ collection.suffix }}">{{ collection.title }}</a></div>
{%- endfor -%}
</div>
