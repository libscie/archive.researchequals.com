---
layout: mylayout.njk
---
# Modules

<div>
{%- for module in modules -%}
<div><a href="./{{ module.suffix }}">{{ module.title }}</a></div>
{%- endfor -%}
</div>
