# Codebook

The dataset is in long format, including one row per observation.

## Variables

* `publisher` - string variable naming the publisher
* `year` - 
* `scope` - numeric value depicting relevant emission scope for the `reported_category` (for more details see the [GHG protocol](https://ghgprotocol.org/calculationg-tools-faq)). Options: 
  * Scope 1: All direct GHG emissions.
  * Scope 2: Indirect GHG emissions from consumption of purchased electricity, heat or steam.
  * Scope 3: Other indirect emissions, such as the extraction and production of purchased materials and fuels, transport-related activities in vehicles not owned or controlled by the reporting entity, electricity-related activities (e.g. T&D losses) not covered in Scope 2, outsourced activities, waste disposal, etc.
* `reported_category` - string indicating the reported category in the original report
* `tCo2e` - tonnes carbon dioxide equivalent metric, numeric value
* `notes` - variable to contain any comments from the coding 
* `url` - contains the link to the original report, if available
* `include` - binary variable indicating inclusion for cumulative calculations (`1`) or exclusion for cumulative calculations (`0`).

## Coding details

* Collected data from the five largest publishers based on [10.1371/journal.pone.0127502](https://doi.org/10.1371/journal.pone.0127502), if available, and otherwise from their respective parent companies
  * Springer Nature
  * Elsevier -> RELX
  * Sage
  * Wiley
  * Taylor & Francis -> Informa
* Collected data from the report to the year when available (so 2011 from the 2011 report if possible).
* We focused on things reported in tCO2e, not other units such as cubic metres, tonnes, or otherwise.
* Removed cumulative numbers across scopes.
* Included location-based (grid-mix) over market-based (Renewable Energy Certificates [RECs]).