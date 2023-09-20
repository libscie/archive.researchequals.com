We imagine a world where people don't think twice about creating DOIs, and integrate changes with confidence. Further implementation of Persistent Identifiers (PIDs) is helpful for metadata reuse by various organisations like academic institutes and funders. Previous reports indicate this can save millions of dollars[^1] per year by streamlining data sources and that there is dire need for better funder metadata[^2].

Specifically for Digital Object Identifiers (DOIs) as one form of PIDs, a barrier to further implementation is the effort and investment needed to generate schema conform XML.

We propose an open source JavaScript library that auto-generates schema conform XML, allowing rapid adoption of changes to CrossRef’s DOI schema and simplify community implementations in web applications. CrossRef has already announced a new vision for their metadata, the “Research Nexus”, which creates a sense of urgency around this. Also, several community projects reinvent the wheel on how to create schema conform XML, and an open source library can pool resources and improve reliability.

## Problem: Silo's

Creating schema conform XML in JavaScript (JS) is error prone. JS does not work with XML directly and most implementations to generate XML use JSON objects. This can work, but the risk of creating non-conform XML is high, resulting in a lot of trial and error and time wasted. Anecdotally we know that multiple projects receive errors from the CrossRef system because of this (e.g., ResearchEquals, PubPub).

Each project wanting to mint DOIs in JS creates their own approach to do so, resulting in untested implementations. This is redundant. Depending on the time available to the project, the implementation may also be limited and not covering the entire schema. Relatively infrequently used parts of the schema will receive little support, resulting in little use.
Testing whether custom implementations work takes some time, which adds to development and debugging cycles. Currently every update needs to be tested by generating XML and then running it through [CrossRef’s validator](https://www.crossref.org/02publishers/parser.html) or by doing a test deposit (email results potentially not being available to the developer directly).

Every time the schema changes, each project revisits their implementation, tests, and inspects potential edge-cases. New implementations also have to go through this cycle, for each project, increasing resources required to start minting DOIs. These difficulties can impact the further implementation of DOIs and adoption of innovations in the schema. These issues reinforce previous implementations, which may suffer from the problems we describe above and can result in support time spent by CrossRef.

## Proposal: Strongly typed library

We will reduce inefficiencies along all levels by creating a strongly typed open source library for generating CrossRef XML. This means that implementations are schema conform from the start and get informative error messages in their development environment. Different projects can reuse this and jointly improve it for increased reliability and confidence.
To that end, we propose creating (1) an all-purpose XML schema converter and (2) auto-generate a library to generate schema conform XML for CrossRef based on a specific schema.

### 1. XSD Converter

We will create an XML Schema Definition (XSD) converter, which ports an XSD into strongly typed validation functions. A reliable open source XSD converter can be applied to any XML schema to produce TypeScript types and validators (e.g., for [BITS](https://jats.nlm.nih.gov/extensions/bits/), [JATS](https://jats.nlm.nih.gov), [DataCite](https://schema.datacite.org)). Current libraries are not maintained well nor are they robust enough for the purposes of a CrossRef XML generator. Additionally, the validators generated also offer the possibility of validating Crossref XML in scenarios where traditional validation methods aren't available, such as serverless environments or client-side.

### 2. CrossRef XML Generator

We use the XSD converter from (1) to generate a library of functions for generating CrossRef XML. This library will cover the entire schema and remove the need for manual testing. Generating this library can be done withindaysof an update to the schema, to ensure projects can implement the latest changes. Projects then only need to update their dependency, and test their production environment.

## Who

[Thomas Jorna](https://github.com/tefkah) from the [Center of Trial and Error](https://trialanderror.org), senior engineer on this project, previously created elements of this proposal already in use at ResearchEquals and PubPub. They also have experience writing similar tools that are used in production, such as the [parsers used](https://github.com/TrialAndErrorOrg/parsers) to convert between different file formats (e.g., `.docx`, JATS XML, `.tex`) at journals.

[Liberate Science GmbH](https://libscie.org) provides project support in the form of communications, funding acquisition and management, mentoring, community adoption strategising. This stewards development and adoption of the library. Overhead is used to support these efforts.

## Budget/time

### Lower tier (€8,000 + 15% overhead)

This includes a pilot program factoring out previous code already used in production at ResearchEquals and PubPub into a common library. This is a stepping stone towards making it easy to generate CrossRef XML with confidence.

### Middle tier (€20,000 + 15% overhead)

This tier includes basic functionality outlined in this proposal, for both the XSD converter and the CrossRef XML generator. This would start to make it easier to generate CrossRef XML with confidence.

### Highest tier (€32,000 + 15% overhead)

This includes everything from the middle tier, plus an additionally refined developer experience for libraries. It also includes a test suite to ensure stability and reliability now and into the future. This would fulfil the goal of making it easier to generate CrossRef XML with confidence.

[^1]: doi:[10.5281/zenodo.7100578](https://doi.org/10.5281/zenodo.7100578)
[^2]: doi:[10.1162/qss_a_00210](https://doi.org/doi:10.1162/qss_a_00210)
