<!-- README.md is generated from README.Rmd. Please edit that file -->

# [Resample survey data](https://github.com/dgbolser/Resample-survey-data) <img src="https://github.com/dgbolser/Resample-survey-data/blob/em/img/bigelow_nefsc_bts.JPG?raw=true" alt="Logo." align="right" width="139" height="139"/>

> This repository is under active development and all code, results, and
> other materials should be considered preliminary. Find code used for
> various versions in the
> \[releases\](<https://github.com/dgbolser/Resample-survey-data/releases>
> section for finalized products and project milestones.

## This code is primarally maintained by:

**Derek Bolser** (Derek.Bolser AT noaa.gov;
[@dgbolser](https://github.com/dgbolser))

Office of Science and Technology,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Silver Spring, MD 98195

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

**Ian Taylor** (Ian.Taylor AT noaa.gov;
[@iantaylor-NOAA](https://github.com/iantaylor-NOAA))

Northwest Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

**Elizabeth Perl** (Elizabeth.Perl AT noaa.gov;
[@e-perl-NOAA](https://github.com/e-perl-NOAA))

/Affiliate with ECS Federal in support of the Office of Science and
Technology,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Silver Spring, MD 98195

# Table of contents

> - [*Purpose*](#purpose)
> - [*User Resources*](#user-resources)
> - [*Cite data used in analysis*](#cite-data-used-in-analysis)
> - [*Suggestions and Comments*](#suggestions-and-comments)
> - [*R Version Metadata*](#r-version-metadata)
> - [*Legal*](#legal)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

# Purpose

The ‘get prediction grid’ script should be run before running other
scripts. Use the ‘pull data’ file or go to the ‘data’ folder to get the
data we are working with then run the ‘main’ file. Other scripts provide
supporting functions that are run in the ‘main’ file. Due to the high
number of species, effort scenarios, and replicates, the script is very
computationally intensive. Reduce the number of species/effort
scenarios/replicates when testing.

# User Resources

- [GitHub repository](https://github.com/dgbolser/Resample-survey-data).

- [Access Tips and Documentation for All AFSC Groundfish Assessment
  Program Production
  Data](https://afsc-gap-products.github.io/gap_products/)

- [AFSC Groundfish Assessment Program Fisheries One Stop Shop
  (FOSS)](https://www.fisheries.noaa.gov/foss)

- [Groundfish Assessment Program Bottom Trawl
  Surveys](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys)

- NWFSC links TBD

# Cite data used in analysis

NOAA Fisheries Alaska Fisheries Science Center (2024)

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-FOSSAFSCData" class="csl-entry">

NOAA Fisheries Alaska Fisheries Science Center. (2024). *Fisheries one
stop shop public data: RACE division bottom trawl survey data query*.
https://www.fisheries.noaa.gov/foss; U.S. Dep. Commer.

</div>

</div>

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/dgbolser/Resample-survey-data/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/dgbolser/Resample-survey-data/issues).

# R Version Metadata

# Legal

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
