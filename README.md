Detecting life-threatening patterns in Point-of-care ECG using efficient
memory and processor power.
================
Francisco Bischoff
on March 04, 2021

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

![Binder](https://github.com/franzbischoff/false.alarm/workflows/Binder/badge.svg)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/franzbischoff/false.alarm/master?urlpath=rstudio)
[![Read
Thesis](https://img.shields.io/badge/read-thesis__down-brightgreen)](https://franzbischoff.github.io/false.alarm/)

<!-- badges: end -->

# Abstract

Currently, Point-of-Care (POC) ECG monitoring works either as plot
devices or alarms for abnormal cardiac rhythms using predefined normal
trigger ranges. On the other hand, full 12-derivation ECG machines are
complex to use as simple monitors and are used with strict techniques
for formal diagnostics of hearth electric conduction pathologies, and
the automatic diagnostics are derived from a full analysis of the
12-dimension data after it is fully collected. Both systems do not
handle disconnected leads and patient’s motions, being strictly
necessary to have a good and stable signal to allow proper diagnosis.

This research aims to identify abnormal hearth electric patterns using
streaming data, specifically those who are life-threatening, being a
reliable signal for Intensive Care Units to respond quickly to those
situations.

The study design is comparable to a Diagnostic study, where high
accuracy is essential. It will use the Physionet
datasets<sup>[1](#ref-Clifford2015)</sup>, and the algorithm will try to
minimize the false negatives and false positives.

The expected result is the concretization of a new method that, besides
being accurate, accomplishes this task using state of the art technology
for time series analysis that allows minimum space and processor power
to solve this problem. Also, we expect that fading factors can
contribute to the state of the art of this technology.

The research team is well experienced in time-series and has studied the
Matrix Profile since its beginning, being founders of the Matrix Profile
Foundation whose goal is to have a concise and stable cross-language API
for developing with the Matrix Profile
technology.<sup>[2](#ref-Bischoff2019a),[3](#ref-VanBenschoten2020)</sup>

# About the ongoing project

The document submitted for approval is
[here](https://github.com/franzbischoff/false.alarm/blob/master/protocol/Protocol.pdf).

To follow the thesis timeline you can access the full Gantt chart at
Zenhub. Click
[here](https://app.zenhub.com/workspaces/phd-thesis-5eb2ce34f5f30b3aed0a35af/roadmap)
(you need a github account, but that’s it).

# Reproducible Research<sup>[4](#ref-krystalli_2019)</sup>

This thesis will follow the compendium principles:

![](https://annakrystalli.me/rrresearch/assets/reproducible-data-analysis-04.png)

![](https://annakrystalli.me/rrresearch/assets/reproducible-data-analysis-06.png)

## Following Standards

Aiming to create secure materials that are
[FAIR](https://www.nature.com/articles/sdata201618) *findable,
accessible, interoperable, reusable*

### Research Data Management

-   [**RDM
    checklist**](http://www.dcc.ac.uk/sites/default/files/documents/resource/DMP/DMP_Checklist_2013.pdf)<sup>[5](#ref-dcc_2013)</sup>
-   Anticipate **data products** as part of your thesis **outputs**
-   Think about what technologies to use

### Missing values are a fact of life

-   Usually, best solution is to **leave blank**
-   **`NA`** or **`NULL`** are also good options
-   **NEVER use `0`**. Avoid numbers like **`-999`**
-   Don’t make up your own code for missing values

### Raw data are sacrosanct

-   Don’t, not even with a barge pole, not for one second, touch or
    otherwise edit the raw data files. Do any manipulations in script

### Three principles for good (file) names

#### Machine readable

-   Regular expression and globbing friendly

    -   Avoid spaces, punctuation, accented characters, case sensitivity

-   Easy to compute on

    -   Deliberate use of delimiters

    -   Deliberate use of `"-"` and `"_"` allows recovery of metadata
        from the filenames:

        -   `"_"` underscore used to delimit units of metadata I want to
            access later

        -   `"-"` hyphen used to delimit words so our eyes don’t bleed

#### Human readable

-   Borrowing the concept from
    [slugs](https://en.wikipedia.org/wiki/Clean_URL#Slug) from semantic
    URLs

#### Play well with default ordering

-   Put something numeric first

-   Use the ISO 8601 standard for dates

-   Left pad other numbers with zeros

# License

<center>

[![Creative Commons
License](https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

</center>

This work is licensed under a [Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-nc-sa/4.0/).

# Package dependencies

<center>

![](man/figures/dependency_plot-1.png)<!-- -->

</center>

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-Clifford2015" class="csl-entry">

<span class="csl-left-margin">1. </span><span
class="csl-right-inline">Clifford GD, Silva I, Moody B, et al. The
PhysioNet/computing in cardiology challenge 2015: Reducing false
arrhythmia alarms in the ICU. In: *Computing in Cardiology*.; 2015.
doi:[10.1109/cic.2015.7408639](https://doi.org/10.1109/cic.2015.7408639)</span>

</div>

<div id="ref-Bischoff2019a" class="csl-entry">

<span class="csl-left-margin">2. </span><span
class="csl-right-inline">Bischoff F, Rodrigues PP. Tsmp: An r package
for time series with matrix profile. Published online April 2019.
doi:[10.13140/rg.2.2.13040.30726](https://doi.org/10.13140/rg.2.2.13040.30726)</span>

</div>

<div id="ref-VanBenschoten2020" class="csl-entry">

<span class="csl-left-margin">3. </span><span
class="csl-right-inline">Van Benschoten A, Ouyang A, Bischoff F, Marrs
T. MPA: A novel cross-language API for time series analysis. *Journal of
Open Source Software*. 2020;5(49):2179.
doi:[10.21105/joss.02179](https://doi.org/10.21105/joss.02179)</span>

</div>

<div id="ref-krystalli_2019" class="csl-entry">

<span class="csl-left-margin">4. </span><span
class="csl-right-inline">Krystalli A. *R for Reproducible Research*.
Published online 2019. <https://annakrystalli.me/rrresearch/></span>

</div>

<div id="ref-dcc_2013" class="csl-entry">

<span class="csl-left-margin">5. </span><span
class="csl-right-inline">Centre EDC. Checklist for a data management
plan. v.4.0. Published 2013.
<http://www.dcc.ac.uk/resources/data-management-plans></span>

</div>

</div>
