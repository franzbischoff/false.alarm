Detecting life-threatening patterns in Point-of-care ECG using efficient
memory and processor power.
================
Francisco Bischoff
on July 22, 2020

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/franzbischoff/heads_thesis/master?urlpath=rstudio)
[![Read
Thesis](https://img.shields.io/badge/read-thesis__down-brightgreen)](https://franzbischoff.github.io/heads_thesis/)
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
accuracy is essential. It will use the Physionet datasets<sup>1</sup>,
and the algorithm will try to minimize the false negatives and false
positives.

The expected result is the concretization of a new method that, besides
being accurate, accomplishes this task using state of the art technology
for time series analysis that allows minimum space and processor power
to solve this problem. Also, we expect that fading factors can
contribute to the state of the art of this technology.

The research team is well experienced in time-series and has studied the
Matrix Profile since its beginning, being founders of the Matrix Profile
Foundation whose goal is to have a concise and stable cross-language API
for developing with the Matrix Profile technology.<sup>2,3</sup>

# About the ongoing project

The document submitted for approval is
[here](https://github.com/franzbischoff/heads_thesis/blob/master/protocol/Protocol.pdf).

To follow the thesis timeline you can access the full Gantt chart at
Zenhub. Click
[here](https://app.zenhub.com/workspaces/phd-thesis-5eb2ce34f5f30b3aed0a35af/roadmap)
(you need a github account, but that’s it).

# References

<div id="refs" class="references">

<div id="ref-Clifford2015">

1\. Clifford GD, Silva I, Moody B, et al. The PhysioNet/Computing in
Cardiology Challenge 2015: Reducing false arrhythmia alarms in the ICU.
In: *Computing in Cardiology*.; 2015.
doi:[10.1109/CIC.2015.7408639](https://doi.org/10.1109/CIC.2015.7408639)

</div>

<div id="ref-Bischoff2019a">

2\. Bischoff F, Rodrigues PP. tsmp: An R Package for Time Series with
Matrix Profile. Published online April 2019.
doi:[10.13140/RG.2.2.13040.30726](https://doi.org/10.13140/RG.2.2.13040.30726)

</div>

<div id="ref-VanBenschoten2020">

3\. Van Benschoten A, Ouyang A, Bischoff F, Marrs T. MPA: a novel
cross-language API for time series analysis. *Journal of Open Source
Software*. 2020;5(49):2179.
doi:[10.21105/joss.02179](https://doi.org/10.21105/joss.02179)

</div>

</div>
