---
title: "False Alarm"
date: "2021-10-21"
author: Francisco Bischoff
editor_options:
  markdown:
    canonical: false
output: revealjs::revealjs_presentation
bibliography: '/home/franz/workspace/false.alarm/papers/references.bib'
csl: csusbe.csl
css: style.css
# rstudioapi::viewer("http://localhost:8787/files/workspace/false.alarm/presentations/MEDCIDS21/MEDCIDS21.html#/")
---



# Introduction

<div class="figure" style="text-align: center">
<img src="figures/pocmonitor.jpg" alt="Fig. 1:  POC Monitor."  />
<p class="caption">Fig. 1:  POC Monitor.</p>
</div>

::: notes
<div style="font-size:0.9em">
-   POC devices works "either" as plot or alarms using predefined normal trigger ranges
-   Modern devices also incorporate algorithms to analyze arrhythmias improving their specificity
</div>
:::

------------------------------------------------------------------------------------------------------------------------

<div class="figure" style="text-align: center">
<img src="figures/ecg12deriv.jpg" alt="Fig. 2:  12-derivation ECG."  />
<p class="caption">Fig. 2:  12-derivation ECG.</p>
</div>

\
False alarms increases both patient and staff's stress; depending on how it is measured, the rate of false alarms
(overall) in ICU is estimated at 65 to 95% [@donchin2002].

::: notes
<div style="font-size:0.9em">
12-derivation ECG machines are complex

-   Both systems do not handle disconnected leads and patient's motions, being strictly necessary to have a good and
    stable signal to allow proper diagnosis.
</div>
:::

# Objectives and the research question

**AIM:** to identify, on streaming data, abnormal hearth electric patterns, specifically those which are
life-threatening, to be a reliable signal for Intensive Care Units to respond quickly to those situations.

**If possible:** It also may be able to continuously analyze new data and correct itself shutting off false alarms.

Questions:

1.  Can we reduce the number of false alarms in the ICU setting?

2.  Can we accomplish this objective using a minimalist approach (low CPU, low memory) while maintaining robustness?

3.  Can this approach be used in other health domains other than ICU or ECG?

# Related Works

The CinC/Physionet Challenge 2015 produced several papers aiming to reduce false alarms on their dataset.\

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Table  1 - Definition of the 5 alarm types used in CinC/Physionet Challenge 2015 challenge.</caption>
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;"> Alarm </th>
   <th style="text-align:left;font-weight: bold;"> Definition </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 5cm; "> Asystole </td>
   <td style="text-align:left;"> No QRS for at least 4 seconds </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; "> Extreme Bradycardia </td>
   <td style="text-align:left;"> Heart rate lower than 40 bpm for 5 consecutive beats </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; "> Extreme Tachycardia </td>
   <td style="text-align:left;"> Heart rate higher than 140 bpm for 17 consecutive beats </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; "> Ventricular Tachycardia </td>
   <td style="text-align:left;"> 5 or more ventricular beats with heart rate higher than 100 bpm </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; "> Ventricular Flutter/Fibrillation </td>
   <td style="text-align:left;"> Fibrillatory, flutter, or oscillatory waveform for at least 4 seconds </td>
  </tr>
</tbody>
</table>

::: notes
<div style="font-size:0.9em">
-   In Table 1 it is listed the five life-threatening alarms present in their dataset.
</div>
:::

------------------------------------------------------------------------------------------------------------------------

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Table  2 - Challenge Results on Streaming</caption>
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;"> Score </th>
   <th style="text-align:left;font-weight: bold;"> Authors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 81.39 </td>
   <td style="text-align:left;width: 9cm; "> Filip Plesinger, Petr Klimes, Josef Halamek, Pavel Jurak </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 79.44 </td>
   <td style="text-align:left;width: 9cm; "> Vignesh Kalidas </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 79.02 </td>
   <td style="text-align:left;width: 9cm; "> Paula Couto, Ruben Ramalho, Rui Rodrigues </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 76.11 </td>
   <td style="text-align:left;width: 9cm; "> Sibylle Fallet, Sasan Yazdani, Jean-Marc Vesin </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 75.55 </td>
   <td style="text-align:left;width: 9cm; "> Christoph Hoog Antink, Steffen Leonhardt </td>
  </tr>
</tbody>
</table>

::: notes
<div style="font-size:0.9em">
-   In Table 3 it is listed the top-5 Challenge Results on Streaming.
</div>
:::

# Research plan and methods

This research is being conducted using the Research Compendium principles[@compendium2019]:

<div class="figure" style="text-align: center">
<img src="figures/compendium_principles.png" alt="Fig. 3:  Research Compendium principles."  />
<p class="caption">Fig. 3:  Research Compendium principles.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

Data management follows the FAIR principle (findable, accessible, interoperable, reusable)[@wilkinson2016].

<div class="figure" style="text-align: center">
<img src="figures/compendium_principles2.png" alt="Fig. 4:  FAIR principle (findable, accessible, interoperable, reusable)."  />
<p class="caption">Fig. 4:  FAIR principle (findable, accessible, interoperable, reusable).</p>
</div>

------------------------------------------------------------------------------------------------------------------------

All steps of the process will be managed using the R package `targets`[@landau2021] from data extraction to the final
report, as shown in Fig. 5.

<div class="figure" style="text-align: center">
<img src="figures/targets.png" alt="Fig. 5:  Reprodutible research workflow using `targets`."  />
<p class="caption">Fig. 5:  Reprodutible research workflow using `targets`.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

<div class="figure" style="text-align: center">
<img src="figures/workflowr_print.png" alt="Fig. 6:  Reprodutible research workflow using `workflowr`."  />
<p class="caption">Fig. 6:  Reprodutible research workflow using `workflowr`.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

<div class="figure" style="text-align: center">
<img src="figures/session_information.png" alt="Fig. 7:  Reprodutible research workflow using `workflowr`."  />
<p class="caption">Fig. 7:  Reprodutible research workflow using `workflowr`.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

<div class="figure" style="text-align: center">
<img src="figures/badges.png" alt="Fig. 8:  Ready to code, versioned, code quality checked."  />
<p class="caption">Fig. 8:  Ready to code, versioned, code quality checked.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

The dataset is publicly available at Zenodo: ![](figures/zenodo.doi.svg){style="height:1em;margin: 0 0 -0.2em 0;"}

<div class="figure" style="text-align: center">
<img src="figures/zenodo.png" alt="Fig. 9:  FAIR principle (findable, accessible, interoperable, reusable)." width="60%" />
<p class="caption">Fig. 9:  FAIR principle (findable, accessible, interoperable, reusable).</p>
</div>

::: notes
<div style="font-size:0.9em">
Currently, the dataset used is stored on a public repository[@franz_dataset], the source code is publicly open and
stored on Github[@franz_github], while the reports and reproducibility information on each step is found on a public
website[@franz_website]. The current dataset and further collected data will be publicly available following the FAIR
principle.
</div>
:::

## Proposed approach

The proposed approach is depicted in Fig. 10:

<div class="figure" style="text-align: center">
<img src="figures/false_alarm.svg" alt="Fig. 10:  Proposed approach to train the model for relevant patterns detection."  />
<p class="caption">Fig. 10:  Proposed approach to train the model for relevant patterns detection.</p>
</div>

::: notes
<div style="font-size:0.9em">
-   That is only a draft of the final workflow.
-   The algorithm for the classification of the regime changes is still to be defined.
-   However, the main innovation resides in the correct regime detection.
-   Also, to achieve the goal of low CPU and memory usage, the strategy will be to combine fading factors[@Gama2013;
    @Rodrigues2010] to reduce computation in online settings like in this research.
</div>
:::

# Preliminary Experimentations

## Raw Data

While programming the pipeline for the current dataset, it has been acquired a Single Lead Heart Rate Monitor breakout
from Sparkfun^TM^ [@sparkfun2021] using the AD8232 [@AnalogDevices2020] microchip from Analog Devices Inc., compatible
with Arduino^(R)^ [@arduino2021], for an in-house experiment. Fig. 11.

<div class="figure" style="text-align: center">
<img src="figures/sparkfun2.jpg" alt="Fig. 11:  Single Lead Heart Rate Monitor"  />
<p class="caption">Fig. 11:  Single Lead Heart Rate Monitor</p>
</div>

------------------------------------------------------------------------------------------------------------------------

The output gives us a raw signal as shown in Fig. 12.

<div class="figure" style="text-align: center">
<img src="figures/arduino_plot.jpg" alt="Fig. 12:  raw output from Arduino at ~300Hz"  />
<p class="caption">Fig. 12:  raw output from Arduino at ~300Hz</p>
</div>

After applying the same settings as the Physionet database (collecting the data at 500Hz, resample to 250Hz,
pass-filter, and notch filter), the signal is much better as shown in Fig. 13.

<div class="figure" style="text-align: center">
<img src="figures/filtered_ecg.png" alt="Fig. 13:  Gray is raw, Red is filtered"  />
<p class="caption">Fig. 13:  Gray is raw, Red is filtered</p>
</div>

## Data quality

At the same time, the ECG data needs to be "cleaned" for proper evaluation. That is different from the initial filtering
process. Several SQIs (Signal Quality Indexes) are used on literature [@eerikainen2015], some trivial measures as
*kurtosis*, *skewness*, median local noise level, other more complex as pcaSQI (the ratio of the sum of the five largest
eigenvalues associated with the principal components over the sum of all eigenvalues obtained by principal component
analysis applied to the time aligned ECG segments in the window). By experimentation (yet to be validated), a simple
formula gives us the "complexity" of the signal and correlates well with the noisy data is shown in Equation
$\eqref{complex}$.

$$
\sqrt{\sum_{i=1}^w((x_{i+1}-x_i)^2)}, \quad \text{where}\; w \; \text{is the window size} \tag{1} \label{complex}
$$

------------------------------------------------------------------------------------------------------------------------

The Fig. 14 shows some SQIs.

<div class="figure" style="text-align: center">
<img src="figures/noise.png" alt="Fig. 14:  Green line is the &quot;complexity&quot; of the signal"  />
<p class="caption">Fig. 14:  Green line is the "complexity" of the signal</p>
</div>

------------------------------------------------------------------------------------------------------------------------

Fig. 15 shows that noisy data (probably patient muscle movements) are marked with a blue point
and thus are ignored by the algorithm. Also, valid for the following plots, the green and red lines on the data mark the
10 seconds window where the "event" that triggers the alarm is supposed to happen.

<div class="figure" style="text-align: center">
<img src="figures/regime_filter.png" alt="Fig. 15:  Regime changes with noisy data - false alarm"  />
<p class="caption">Fig. 15:  Regime changes with noisy data - false alarm</p>
</div>

------------------------------------------------------------------------------------------------------------------------

In Fig. 16, the data is clean; thus, nothing is excluded. Interestingly one of the detected
regime changes is inside the "green-red" window. But it is a false alarm.

<div class="figure" style="text-align: center">
<img src="figures/regime_false.png" alt="Fig. 16:  Regime changes with good data - false alarm"  />
<p class="caption">Fig. 16:  Regime changes with good data - false alarm</p>
</div>

------------------------------------------------------------------------------------------------------------------------

The last plot (Fig. 17) shows the algorithm's robustness, not excluding good data with a
wandering baseline, and the last regime change is correctly detected inside the "green-red" window.

<div class="figure" style="text-align: center">
<img src="figures/regime_true.png" alt="Fig. 17:  Regime changes with good but wandering data - true alarm"  />
<p class="caption">Fig. 17:  Regime changes with good but wandering data - true alarm</p>
</div>

# Detecting Regime Changes

Briefly describing the regime detection algorithm, which can be explored in the original paper [@gharghabi2018], it is
based on the assumption that between two regimes, the most similar shape (its nearest neighbor) is located on "the same
side". This information is obtained from the Matrix Profile computation. More precisely, using only the Profile Index.

<div class="figure" style="text-align: center">
<img src="figures/arcs_fluss_original.png" alt="Fig. 18:  FLUSS algorithm, using arc counts."  />
<p class="caption">Fig. 18:  FLUSS algorithm, using arc counts.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

Claims about the algorithm:

-   **Domain Agnosticism:** the algorithm makes no assumptions about the data as opposed to most available algorithms to
    date.
-   **Streaming:** the algorithm can provide real-time information.
-   **Real-World Data Suitability:** the objective is not to *explain* all the data. Therefore, areas marked as "don't
    know" areas are acceptable.
-   **FLUSS/FLOSS is not:** a change point detection algorithm [@aminikhanghahi2016]. The interest here is changes in
    the shapes of a sequence of measurements.

# Current state

One example:

<div class="figure" style="text-align: center">
<img src="figures/graph_regime_0.3_0_2500_0_200.svg" alt="Fig. 19:  Regime change detection example."  />
<p class="caption">Fig. 19:  Regime change detection example.</p>
</div>

------------------------------------------------------------------------------------------------------------------------

The Online algorithm (FLOSS) considers only the arcs pointing forward.

One change to the algorithm is the **temporal constraint** that allows to detect repeated and short-time regimes:

<div class="figure" style="text-align: center">
<img src="figures/forward.svg" alt="Fig. 20:  FLOSS with temporal constraint."  />
<p class="caption">Fig. 20:  FLOSS with temporal constraint.</p>
</div>

## Some findings

In the original paper, in chapter 3.5, the authors of FLOSS wisely introduce the **temporal constraint**.

Nevertheless,

-   Should we use the already computed Indexes or recompute the Matrix Profile using this constraint?
-   The authors declare the correction curve typically used on FLUSS and FLOSS as "simply a uniform distribution", but
    this is not an accurate statement.
-   The original paper discards the datapoints within $(ProfileSize - constraint) \cdots ProfileSize$.

The solution for evaluating the effect of using time constraints in this work's setting was to generate the ideal
distribution using the constrained parameters beforehand. That gives us enough data to evaluate a regime change
accurately utilizing a minimum of $2 \times WindowSize$ datapoints. The best index is still to be determined, and
current tests are using 3 seconds limit.

::: notes
<div style="font-size:0.9em">
That is important because the output of the FLOSS algorithm should be normalized and constrained between 0 and 1, which
allows us to compare different trials using different parameters in the process. Finally, the last datapoints are
**not** irrelevant, opposed to what was stated by the authors, since an *Online* algorithm needs to return an answer as
soon as the application domain requires. That is very much relevant to this work's field, as, for example, for asystole
detection, we have a window of 4 seconds to fire the alarm. If the time constraint is 10 seconds, this would mean (by
the original article) that the last 10 seconds of the incoming data would not be sufficient to detect the regime change.
</div>
:::

## Temporal constraint

By experimenting, we see that the original paper set the temporal constraint in the Matrix Profile algorithm (as it
seems more appropriate too). That reduces the computation time of the online Matrix Profile, and any post-processing
done afterward will inherit this constraint. The distribution for correcting the FLOSS algorithm is also simpler. Fig.
21.



<div class="figure" style="text-align: center">
<img src="MEDCIDS21-10min_files/figure-revealjs/distributions-1.svg" alt="Fig. 21:  1D-IAC distributions for earlier temporal constraint (on Matrix Profile)"  />
<p class="caption">Fig. 21:  1D-IAC distributions for earlier temporal constraint (on Matrix Profile)</p>
</div>

------------------------------------------------------------------------------------------------------------------------

On the other hand, it is possible to apply the time constraint in the FLOSS algorithm, leaving the online Matrix Profile
in its original form. See Fig. 22. The theoretical distribution changes significantly according
to the constraint value.

<div class="figure" style="text-align: center">
<img src="MEDCIDS21-10min_files/figure-revealjs/floss_dist-1.svg" alt="Fig. 22:  1D-IAC distributions for later temporal constraint (on FLOSS)"  />
<p class="caption">Fig. 22:  1D-IAC distributions for later temporal constraint (on FLOSS)</p>
</div>

::: notes
<div style="font-size:0.9em">
The upside of this approach, at least during the prospective phase, is to allow us to decide the time constraint value
later in the pipeline, avoiding the recomputation of the Matrix Profile. The results on detecting regime changes are
very similar to the first approach.
</div>
:::

------------------------------------------------------------------------------------------------------------------------

The results on detecting regime changes are very similar in both cases. Fig. 23.



<div class="figure" style="text-align: center">
<img src="MEDCIDS21-10min_files/figure-revealjs/constraints-1.svg" alt="Fig. 23:  CAC and Regime detection using early and later IAC"  />
<p class="caption">Fig. 23:  CAC and Regime detection using early and later IAC</p>
</div>

# One more contribution

The *Similarity Threshold* (ST). 

The ST is an interesting factor that we can use, especially when detecting pattern changes during time. The FLUSS/FLOSS
algorithms rely on counting references between indexes in the time series. ST can help remove "noise" from these
references since only similar patterns above a certain threshold are referenced, and changes have more impact on these
counts. More information and visual content on ST will be provided later. The best ST threshold is still to be
determined.

<div class="figure" style="text-align: center">
<img src="figures/similarity.png" alt="Fig. 24:  Example of similar patterns." width="50%" />
<p class="caption">Fig. 24:  Example of similar patterns.</p>
</div>

# Further Steps

-   Explanation on the evaluation method for regime change detection, as described in the FLOSS paper [@gharghabi2018]
-   Extraction of regime samples and classification of `TRUE` or `FALSE` for Asystole
-   Cross-validation and evaluation of the algorithm
-   Publish results

Later:

-   Extend the classification algorithm to the five life-threatening patterns
-   Combine fading factors[@Gama2013; @Rodrigues2010] to reduce computation requirements
-   Implement the final model on a low-power CPU, e.g.: Raspberry Pi (ARM), Arduino (atmega) or NodeMCU (ESP12)


# References

::: {#refs}
:::
