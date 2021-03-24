---
title: 'Detecting life-threatening patterns in Point-of-care ECG using efficient memory and processor power'
author: 'Francisco Bischoff'
date: 'Jul 2020'
institution: 'Faculdade de Medicina da Universidade do Porto'
division: 'CINTESIS'
advisor: 'Pedro Pereira Rodrigues'
# If you have more two advisors, un-silence line 7
#altadvisor: 'Your Other Advisor'
altadvisor: 'Eamonn Keogh'
department: 'Medical Investigation'
degree: 'Ph.D. in Health Data Science'
knit: bookdown::render_book
site: bookdown::bookdown_site

# This will automatically install the {remotes} package and {thesisdown}
# Change this to FALSE if you'd like to install them manually on your own.
params:
  'Install needed packages for {thesisdown}': True
  
# Remove the hashtag to specify which version of output you would like.
# Can only choose one at a time.
output:
  # thesisdown::thesis_pdf:
  thesisdown::thesis_gitbook:
#  thesisdown::thesis_word: default
#  thesisdown::thesis_epub: default
    md_extensions: -tex_math_single_backslash
    keep_tex: yes
    keep_md: yes
    fig_caption: yes
# If you are creating a PDF you'll need to write your preliminary content 
# (e.g., abstract, acknowledgements) below or use code similar to line 25-26 
# for the .RMD files. If you are NOT producing a PDF, delete or silence
# lines 25-39 in this YAML header.
abstract: ''
# If you'd rather include the preliminary content in files instead of inline
# like below, use a command like that for the abstract above.  Note that a tab 
# is needed on the line after the `|`.
acknowledgements: |
  I want to thank a few people1.
dedication: |
  You can have a dedication here if you wish1.
preface: |
  This is an example of a thesis setup to use the reed thesis document class
  (for LaTeX) and the R bookdown package, in general1.
# Specify the location of the bibliography below
bibliography: 'D:/Workspaces/RStudio/false.alarm/papers/references.bib'
link-citations: true
# Download your specific csl file and refer to it in the line below.
csl: https://raw.githubusercontent.com/citation-style-language/styles/master/american-medical-association.csl
lot: true
lof: true
# If you prefer blank lines between paragraphs, un-silence lines  40-41 (this requires package tikz)
#header-includes:
#- \usepackage{tikz}
editor_options:
  markdown:
    canonical: false
    wrap: 120
---

<!--
Above is the YAML (YAML Ain't Markup Language) header that includes a lot of metadata used to produce the document.  Be careful with spacing in this header!

If you'd prefer to not include a Dedication, for example, simply delete the section entirely, or silence them (add # before each line). 

If you have other LaTeX packages you would like to include, delete the # before header-includes and list the packages after hyphens on new lines.

If you'd like to include a comment that won't be produced in your resulting file enclose it in a block like this.

If you receive a duplicate label error after knitting, make sure to delete the index.Rmd file and then knit again.
-->



<!--
The acknowledgments, preface, dedication, and abstract are added into the PDF
version automatically by inputting them in the YAML at the top of this file.
Alternatively, you can put that content in files like 00--prelim.Rmd and
00-abstract.Rmd like done below.
-->



# Preliminary Content {.unnumbered}

## Acknowledgements {.unnumbered}

I want to thank a few people2.

## Preface {.unnumbered}

This is an example of a thesis setup to use the reed thesis document class (for LaTeX) and the R bookdown package, in
general2.

## Dedication {.unnumbered}

You can have a dedication here if you wish2.

## Abstract {-}


Currently, Point-of-Care (POC) ECG monitoring works either as plot devices or alarms for abnormal cardiac rhythms using
predefined normal trigger ranges and some rhythm analysis, which raises the problem of false alarms. In comparison,
complex 12-derivation ECG machines are not suitable to use as simple monitors and are used with strict techniques for
formal diagnostics. We aim to identify, on streaming data, life-threatening hearth electric patterns to reduce the
number of false alarms, using low CPU and memory maintaining robustness. The study design is comparable to a diagnostic
study, where high accuracy is essential. Physionet's 2015 challenge yielded very good algorithms for reducing false
alarms. However, none of the authors reported benchmarks, memory usage, robustness test, or context invariance that
could assure its implementation on real monitors to reduce alarm fatigue indeed. We expect to identify the obstacles of
detecting life-threatening ECG changes within memory, space, and CPU constraints and to reduce ECG monitor's false
alarms using the proposed methodology, and assess the feasibility of implementing the algorithm in the real world and
other settings than ICU monitors.


<!-- The {.unnumbered} option here means that the introduction will be 
"Chapter 0." You can also use {-} for no numbers on chapters.
-->

# Introduction {.unnumbered}

Currently, Point-of-Care (POC) ECG monitoring works either as plot devices or alarms for abnormal cardiac rhythms using
predefined normal trigger ranges. Modern devices also incorporate algorithms to analyze arrhythmias improving their
specificity. On the other hand, full 12-derivation ECG machines are complex, are not suited to use as simple monitors
and are used with strict techniques for formal diagnostics of hearth electric conduction pathologies. The automatic
diagnostics are derived from a complete analysis of the 12-dimension data after it is fully and well collected. Both
systems do not handle disconnected leads and patient's motions, being strictly necessary to have a good and stable
signal to allow proper diagnosis. These interferences with the data collection frequently originate false alarms
increasing both patient and staff's stress; depending on how it is measured, the rate of false alarms (overall) in ICU
is estimated at 65 to 95%[@donchin2002].

Alarm fatigue is a well-known problem that consists of a sensory overload of nurses and clinicians, resulting in
desensitization to alarms and missed alarms (the "crying wolf" situation). Patient deaths have been attributed to alarm
fatigue[@sendelbach2013]. In 1982 it was recognized the increase in alarms with "no end in sight"; studies have
demonstrated that most alarm signals have no clinical relevance and lead to clinical personnel's delayed response.
Ultimately patient deaths were reported related to inappropriate responses to alarms[@sendelbach2013].

In April of 2013, The Joint Commission[@the_jc] issued the Sentinel Event Alert[@JointCommission2013], establishing
alarm system safety as a top hospital priority in the National Patient Safety Goal. Nowadays (2021), this subject still
on their list, in fourth place of importance[@the_jc2021].

In February of 2015, the CinC/Physionet Challenge 2015 was about "Reducing False Arrhythmia Alarms in the
ICU[@Clifford2015]. The introduction article stated that it had been reported that up to 86% resulting of the alarms are
false, and this can lead to decreased staff attention and an increase in patients' delirium[@Lawless1994; @Chambrin2001;
@Parthasarathy2004].

Due to this matter's importance, this research aims to identify abnormal hearth electric patterns using streaming data,
specifically those who are life-threatening, reducing the false alarms, being a reliable signal for Intensive Care Units
to respond quickly to those situations.

# Objectives and the research question

This research aims to identify, on streaming data, abnormal hearth electric patterns, specifically those which are
life-threatening, to be a reliable signal for Intensive Care Units to respond quickly to those situations. It also may
be able to continuously analyze new data and correct itself shutting off false alarms.

As it is known, this goal is not a new problem, so the main questions to solve are: (1) Can we reduce the number of
false alarms in the ICU setting? (2) Can we accomplish this objective using a minimalist approach (low CPU, low memory)
while maintaining robustness? (3) Can this approach be used in other health domains other than ICU or ECG?

# Related Works

The CinC/Physionet Challenge 2015 produced several papers aiming to reduce false alarms on their dataset. On Table
\ref{tab:alarms} it is listed the five life-threatening alarms present in their dataset.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:alarms)Definition of the 5 alarm types used in CinC/Physionet Challenge 2015 challenge.</caption>
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

They used as score the following formula, which penalizes five times the false negatives (since we do not want to miss
any real event):

$$Score=\frac{TP+TN}{TP+TN+FP+5*FN}$$

The five-best scores in this challenge are presented on Table \ref{tab:scores}[@plesinger2015; @kalidas2015; @couto2015;
@fallet2015; @hoogantink2015].

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:scores)Challenge Results on Streaming</caption>
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

Their algorithm did a pretty good job on the Physionet test-set. However, independently of their approach to this
problem, none of the authors reported benchmarks, memory usage, robustness test, or context invariance that could assure
its implementation on real monitors to reduce alarm fatigue indeed.

There are other arrhythmias that this challenge did not assess, like atrial standstill (hyperkalemia), third-degree
atrioventricular block, and others that may be life-threatening in some settings. Pulseless electrical activity is a
frequent condition in cardiac arrest but cannot be identified without blood pressure information. This information is
usually present in ICU settings but not in other locations.

# The planned approach and methods for solving the problem

## State of the art

A literature review of the last ten years is being conducted to assess state of the art for ECG automatic processing
collecting the following points if available : (1) The memory and space used to perform the primary goal of the
algorithm (sound an alarm, for ex.). (2) The type of algorithms used to identify ECG anomalies. (3) The type of
algorithms used to identify specific diagnoses (like a flutter, hyperkalemia, and others). (4) Their performance
(accuracy, ROC, etc.)

A broad search will be conducted on Pubmed, Scopus, Google Scholar, device manuals, and other specific sources.

Keywords:

-   ECG AND monitoring AND ICU
-   ECG AND\[time series\]
-   ECG AND automatic AND interpretation

Articles published after "The PhysioNet/Computing in Cardiology Challenge 2015: Reducing False Arrhythmia Alarms in the
ICU" will also be analyzed.

## Research plan and methods

This research is being conducted using the Research Compendium principles[@compendium2019]:

1.  Stick with the convention of your peers;
2.  Keep data, methods, and output separated;
3.  Specify your computational environment as clearly as you can.

Data management is following the FAIR principle (findable, accessible, interoperable, reusable)[@wilkinson2016].

Currently, the dataset used is stored on a public repository[@franz_dataset], the source code is publicly open and
stored on Github[@franz_github], while the reports and reproducibility information on each step is found on a public
website[@franz_website].

### Type of study

This thesis will be a diagnostic study as the algorithm must classify the change in pattern as positive or negative for
life-threatening.

### The data

Initially we will use the CinC/Physionet Challenge 2015 dataset that is publicly available on Physionet. This dataset is
a good start for exploring the main goal of reduce false alarms. This dataset was manually selected for this challenge
and the events were labeled by experts, so it is not RAW data. All signals have been resampled (using anti-alias
filters) to 12 bit, 250 Hz and have had FIR bandpass \[0.05 to 40Hz\] and mains notch filters applied to remove noise.
Pacemaker and other artifacts may be present on the ECG[@Clifford2015]. Furthermore, this dataset contains at least two
ECG derivations and one or more variables like arterial blood pressure, photoplethysmograph readings, and respiration
movements.

These variables may or may not be helpful for increasing the sensitivity or specificity of the algorithm. It is planned
to use the minimum set of variables as it is known in multi-dimensional analysis that using just two (or some small
subset) of all the dimensions can be much more accurate than either using all dimensions or a single
dimension[@gharghabi2018].

It is desirable that real data extracted from Portuguese ICU could be used in further stage to assess the validity of
the model in real settings and robustness (using RAW data instead of filtered data). The variables available on
Physionet's dataset are commonly available on Portuguese ICU's.

### Workflow

All steps of the process will be managed using the R package `targets`[@landau2021] from data extraction to the final
report, as shown in Fig. \ref{fig:targets}.



The report will then be available on the main webpage[@franz_website], allowing inspection of previous versions managed
by the R package `workflowr`[@workflowr2021], as we can see in Fig. \ref{fig:workflow_workflowr}.



### Statistical analysis

The Statistical analysis will be performed using R language v4.0.4 or greater and it will be computed the ROC curve for
the algorithm.

The experiment will be conducted using the Matrix Profile concept[@yeh2016], the state-of-the-art for time series
analysis. It will be conducted several experiments to identify the best algorithm for this task. One of such algorithms
is the online semantic segmentation for multi-dimensional time series[@gharghabi2018].

In addition, we will combine the fading factors[@Gama2013; @Rodrigues2010] strategy to minimize the memory and space
consumption lowering the processor power needed, allowing this algorithm to be used in almost any device.

### Research Team

-   Thesis Author: Francisco Bischoff
-   Supervisor: Professor Pedro Pereira Rodrigues
-   Co-supervisor: Professor Eamonn Keogh (UCR, Riverside)

## Expected results and outcomes

We expect the following results: (1) Identify the obstacles of identifying life-threatening ECG changes within memory,
space, and CPU constraints. (2) Be able to reduce ECG monitor's false alarms using the proposed methodology. (3) Assess
the feasibility of implementing the algorithm in the real world and other settings than ICU monitors.

And outcomes: (1) To achieve a better score of false alarm reduction than the best on Physionet's 2015 challenge. (2) To
push forward the state-of-the-art technology on false alarms reduction, maybe even being domain agnostic. (3) To draw
more attention to fading factors as a reliable, fast, and cheap approximation of the true value. (4) To draw more
attention to the matrix profile concept as an efficient, agnostic, and almost parameter-free way to analyze time series.
(5) To draw more attention of the Patient Monitorization industry on solving the false alarm problem.

# Whatever

**Research question and aims**

This research aims to identify, on streaming data, abnormal hearth electric patterns, specifically those who are life-threatening, in order to be a reliable signal for Intensive Care Units to respond quickly to those situations. It also may be able to continuously analyze new data and correct itself shutting off false alarms.
Primarily an experiment will be conducted using 2 main algorithms that use Matrix Profile in detecting context changes: SDTD and FLOSS. One uses whole data training and testing, and the other uses a streaming approach that is our main interest. The goal will be detecting the transition from normal to flutter/FA to normal condition with special attention to not rely on rhythm changes.
Being this successful, a more generalistic approach will be attempted: to detect changes from normal to abnormal to normal conditions, with special attention to handle with disconnected leads or patient movements.
Finally, this research can prove to be a good addition to the Matrix Profile method, using fading factors in order to reduce memory and space consumption, lowering the processor power needed, allowing this algorithm to be used in almost any device.


**About the ongoing project**

The document submitted for approval is [here](https://github.com/franzbischoff/false.alarm/blob/master/protocol/Protocol.pdf).

The full code is open-sourced and available [here](https://github.com/franzbischoff/false.alarm/)

To follow the thesis timeline you can access the full Gantt chart at Zenhub. Click [here](https://app.zenhub.com/workspaces/phd-thesis-5eb2ce34f5f30b3aed0a35af/roadmap) (you need a github account, but that's it).

PDF, EPUB and WORD versions will be available at the end of this work.

<!--
If you're still on the fence about using _R Markdown_, check out the resource for newbies available at <https://ismayc.github.io/rbasics-book/> or email us at <data@reed.edu>.
-->

<!--
Having your code and commentary all together in one place has a plethora of benefits!
-->

<!--chapter:end:index.Rmd-->

---
editor_options:
  markdown:
    canonical: false
    wrap: 120
---

<!--
The bib chunk below must go last in this document according to how R Markdown renders.  More info is at http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html
-->

\backmatter

<!-- 
If you'd like to change the name of the bibliography to something else,
delete "References" and replace it.
-->

# References {-}
<!--
This manually sets the header for this unnumbered chapter.
-->
\markboth{References}{References}
<!--
To remove the indentation of the first entry.
-->
\noindent

<!--
To create a hanging indent and spacing between entries.  These three lines may need to be removed for styles that don't require the hanging indent.
-->

\setlength{\parindent}{-0.20in}
\setlength{\leftskip}{0.20in}
\setlength{\parskip}{8pt}

<!--chapter:end:99-references.Rmd-->

