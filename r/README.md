---
editor_options: 
  markdown: 
    wrap: 72
---

# R sample — Compulsory voting & vote choice (Brazil)

`CESOP_analysis.R`

Author: Ziqian (Leah) Liu

In this R script, I analyzed Brazilian individual-level survey data to
estimate how *compulsory voting* affects turnout and vote choice, using
a design that compares respondents just below and just above Brazil's
compulsory-voting age cutoffs (17–18 vs. 69–70).

**What it shows:** end-to-end empirical analysis in R — combining survey
datasets, constructing variables, difference-in-proportions tests, and a
series of regression models with interaction terms and demographic
controls, robust standard errors (`estimatr`), and publication-quality
tables (`stargazer`, `xtable`).

Source survey data and one project helper (`PermutationTest.R`) are not
included, so the script is meant to be read rather than run. Paths are
relative — point them at your own data directory to run. Written by me
(Leah Liu) as part of a larger research project.

Dependencies: `xtable`, `stargazer`, `estimatr`.
