
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The Backlash Effect of State Coercion: Protest Resilience Under Costly and Targeted Repression

[Francisca Castro](https://francisca-castro.com/) • Institute of Social
Sciences • Humboldt-Universität zu Berlin  

------------------------------------------------------------------------

## Abstract

The impact of state repression on protests is puzzling. While in some
cases repression can deter street mobilization, other times it
backfires, increasing protest occurrence. It is not yet entirely clear
repressive actions are associated with deterrence or incitement of
protest activity, and why. Using novel data on protest and repression in
Chile, I study the effect of repressive actions on the occurrence of
protest events. Through models that account for special dynamics and
lagged effects, I find that police beatings increase subsequent protest
activity. Conversely, other repressive acts, such as the use of water
cannons or arrests, neither incite nor discourage protests. I argue that
repression backfires when it is visible and targeted: when is visible,
costs are perceived and, when directed at individuals rather than a
group of protesters, repression may be deemed inappropriate and even
unjustified. These results help to understand why repression backfires,
and also question whether repression is effective in reducing
contentious activities in democratic contexts.

------------------------------------------------------------------------

## How to download and replicate

You can download the data for replication
[here](https://github.com/frcastrog/police-repression/tree/main/01-data/01-raw-data).
In this folder, you will find the raw data used in this study:

- Data on human rights violations provided by the National Institute of
  Human Rights

- Data on protest occurrence provided by COES

- Number of police working in crowd control during the protest period,
  provided by the Chilean Police, Carabineros, official statistics,
  requested through Transparency Law.

The script `01-data-prep` contains the code to make the necessary
modifications to the original data, which includes recoding, merging of
data, and clean up.

The script `02-analysis` contains the code to replicate the figures and
tables of both the manuscript and the supplementary information.
