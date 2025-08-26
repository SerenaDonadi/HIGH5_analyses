---
output:
  html_document: default
  word_document: default
  pdf_document: default
---

HIGH FIVE

Aim: to develop an index, which can substitute VIX, to assess the status of rivers (small and large) focusing on
trout, both resident and migratory. A model that can predict the max/optimal density of trout (0+) based on 
habitat variables, including THS. Possibly with indication of uncertainty.

Methods: use the approach of Katarina M., i.e. find reference points for each site using time series as well as 
spatial extended data. But this time include also resident trout. Ask Katarina for the script. Develop a model 
to predict the reference points based on habitat variables, including THS.

Focus on trout 0+

Start with, hence collected the data for, one county: Jämtland. Joacim will give me the data, after filtering
out poor quality data or sits to exclude.
Norway is involved also (Nina and others), they haven’t provided the data yet. Meeting in September where we
should show something

I received the data from Joacim: all location, only Jämtland and Jämtland filetred according to the criteria listed in the document “filtering”. Keep a variable saying if the whole width has been sampled and use it as predictor. Uso all years, also the old ones. If data are not enough, we can possibly add also Dalarna data.

# Filtering of SERS data

* Only retained Quantitative electrofishing (at least two pass fising)
* Removed fishing with a site length < 18 m
* Removed fishing sites with fished area < 50 m^2

# questions 

check the meaning of different VIX variables

# notes on Katarina method:
keep only sites with at least 10 years of data for the times series analysis
keep only samplings conducted between June and September
keep sites with either one or more electrofishing passes

breakpoints are calculated using a linear and a quadratic function. then the best model is selected based on AIC.
the breakpoint needs to match some criteria, e.g. should be no less than 80% of the max density observed at the site, the CI
(uncertainty) should not be too wide. There should be at least 5 (or 8?) different unique values per site for the functions to be 
used. Sometimes there is no plateau (e.g. linear increase), or points are too far away (lots of zeros and then a very high value.
Hence breakpoints are not always found, or don't match the criteria.
-> for sites with no breakpoints, the 90th quantile is used instead
Question: for good sites (relatively high trout density) with no breakpoints, we use the 90th quantile. But for bad sites 
(null/low trout density) with no breakpoints, shall we keep the 90th quantile or discard the sites from the further analysis? 
(the one where I use covariates to predict breakpoints)
My take will be to retain all, but keep track of the reason why no breakpoints were found. That is, potentially use a covariate
indicating if the site is good or bad (e.g. max density observed, or mean density observed, or Coeff of variation), or potentially
exclude them at a later stage.



