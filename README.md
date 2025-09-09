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
# by Joacim (when extracting the data)
- Only retained Quantitative electrofishing (at least two pass fising)
- Removed fishing with a site length < 18 m
- Removed fishing sites with fished area < 50 m^2
# by Serena (included in the scripts)
- months: July to October
- I iclude all years for now


# questions 

check the meaning of different VIX variables - maybe not needed, I wouldn't use it as explanatory

# notes on Katarina method:
keep only sites with at least 10 years of data for the times series analysis
keep only samplings conducted between July and October
keep sites with either one or more electrofishing passes (to be ,ore includive, otherwise many sites in the north would be dropped)

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
indicating if the site is good or bad (e.g. mean density at the site is < mean density at subdivision), or potentially
exclude them at a later stage.

the output of the breakpoint analyses is a table with lots of parameters that keep track of the process.
It contains also the 90th quantile (q90) in case I want to use that one instead of the breakpoint (clx)
"thr_val" is the mean density of the subdivision (SD), which can be used as a reference to compare with the mean density of the 
site to determine whether the sites are "bad ones" (see above)

Katarina found that using q90 or clx (breakpoints) did not make a big difference in the time series analysis, i.e. using q90 is 
fine, but the difference was bigger in the spatial analyses (with all sites regardless of how many years of sampling, grouped
according to some variables intervals), and the clx seemed better.
My thought: this could be due to 1) the variation is larger between than within sites, 2) the grouping is suboptimal, i.e. 
other variables and/or intervals may better catch the similarities among sites, 3) the constraints used in the calculation
of the breakpoints results in values very close to the q90th

EXTRA NOTES to integrate in what I already wrote:
Here are the breakpoints calculated for all sites in Jämtland for which we have at least 10 years of data, collected between July and October.

Samples size: 1448 obs from 84 sites from 58 rivers. First year of sampling: min 1975, max 2012. Last year of sampling: min 1997, max 2024. That means that the time period can be quite different. We can always narrow it down and calculate breakpoints during the same time period, this will reduce the sample size though.

About the method:
Breakpoints are first estimated by a plateau method, including a linear and quadratic curve, which are compared are compared via AIC. The one with lowest AIC is chosen. Some criteria need to be met for the breakpoint to be approved:
 - the curve reaches required fraction of asymptote (0.80)
 - confidence interval is non-negative and reasonably tight (CI_width / estimate ≤ 0.25)
 - breakpoint lies within an inner quantile band of x (to avoid edge fits): set to 0.05 and 0.95
 - minimum number of observations (years) required before attempting model fitting/selection was set to 10
 - minimum number of distinct x-values needed to fit reliably was set to 5
If one of this criteria is not met, breakpoints are instead taken to be the 90th quantile.
A more detailed description of the method should come soon, but hopefully this is enough for you to be able to show our work, let me know if not.
The figures show cumulative frequency distributions of trout recruits in different sites. Dashed red lines represent breakpoints estimated via plateau methods, with red bands showing CI. Blue dotted lines indicate quantile 90th.

In each panel you see the estimated values of: q90 = 90th quantile, clx = breakpoint, with CI between brackets


# notes on analyses and TO DO things

Run separate models for migratory and resident or introduce an interaction with pop type for all factor (or two different gams)

Meeting Joacim 1 sept 2025:

-vandhind: assign 1 to ned, 0.5 to upp. As in most cases the spawner go upstream, so an obstacle downstream would hinder them.
Because the values are either Inga or Ned or Upp (not combined), if there were obstacles both upp and down, the fieldstuff 
likely wrote down. If only upp, they wrote upp. However, there are sites where different values are found over years. Joacim 
says thatit's unlikely that obstacles have been removed. Find a way to dealwith this, maybe count the frequencies of upp and
ned, and take the highest (?)

Answer: in each site, over years, there are 4 combinations: "Inga" alone, and "Ned, Inga", "Ned, Inga, Upp" and "Upp, Inga". 
see vandhind_table to see the relative frquencies by site. Very variables. Assuming that the obstacles situation is stable
over years, I will assign:
"Inga" alone -> 0
"Ned, Inga"-> 1
"Ned, Inga, Upp" -> 1.5
"Upp, Inga" -> 0.5

check with Joacim and revise if needed

-Migratory population: if migratory at least once over the years, set to 1, otherwise 0. We are assuming 
that the population migration startegy is stable over the years, consistently with the assumption that 
obstaclesupp och ned did not change over time

Predictive model for breakpoints: 
consider/fix all covariates included in THS. Consider interaction altitude * stream width. 
Try function dredge in mumin package. Also recursive regression trees (?). See paper attached by Joacim
Validate model on Dalarna
The idea is to predict what the ideal trout density was if there were no impacts, hence we could maybe use the best sites to make predictions, or try a quantile regression

Conbsider this!: -> for sites with no breakpoints, the 90th quantile is used instead. Question: for good sites (relatively high trout density) with no breakpoints, we use the 90th quantile. But for bad sites (null/low trout density) with no breakpoints, shall we keep the 90th quantile or discard the sites from the further analysis? (the one where I use covariates to predict breakpoints). My take will be to retain all, but keep track of the reason why no breakpoints were found. That is, potentially use a covariate indicating if the site is good or bad (e.g. mean density at the site is < mean density at subdivision), or potentially exclude them at a later stage.





