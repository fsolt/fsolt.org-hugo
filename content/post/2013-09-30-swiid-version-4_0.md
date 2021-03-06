---
date: "2013-09-30 14:04:01"
layout: post
slug: swiid-version-4_0
tags:
- update
- swiid
title: SWIID Version 4.0 is available!
---

Version 4.0 of the SWIID is now available [here](../swiid/swiid_downloads.html).  Drawing on nearly 14,000 Gini observations in more than 3100 country-years, this version provides even better estimates of income inequality in countries around the world than in previous versions.

This version introduces two other improvements.  First, many users have had trouble making appropriate use of the standard errors associated with the SWIID estimates.  The uncertainty, however, can sometimes be substantial, making it crucial to incorporate in one&rsquo;s analyses.  Fortunately, there are now tools in Stata and R that make it quite straightforward to analyze data that is measured with error, and this version of the SWIID includes files that are pre-formatted for use with these tools.  The file "Using the SWIID.pdf", which is also included in the data download, explains how.  Some additional examples of using the SWIID with Stata's `mi estimate` command prefix can be found towards the end of the slides posted <a href="../papers/swiid_un_talk" rel="self">here</a>.<br /><br />Second, I've received several requests for measures of top income share, so in this version I am including estimates of the top 1 percent's share (the variable share1), standardized to the data provided in the <a href="http://topincomes.g-mond.parisschoolofeconomics.eu" rel="self">World Top Incomes Database</a>: Country-years included in that dataset are reproduced without modification in the SWIID, and comparable figures for other country-years are estimated using the SWIID's custom multiple-imputation algorithm.  Like all inequality datasets, Top Incomes has tradeoffs---among other things, the share of pre-tax, pre-transfer income reported on tax returns by the richest filers may not be of much theoretical interest to many investigators---but the additional estimates the SWIID provides may prove to be useful to some.

I encourage users of the SWIID to email me with their comments, questions, and suggestions.
