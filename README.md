# climate-insurance-claims-analysis
Statistical analysis of weather-related property insurance claims using Poisson, Negative Binomial, and Zero-Inflated models in R to assess the impact of climate change on claim frequencies.

# Assessing the Impact of Climate Change on Insurance Claims

## Overview
This project investigates how climate change influences property insurance claims by applying advanced count regression models to weather-related data from Greece (2012–2022). The focus is on handling overdispersion and zero inflation, common in real-world insurance datasets.

## Objectives
- Explore and visualize claim frequency and exposure data.
- Compare Poisson, Negative Binomial (NB), Zero-Inflated Poisson (ZIP), and Zero-Inflated Negative Binomial (ZINB) models.
- Identify key predictors such as elevation and sub-item type affecting claim counts.
- Evaluate model performance using AIC, BIC, and diagnostic checks.

## Methods
- **Language:** R  
- **Techniques:** Poisson Regression, Negative Binomial Regression, Zero-Inflated Models  
- **Packages:** `glm`, `MASS`, `gamlss`  
- **Variable Selection:** Forward, backward, and stepwise AIC/BIC-based selection.

## Key Findings
- Overdispersion and excess zeros were observed, indicating standard Poisson models were unsuitable.
- The **Negative Binomial model** offered the best balance of fit and simplicity.
- Elevation and sub-item type were significant predictors of claim frequency.

## Results 
- Model comparison metrics: AIC and BIC tables included in `results/model_summary.txt`.

## Practical Implications
These findings can assist insurers in better understanding and pricing climate-related risks in property portfolios.

## Author
**Kesia Eldo**  
BSc (Hons) Data Science, Heriot- Watt University, Edinburgh
*This research formed part of my undergraduate dissertation project.*


