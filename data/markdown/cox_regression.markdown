**Title:** Cox Proportional Hazard Ratio Models

**1. Univariable Cox Proportional Hazard Model:** 

In this case, a Cox Proportional Hazard model for each combination of dataset and feature is created, using the `coxph` function of the R package `survival` (Thermeau et al., 2020).

To correct for multiple testing, Benjamini-Hochberg method is computed on p-values from all the models of the same dataset. This step uses the `p.adjust` function from the R package `stats` (R Core Team, 2019).

In this analysis, the Hazard Ratio coefficients are independent, ie., the computed values are not changed if you add or delete features.

<u>Example</u>

* User Selection:

    *Survival endpoint*: Overall Survival (OS)

    *Datasets selected*: Gide 2019, Van Allen 2015

    *Features selected*: Wound Healing, IFN-gamma signature

*  Models created:

  For samples from Gide 2019: 

    Model1. OS ~ Wound Healing 

    Model2. OS ~ IFN-gamma signature

  For samples from Van Allen 2015: 

    Model3. OS ~ Wound Healing 

    Model4. OS ~ IFN-gamma signature


* Benjamini-Hochberg False Discovery Rate correction:

    p-values from Model1 and Model2 -> p.adjust()

    p-values from Model3 and Model4 -> p.adjust()

---

**2. Multivariable Cox Proportional Hazard Model:** 
	
In this option, it is computed, for each dataset, a Cox Proportional Hazard model of time to survival outcome considering all the selected features as predictors. As a consequence, the inclusion or exclusion of features can change the computed values for coefficients in a model. This analysis also uses the `coxph` function of the R package `survival` (Thermeau et al., 2020).

<u>Example</u>

* User Selection:

    *Survival endpoint*: Overall Survival (OS)

    *Datasets selected*: Gide 2019, Van Allen 2015

    *Features selected*: Wound Healing, IFN-gamma signature

* Models created:

    For samples from Gide 2019: OS ~ Wound Healing + IFN-gamma signature
  
    For samples from Van Allen 2015: OS ~ Wound Healing + IFN-gamma signature
	
<br>
**References:**

R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.

Therneau T (2020). A Package for Survival Analysis in R. R package version 3.1-12, <URL: https://CRAN.R-project.org/package=survival>.
