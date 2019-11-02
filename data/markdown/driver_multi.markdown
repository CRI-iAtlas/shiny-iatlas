This section displays mulit-variable models of the form:

Response ~ Mutation Status + transformation(Covariate1) ... + transformation(CovariateN)

Across group mode: sampels are not spliut by group, instead pvalues are calculated across all groups.
By group mode: This works identically to the single variable models above.

Add covriates by clicking the two add covariate functions. 
- You must select atleast one covariate.
- Each added covariate comes with a remove button.
- Numerical covariates come with an optional transformation selection.

Once satisfied with the formula for the lnear model press the calculate button, this will take a few seconds.

Click on a point to see a violin plot for the immune readout value distribution in mutated vs non-mutated samples for the selected cohort and driver.
