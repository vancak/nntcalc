# R shiny application for the NNT calculator
[Valentin Vancak](https://www.linkedin.com/in/valentin-vancak-0a56227a/?originalSubdomain=il)
#### This application is based on the `nntcalc` R package[1]
This is the [source code](https://github.com/vancak/nntcalc/commit/58d0c17c1e90caab7662b21e72243cb480363c8d) of the R shiny application of the NNT calculator that is available [here](https://nntcalc.iem.technion.ac.il/). This repository also includes the [PANSS](https://en.wikipedia.org/wiki/Positive_and_Negative_Syndrome_Scale#:~:text=The%20Positive%20and%20Negative%20Syndrome,the%20study%20of%20antipsychotic%20therapy.) datasets simulation source code and the [resulted datasets](https://github.com/vancak/nntcalc_shinyapp/tree/main/data). The NNTcalculator R shiny application provides functions to calculate the unadjusted, the adjusted, and the marginal Laupacis NNT with the corresponding 95% confidence intervals. Available regression models include ANOVA, linear and logistic regression, and Cox models. In addition, the package provides a function to calculate the estimators of the Kraemer & Kupfer's NNT. For further details on the `nntcalc` package functionality see the package [manual](https://github.com/vancak/NNTcalculator/blob/main/manual.pdf).

### References 
[1] Vancak V. nntcalc: The Number Needed to Treat (NNT) calculator. 2020. R package version 1.1.
