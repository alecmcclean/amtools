# amtools
amtools contains (will contain) a suite of R functions (so far one function) to help with data analytics.

## Installation
The package can be downloaded with the following snippet:
```{r download}
library(devtools)
install_github("alecmcclean/amtools")
```

## Function Details
### DataVisualiser
DataVisualiser creates a user interface using shiny to allow data exploration and visualization in one, two or three variables.

### ModelByGroup
ModelByGroup runs and estimates a model by specific grouping.  

### FillNA
FillNA adds extra customizability and usability to the usual na.locf and fill functions to deal with missing values.
