# GlobClus_prop Analysis

The aim of this project is to analyze globular stars clusters in the Milky Way, in order to understand their dynamics.  
The conducted study examined the properties that affect the central velocity dispersion, their impact and the correlations between them.  
The task was to find an accurate model that could fit the given data ([GlobClus_prop dataset](https://search.r-project.org/CRAN/refmans/astrodatR/html/GlobClus_prop.html))
and be used to efficiently explain the statistical distribution of the contained records.  
**Note:** The statistical distribution in this study has been assumed as linear (known beforehand).

## Study Logic
### First phase
In order to identify an initial good model containing only the most significant variables for the central velocity dispersion, several approaches have been used,
including p-value comparisons and penalization criteria.  
The evaluations inlcuded all the directions, forward, backward and mixed.  
More specifically, a combination of all the following methods have been used:
- P-Value
- Likelihoods Comparison
- AIC (Akaike Information Criterion)
- BIC (Bayesian Information Criterion)

### Second phase
This phase consisted in the evaluation of the model and the correlation analysis between the identified features.  
It has been carried out by plotting the features' data and analyzing: confidence intervals, correlation matrices, generic linear models, and graphical model representations.  
The graphs included:
- Gaussian linear graphs (undirected with updated with a BIC forward stepwise procedure)
- Bayesian networks (directed)

## Reporting
A detailed technical report for the whole analysis can be found [here](https://github.com/Scrayil/GlobClus_prop-Analysis/blob/6d9da4a8c89307bd71ca7926ca1cd14437a27544/report/Globular%20Stars%20Clusters%20-%20Technical%20Report.pdf).  
If you are interested into the compiled analysis script (html generated report) you can find it rendered [here](https://Scrayil.github.io/GlobClus_prop-Analysis/model_analysis.html) instead.

## License
Copyright 2024 Mattia Bennati  
Licensed under the GNU GPL V2: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html
