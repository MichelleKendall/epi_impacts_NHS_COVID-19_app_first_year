[![DOI](https://zenodo.org/badge/593203249.svg)](https://zenodo.org/badge/latestdoi/593203249)

# Epidemiological impacts of the NHS COVID-19 app throughout its first year

Code to support the paper 'Epidemiological impacts of the NHS COVID-19 app in England and Wales throughout its first year'.

The analysis is structured as an R project and can be performed by running [main.R](main.R). We recommend running it in RStudio.

It uses a mixture of publicly available data (included) and private app data which is available on request from UKHSA. If you would like to run it and you don't have access to the private data then you may use the csvs from the "dummyprivate" folder. These are dummy versions of the private datasets needed to run the code simply so that it can be sense-checked, though it will give nonsensical results. Change the name of the "dummyprivate" folder to "private" to run the code. The "private" folder is in .gitignore so this should not cause conflicts.

Setting `resave.results` to `TRUE` will overwrite any results in the [results](results/) folder, similarly setting `resave.plots` to TRUE will resave plots in the [plots](plots/) folder. Note that plots can also be viewed individually and interactively via the RStudio interface.
