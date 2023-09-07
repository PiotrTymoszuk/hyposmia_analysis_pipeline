# hyposmia_analsis_pipeline

## Summary

The complete analysis pipeline for the hyposmia project by Health After COVID-19 in Tyrol and CovILD Study Teams aiming at identification of recovery phenotypes of post-COVID-19 condition and resolution of post-COVID-19 olfactory dysfunction.

<p align = "center"> 
<img src = "https://user-images.githubusercontent.com/80723424/229633138-426a6922-0468-463b-a770-86aaf7951796.png" width = "80%">
</p>

<br>

## Terms of use

Please cite the repository, and our [preprint](https://www.medrxiv.org/content/10.1101/2022.06.02.22275932v1) (DOI: 10.1101/2022.06.02.22275932) or the [peer-reviewed publication of the analysis results](https://link.springer.com/article/10.1007/s00405-023-08163-x) (DOI: 10.1007/s00405-023-08163-x). The raw .RData data files will be made available upon request to the study authors, [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at) and [Prof. Raimund Helbok](mailto:raimund.helbok@tirol-kliniken.at).

## Usage

The following development packages are required to run the pipeline:

```r

devtools::install_github('PiotrTymoszuk/soucer') ## script sourcing
devtools::install_github('PiotrTymoszuk/ExDA') ## exploratory data analysis and staristical hypothesis testing
devtools::install_github('PiotrTymoszuk/clustTools') ## factor analysis and unsupervised clustering
devtools::install_github('PiotrTymoszuk/somKernels') ## extra distances for self-organizing maps
devtools::install_github('PiotrTymoszuk/figur') ## management of figures and tables in Rmd documents
devtools::install_github('PiotrTymoszuk/trafo') ## handling of tabular data
devtools::install_github('PiotrTymoszuk/microViz') ## visualization

```

Source 'exec.R' to launch the entire pipeline:

```r

source('exec.R')

```

## Contact

The repository maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at) and [Prof. Raimund Helbok](mailto:raimund.helbok@tirol-kliniken.at).
