# Stat modelling and diagostics Addins for R Studio
statModelAddins is an R package that contains a list of stat modelling and diagnostics [R Studio Addins](https://rstudio.github.io/rstudioaddins/)
    
* Clustering:
    + K-means clustering 
    

## Installation and Usage

Make sure you have the latest and stable version of [devtools](https://github.com/hadley/devtools), [htmltools](https://github.com/rstudio/htmltools), [shiny](https://github.com/rstudio/shiny) and [miniUI](https://github.com/rstudio/miniUI); then install this package

```r
devtools::install_github("sarupurisailalith/statModelAddins")
```
Once the package is installed, addins will be avaiable under the 'Addins' menu in RStudio. 

An other way to launch via executing command in the console is as follows:
```r
RStudioAddins::launch_km()
```

### Additional packages required

* K-means clustering : [clValid](https://cran.r-project.org/web/packages/clValid/index.html), [plotly](https://cran.r-project.org/web/packages/plotly/index.html) - Avaialble on CRAN


### Acknowledgments

* [R Studio Addins](https://rstudio.github.io/rstudioaddins/) - *helped me get started*



To Contirubute to this package - Fork this repository, make your additions and simply submit a pull request. 
