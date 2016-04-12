# rothfreezer *R* package

The data in this *R* package is intended for use by members of the Rothstein Lab to enable modifiable yet reproducible strain and reagent annotations in [Rmarkdown] reports.

## Installation

```r
# Install devtools (>= 1.9.1)
if (!require('devtools') || packageVersion('devtools') < '1.9.1') {
  install.packages('devtools')
}

devtools::install_github('EricEdwardBryant/rothfreezer', build_vignettes = T)
```

## Contribute

### Add entries to database

1. **More work for me:** Create a separate `.csv` file for the table you wish to update. Ensure your table has appropriate column names and only your new entries (rows). Send this file to me (Eric Bryant) and I will add it to the package for you.

2. **Less work for me:** Fork the repository, commit your changes, and submit a pull request on GitHub ([example]). You can make sure that your column names and keys are correct by running `easydb::db_doctor('inst/db/_rothfreezer.yaml')` before submitting your pull request.


### Modify entries in database

1. **More work for me:** Submit an "Issue" on GitHub detailing which table and entry you would like to update.

2. **Less work for me:** Fork the repository, commit your changes, and submit a pull request on GitHub ([example]). Again, you can make sure that your column names and keys are correct by running `easydb::db_doctor('inst/db/_rothfreezer.yaml')` before submitting your pull request.



[Rmarkdown]: http://rmarkdown.rstudio.com
[example]: http://kbroman.org/github_tutorial/pages/fork.html
