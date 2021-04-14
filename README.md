# README

This repository includes data and code required to reproduce the figures from [Papst *et al.* 2021](https://doi.org/10.1186/s12889-021-10611-4).

The data files and dictionary are provided in the `data` subdirectory. The plotting code is in the `figure-scripts` subdirectory. To run a figure script from the command line:

```
cd figure-scripts
R CMD BATCH --vanilla figure-1.R
```

This command will generate `figure-1.pdf` (in the `figure-scripts` subdirectory).

## Paper citation

Papst, I., Li, M., Champredon, D. _et al_. Age-dependence of healthcare interventions for COVID-19 in Ontario, Canada. _BMC Public Health_ **21**, 706 (2021). DOI: [10.1186/s12889-021-10611-4](https://doi.org/10.1186/s12889-021-10611-4)

## Session information

The figure scripts were most recently run with the following session information:

```
R version 4.0.3 (2020-10-10)
Platform: arm-apple-darwin20.2.0 (64-bit)
Running under: macOS  11.2.3

Matrix products: default
BLAS:   /opt/homebrew/Cellar/openblas/0.3.13/lib/libopenblasp-r0.3.13.dylib
LAPACK: /opt/homebrew/Cellar/r/4.0.3_2/lib/R/lib/libRlapack.dylib

locale:
[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] mgcv_1.8-33       nlme_3.1-149      patchwork_1.1.1   ggplot2_3.3.3
 [5] stringr_1.4.0     forcats_0.5.1     lubridate_1.7.9.2 magrittr_2.0.1
 [9] tidyr_1.1.2       dplyr_1.0.4       readr_1.4.0

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6        pillar_1.4.7      compiler_4.0.3    base64enc_0.1-3
 [5] R.utils_2.10.1    R.methodsS3_1.8.1 tools_4.0.3       digest_0.6.27
 [9] lifecycle_0.2.0   tibble_3.0.6      gtable_0.3.0      lattice_0.20-41
[13] pkgconfig_2.0.3   rlang_0.4.10      Matrix_1.2-18     DBI_1.1.1
[17] cli_2.3.0         rstudioapi_0.13   withr_2.4.1       R.devices_2.17.0
[21] generics_0.1.0    vctrs_0.3.6       hms_1.0.0         cowplot_1.1.1
[25] grid_4.0.3        tidyselect_1.1.0  glue_1.4.2        R6_2.5.0
[29] farver_2.0.3      purrr_0.3.4       ps_1.5.0          scales_1.1.1
[33] ellipsis_0.3.1    splines_4.0.3     assertthat_0.2.1  colorspace_2.0-0
[37] labeling_0.4.2    stringi_1.5.3     munsell_0.5.0     crayon_1.4.1
[41] R.oo_1.24.0
```
