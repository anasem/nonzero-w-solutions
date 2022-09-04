# Solution to the problem of non-vanishing $\bar{w}$ in TEA

This repository contains data and scripts needed to reproduce the figures of
Emad and Siebicke (2021)


**True eddy accumulation - Part 1: Solutions to the problem of non-vanishing mean vertical wind velocity**  
 [![DOI:10.5194/amt-2021-319](https://img.shields.io/badge/doi-10.5194%2Famt--2021--319-blue)](https://doi.org/10.5194/amt-2021-319)
 
 
The publication version is archived on zenodo  
[![DOI](https://zenodo.org/badge/532369737.svg)](https://zenodo.org/badge/latestdoi/532369737)


 
 Abstract

> The true eddy accumulation method (TEA) provides direct measurements of
> ecosystem-level turbulent fluxes for a wide range of atmospheric constituents. 
> TEA utilizes conditional sampling to overcome the requirement for a fast sensor
> response demanded by the state-of-the-art eddy covariance method (EC).
> The TEA method is formulated under the assumption of ideal conditions with a
> zero mean vertical wind velocity during the averaging interval.  
> However, this idealization is rarely met under field conditions.  
> Additionally, unlike in EC, this assumption can not be imposed in post
> processing due to the real-time nature of sampling and the absence of
> high-frequency measurements of the scalar.
> Consequently, fluxes measured with the TEA method are biased with a
> non-turbulent advective term that scales with the scalar mean concentration.
> 
> Here, we explore the magnitude of this biased advective term and potential ways
> to minimize or remove it.
> We propose a new formulation to calculate TEA fluxes that minimizes the bias term.
> The new formulation shows that the magnitude of the error is constrained to
> $\bar{w}/{\overline{|w|}}$ when the stationarity criterion is fulfilled.
> Here, $w$ is the vertical wind velocity, and the overbar denotes time averaging. 
> The error is shown to be dependent on the asymmetry of atmospheric transport,
> represented by the coefficient $\alpha_{c}$.
> Two methods of estimating the coefficient $\alpha_{c}$ are proposed, a
> probabilistic treatment of turbulent transport, and a method utilizing the
> assumption of scalar similarity.
> We show how other formulas for calculating the TEA flux are linked to the new
> formulation and explore the different corrections in a numerical simulation.
> 
> The new formulation avoids the direct dependence of the bias term on the scalar
> background concentration.
> This result increases the confidence in applying the TEA method to measuring
> fluxes of atmospheric constituents. 
> This is particularly relevant to scalars with a large background concentration
> and a small flux.
> This paper is part one of a two-part series on true eddy accumulation. 

 
## Data format
Data are provided under the directory `data` in two different formats:
- `rds`: native R serialization format, convenient and recommended for loading the data
  into R.
- `csv`: for intolerability, a copy of the data is provided in csv format.

## How to reproduce
Run `scripts/03-create-plots.R` which will call the dependency scripts and
save the resulting figures in `figures` directory.
 
### Metadata
Metadata for all files is stored under `data/metadata`.
The metadata contains information about variables description and units.

## Requirements

The font [Carrois Gothic](https://fonts.google.com/specimen/Carrois+Gothic) is
required for the figures.

R packges in `scripts/00-deps.R` are needed.
Below is a full R session info.

### R session info
<details>
   <summary>sessionInfo()</summary>
 
   ```R
   > sessionInfo()
     R version 4.2.1 (2022-06-23)
     Platform: x86_64-pc-linux-gnu (64-bit)
     Running under: Manjaro Linux
     
     Matrix products: default
     BLAS/LAPACK: /opt/intel/oneapi/mkl/2022.1.0/lib/intel64/libmkl_gf_lp64.so.2
     
     locale:
      [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=de_DE.UTF-8       
      [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
      [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
     [10] LC_TELEPHONE=C             LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
     
     attached base packages:
     [1] stats     graphics  grDevices utils     datasets  methods   base     
     
     other attached packages:
     [1] patchwork_1.1.1   latex2exp_0.9.4   lubridate_1.8.0   lmodel2_1.7-3     data.table_1.14.2
     [6] ggplot2_3.3.6     nvimcom_0.9-132   colorout_1.2-2   
     
     loaded via a namespace (and not attached):
      [1] magrittr_2.0.3   tidyselect_1.1.2 munsell_0.5.0    colorspace_2.0-3 R6_2.5.1        
      [6] rlang_1.0.4      fansi_1.0.3      stringr_1.4.0    dplyr_1.0.9      tools_4.2.1     
     [11] grid_4.2.1       gtable_0.3.0     utf8_1.2.2       cli_3.3.0        DBI_1.1.3       
     [16] withr_2.5.0      digest_0.6.29    assertthat_0.2.1 tibble_3.1.8     lifecycle_1.0.1 
     [21] farver_2.1.1     purrr_0.3.4      vctrs_0.4.1      glue_1.6.2       labeling_0.4.2  
     [26] stringi_1.7.8    compiler_4.2.1   pillar_1.8.0     generics_0.1.3   scales_1.2.0    
     [31] pkgconfig_2.0.3 
 ```
 
</details>



