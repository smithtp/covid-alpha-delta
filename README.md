# covid-alpha-delta
Investigating the climate response of alpha and delta SARS-CoV-2 variants

## Data

 - `df_rt.rds` - Rt of S+ and S- strains during Alpha sweep, obtained from https://github.com/mrc-ide/sarscov2-b.1.1.7 (Volz et. al. paper).
 - `sgss_stp_new_43_56_weeks.rds` - S+ and S- cases during Alpha sweep, obtained from https://github.com/mrc-ide/sarscov2-b.1.1.7 (Volz et. al. paper).
 - `stp_population_long.csv` - populations of STP areas, from Volz et. al.
 - `oxford-interventions.csv` - NPI data from the Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker/tree/master/data).
 - `sgtf_output.csv` - Rt of Delta and Alpha strains during Delta sweep, produced for this study using same methods and data source as Volz paper.
 - `cases_data.rds` - S+ and S- cases during Delta sweep, for this study, using same methods and data source as Volz paper.
 - `/gis/` folder - shapefiles for UK STP and LTLA areas, obtained from the office for national statistics (https://geoportal.statistics.gov.uk).
 - `ltla-vacc.csv` - vaccination data, obtained from UK government portal (https://coronavirus.data.gov.uk/).
 - `data/alpha_background_climate.csv` - Rt and environment of alpha and background strains during the alpha variant sweep, produced by `combine_data_alpha.R`.
 - `delta_alpha_climate.csv` - Rt and environment of delta and alpha strains during the delta variant sweep, produced by `combine_data_delta.R`.


## Code

- `packages.R` - source required packages, download any that are missing.
- `combine_data_alpha.R` - combine raw data for alpha variant sweep.
- `combine_data_delta.R` - combine raw data for delta variant sweep.
- `analysis.R` - perform the main analyses.