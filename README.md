# SoundscapeMapping

Code provided in this repository is not operational, but supports the accompanying science manuscript and data depository [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10938897.svg)](https://doi.org/10.5281/zenodo.10938897). 

Brief descriptions of select scripts are provided below, site locations are not included in the data or code. Code is provided to demonstrate methodologies used in associated manuscript and are not fully reproducible with data provided.

## 0_acoustic_setup
- all scripts in this repository are not directly replicable with provided data as they rely on exact site XY coordinates. 
- 4_response_df_prep.R: generates the period-of-day response data frame for model training.

## 1_predictor_setup (final processed spatial data are provided)
- 1_stack_predictors.R: generates gridded predictor stacks
- 2_site_extraction.R: generates the extracted predictors dataframe

## 2_spatial_modeling
- 0_feature_selection.R: generates the final predictor vif3 dataframe used in spatial modeling using the variance inflation factor univariate RF methodology
- 1_clustered_fold.R: generates the geo-CV folds based on ArcGIS clustering IDs 
- 2_ranger_fit-HPC_by_fold.R: trains a random forest model using single geo-CV fold

## 3_post_modeling
- 0_performance.R: Figure 3
- 2_prediction_surface_avg-HPC.R: Figures 7,8,9,10
- 3_variable_importance_analysis.R: Figure 4
- 6_PDP_top5.R: Figures 5,6
- 7_fire_analysis.R: Figure 11
