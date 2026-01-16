# NBT-Scoring
See scoring wiki for more information on code, measures, and scores. https://github.com/GershonLab/NBT-Scoring/wiki

The purpose of this code is to score several measures and composites within the NIH Baby Toolbox iPad app specific to the Norming App version.

## Installation
1. To get started, please download the following:
* R
* R Studio
* GitHub desktop client

2. Within R Studio, run the following lines of code in your console to download the following packages:
```
install.packages('psych',dep=T)
install.packages('janitor',dep=T)
install.packages('tidyverse',dep=T)
install.packages('mirt',dep=T)
install.packages('mirtCAT',dep=T)
install.packages('mnormt',dep=T)
install.packages('nlme',dep=T)
install.packages('jsonlite',dep=T)
install.packages('purrr',dep=T)
```
3. In GitHub desktop, fork this repository.
4. Organize your raw, unscored data prior to running this code for scoring. You will need separate folders for your item exports, registration exports, and json exports. You will also need to modify the code in either score_data.R or score_data_batch.R to refer to the location of your raw data files. Please see the wiki page: https://github.com/GershonLab/NBT-Scoring/wiki/How-to-score-the-data as well as the individual R files for scoring for more information.
