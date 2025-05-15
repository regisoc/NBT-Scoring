# NBT-Scoring
See scoring wiki for more information on code, measures, and scores. https://github.com/GershonLab/NBT-Scoring/wiki


## Changes

Changes from this repo only affects the `score_data_batch.R` script.


## Expected file name pattern

Expected pattern:

```
ncl_ch_nbtb_<PSCID>_<DCCID>_<VISIT_LABEL>_<FILENAME>ExportNarrow.csv
```

Expected trio of files for each tuple `<PSCID>_<DCCID>_<VISIT_LABEL>`:

```
ncl_ch_nbtb_<PSCID>_<DCCID>_<VISIT_LABEL>_ItemExportNarrow.csv
ncl_ch_nbtb_<PSCID>_<DCCID>_<VISIT_LABEL>_RegistrationExportNarrow.csv
ncl_ch_nbtb_<PSCID>_<DCCID>_<VISIT_LABEL>_ScoresExportNarrow.csv
```