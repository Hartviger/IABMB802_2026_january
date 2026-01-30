# IABMB802_2026_january

This repository contains code and example files used in the IABMB802 ISA project (Jan 2026).  
The focus is maintenance and development work in MetaboLink, including an updated Lipid Heatmap workflow and two new supporting modules (Seq generator and Absolute Quantification).

## Contents

### Lipid Heatmap (old vs new)
These files are included to document and reproduce the main changes between the earlier version and the updated implementation:

- `old_version_lipidheatmap_UI.R`  
- `old_version_lipidheatmap_functions.R`  
- `old_version_server.R`  

- `new_version_lipidheatmap_UI.R`  
- `new_version_lipidheatmap_functions.R`  
- `new_version_server.R`  

### New modules
Two additional tools were implemented as separate modules/modals:

- **Seq generator**  
  - `hyggeproject_ui_server.R`  
  Generates a sequence table from a CSV input (with column mapping, overview, editing, and export).

- **Absolute Quantification**  
  - `Absolute Quantification_server_ui.R`  
  Modal-based workflow for absolute quantification using internal standards, sample amounts, and matching strategy.

### Example input files
- `seq_test.csv` — example input for the Seq generator  
- `data_test.csv` — example dataset for testing

## How to run/test
Chechk out my test server:
https://jakobhartvigapps.shinyapps.io/MetaboLink_test_server/  
