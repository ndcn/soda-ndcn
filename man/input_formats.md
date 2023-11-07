## Input formats

The Start Module serves as the central hub for managing your experiment modules. From here, modules can be named, created, deleted, and example datasets can be loaded.

### Metadata file:  
Supported formats are coma-separated files (CSV, CSV2), tab-separated files (TSV, TXT) and Excel files (XLSX). Contains all metadata pertaining to each sample, values can be of any type. There is no mandatory naming for the columns, but there should be minimum four of them containing:
- **Sample IDs**: unique identifiers (same in all the tables).  
- **Sample type**: specifies which rows are blanks, QC, pools and actual samples.  
- **Group type**: specifies which groups each sample belongs to.   
- **Batch**: batch number for each row. Recommended for all, even blank samples.  
This is the default order in which columns are selected in SODA, it is therefore recommended.

### Data file:  
Supported formats are coma-separated files (CSV, CSV2), tab-separated files (TSV, TXT) and Excel files (XLSX). Contains samples as rows and features as columns. The first column should be the sample IDs (same as in the metadata table), the rest are the feature names, values are only numerical or missing. Currently supported feature names are the following:
- Lipidyzer names (Lipidomics)
- Symbol (Proteomics, Transcriptomics, Genomics)
- Entrez ID (Proteomics, Transcriptomics, Genomics)
- Uniprot (Proteomics, Transcriptomics, Genomics)

