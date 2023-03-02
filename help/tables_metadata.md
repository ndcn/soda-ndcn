Metadata tables
=======================
---

Tables related to the sample metadata, stored in the Omics_data class.  
<details>
<summary><b> Raw metadata table </b></summary>

Imported sample metadata table. It should not contain any NA values. Typically recommended columns include:  
- *ID*, column containing the unique identifier for each row, tying the sample metadata to the rows in the other data tables.  
- *Sample_type*, i.e. whether row is an actual sample, a QC, a blank or a pool sample. The type of each sample should also be specified here, like the cell type for example.  
- *Group_type*, i.e. the main groups in the data the user wants to compare. Typically this is genotype.  
- *Sample_name* if the user wants to keep their own sample names somewhere.  
- *Batch* in case of multiple batches being analysed.  
- *Harvest_date*, with the date formatted yyyymmdd.  
- *Cell_type*, redundant with *Sample_type* which should be the column containing the cell type.  
- ... any other data that might be of use.  

Among these, only the ID, sample type and group type columns is mandatory. There are no naming imperatives aside from avoiding spaces and special characters. The user specifies the columns to be set as ID, group and type in the UI.  

The Raw metadata raw table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$meta_raw
```
It is first initiated when the user uploads their metadata table in the *Metadata* upload tab. It is mostly used to generate the filtered version and store the original data so that the user can start again the processing using the availbable reset buttons.  
</details>

<details>
<summary><b> Filtered metadata table </b></summary>

Updated version of the *Raw metadata table* after setting an ID column and filtering using the *Metadata filter* tab. Typically this table has no longer blank, QC and pool samples, which remain in the *Raw metadata table* for when the need arises. Additionnally, the user should filter out outliers which can be observed in the visualisation tab, or reduce the dataset if only a subset is to be examined.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$meta_filtered
```
</details>
