Data objects
=======================
---

Tables produced during the data processing and stored in the Omics_data class.  

### Raw metadata table
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

### Raw data table
Imported lipidomics table. This contains the concentration of each lipid compound / species detected in each sample. The molecule names are formatted in a specific way so as to extract their lipid class, carbon and unsaturation counts easily. This table can contain NA values. The only mandatory column is a form of *ID* column containing sample IDs present in the *Raw metadata table*.  

The Raw data table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_raw
```
It is first initiated when the user uploads their lipidomics data table in the *Lipidomics* upload tab. It is mostly used to generate the filtered version and store the original data so that the user can start again the processing using the availbable reset buttons.  

### Filtered metadata table
Updated version of the *Raw metadata table* after setting an ID column and filtering using the *Metadata filter* tab. Typically this table has no longer blank, QC and pool samples, which remain in the *Raw metadata table* for when the need arises. Additionnally, the user should filter out outliers which can be observed in the visualisation tab, or reduce the dataset if only a subset is to be examined.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$meta_filtered
```

### Filtered data table
Updated version of the *Raw data table* after setting an ID column, and is automatically filtered to contain only the rows present in the *Filtered meta table*. It is then further filtered in the *Lipidomics filter* tab, by filtering out features using the *Blank & Group filtering* and the *feature filters*.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_filtered
```
It is created as soon as an ID column is selected in the *Lipidomics upload* tab, and further modified with the filtering.  

### Filtered feature table  
Updated version of the *Raw feature table* using the features available in the *Filtered data table*. This is the main feature table that will be used when mapping feature metadata.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$feat_filtered
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  

### Z-scored data table
Z-scoring applied to the *Filtered data table* to normalise the data.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  

### Class normalised data table
Data from the *Filtered data table* normalised to the lipid classes of that sample.  The values for each lipid are divided by the summed values of all lipids of the same class.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_class_norm
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  

### Total normalised data table
Data from the *Filtered data table* normalised to the total lipid content of that sample. Each value is divided by its row sum.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_total_norm
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  

### Class normalised and z-scored data table
Z-scoring applied to the *Class normalised data table* to normalise further the data.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_class_norm_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  

### Total normalised and z-score data table
Z-scoring applied to the *Total normalised data table* to normalise further the data.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_total_norm_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  







