Lipidomics tables
=======================
---

Tables related to the lipidomics data, stored in the Omics_data class.  
<details>
<summary><b> Raw data table </b></summary>

Imported lipidomics table. This contains the concentration of each lipid compound / species detected in each sample. The molecule names are formatted in a specific way so as to extract their lipid class, carbon and unsaturation counts easily. This table can contain NA values. The only mandatory column is a form of *ID* column containing sample IDs present in the *Raw metadata table*.  

The Raw data table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_raw
```
It is first initiated when the user uploads their lipidomics data table in the *Lipidomics* upload tab. It is mostly used to generate the filtered version and store the original data so that the user can start again the processing using the availbable reset buttons.  
</details>

<details>
<summary><b> Filtered data table </b></summary>

Updated version of the *Raw data table* after setting an ID column, and is automatically filtered to contain only the rows present in the *Filtered meta table*. It is then further filtered in the *Lipidomics filter* tab, by filtering out features using the *Blank & Group filtering* and the *feature filters*.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_filtered
```
It is created as soon as an ID column is selected in the *Lipidomics upload* tab, and further modified with the filtering.  
</details>

<details>
<summary><b> Class normalised data table </b></summary>

Data from the *Filtered data table* normalised to the lipid classes of that sample.  The values for each lipid are divided by the summed values of all lipids of the same class. NAs are imputed by 0.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_class_norm
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
</details>

<details>
<summary><b> Total normalised data table </b></summary>

Data from the *Filtered data table* normalised to the total lipid content of that sample. Each value is divided by its row sum.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_total_norm
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
</details>

<details>
<summary><b> Z-scored data table </b></summary>

Z-scoring applied to the *Filtered data table* to normalise the data. NA values are left untouched (NA) and the mean values are calculated by omiting NA values.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
</details>

<details>
<summary><b> Z-scored class normalised data table </b></summary>

Z-scoring applied to the *Class normalised data table* to normalise further the data.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_class_norm_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
</details>

<details>
<summary><b> Z-scored total normalised data table </b></summary>

Z-scoring applied to the *Total normalised data table* to normalise further the data.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$data_total_norm_z_scored
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
</details>


