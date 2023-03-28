Class tables
=======================
---

Tables related to the lipid classes, stored in the Omics_data class.  

<details>
<summary><b> Raw class table </b></summary>

A class table is produced by getting all the lipid classes from the *Filtered data table* to obtain a sample x lipid class table. For each sample, the class value is calculated by summing the values of the lipid species of that class in that sample. NA values are imputed by 0. The use of this table is not recommended, the preferred table being *Total normalised class table*.  
```
Omics_data$tables$data_class_table_raw
```
</details>

<details>
<summary><b> Total normalised class table </b></summary>

A class table is produced by getting all the lipid classes from the *Total normalised data table* to obtain a sample x lipid class table. For each sample, the class value is calculated by summing the values of the lipid species of that class in that sample. NA values are imputed by 0.  
```
Omics_data$tables$data_class_table_total_norm
```
</details>

<details>
<summary><b> Z-scored total normalised class table </b></summary>

```
Omics_data$tables$data_class_table_z_scored
```
</details>
  