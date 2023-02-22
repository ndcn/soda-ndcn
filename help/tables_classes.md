Class tables
=======================
---

Tables related to the lipid classes, stored in the Omics_data class.  

### Total normalised class table
A class table is produced by getting all the lipid classes from the lipidomics total normalised table to total lipids to obtain a sample x lipid class table. For each sample, the class value is calculated by summing the values of the lipid species of that class in that sample. NA values are imputed by 0.  
```
Omics_data$tables$data_class_table
```

### Total normalised and z-scored class table

```
Omics_data$tables$data_class_table_z_scored
```
