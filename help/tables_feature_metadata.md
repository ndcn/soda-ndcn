Feature tables
=======================
---

Tables related to the feature metadata, stored in the Omics_data class.  

### Raw feature table  
Tool tip missing.   
```
Omics_data$tables$feat_raw
```

### Filtered feature table  
Updated version of the *Raw feature table* using the features available in the *Filtered data table*. This is the main feature table that will be used when mapping feature metadata.  
This table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$feat_filtered
```
It is automatically generated with the *Filtered data table* and updated each time the features are filtered.  
