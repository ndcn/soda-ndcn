Non-sample tables
=======================
---

Tables containing data relating to non-samples (blanks, QCs and pools) stored in the Omics_data class.  
<details>
<summary><b> Blank table </b></summary>

Contains the raw blank values from the imported lipidomics *Raw data table* but with the ID column set as in the *Filtered data table*. Since non-samples should be removed from the *Filtered data table*, the blank values are kept in this table for operations like blank filtering.  

The table can be accessed in the Omics_data object thus:  
```
Omics_data$tables$blank_table
```
It is first initiated when the user uploads their lipidomics data table in the *Lipidomics upload* tab.  
</details>
