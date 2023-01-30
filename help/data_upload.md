Data upload
=======================
---

## Sample metadata
#### Upload tab
Tab to upload the sample metadata and identify some key columns and rows.  

**Sample IDs:** column containing the sample IDs shared between all uploaded tables.  
**Type column:** column containing the different sample types, i.e. whether they are actual samples, blanks, quality control or pool samples. *Defaults to the second column*.  
**Blank pattern:** text pattern to be found the in Type column rows to detect blank samples.  
**QC pattern:** text pattern to be found the in Type column rows to detect QC samples.  
**Pool pattern:** text pattern to be found the in Type column rows to detect pool samples.  
**Text pattern feedback:** the number of blanks, QCs and pool samples found by text patterns is displayed live below the table.  

#### Filter tab
Tab to set various filters which can be applied using the "Filter" button or reset using the "Reset table" button. The filtered table can be viewed directly in this table for direct feedback, and the sample count (number of rows) after filtering is displayed directly under that table.

**Non-sample exclusion:** exclude blanks, QCs and/or pool samples based on the previously used text patterns. This is recommended: the exclusion is only for the result analyses, they are still used for blank filtering in the feature filtering tabs.  
**Metadata exclusion:** exclude samples based on some metadata values. For instance, if samples from Batches 1 should be excluded, select the batch column from the metadata table in the "Column" field, select "1" in the "Values field", and the samples from batch 1 will be displayed in the Samples field. Filter them out by clicking the "Filter" button.  
**Manual sample exclusion:** exclude samples based on their name. This is designed to exclude specific samples that are known to be problematic, like outliers after identifying them in the PCA.  

---

## Lipidomics data
#### Upload tab
Tab to upload the lipidomics data and identify some key columns.

**Sample IDs:** column containing the sample IDs shared between all uploaded tables.  
**Group column:** column containing the default groups for the lipidomics data visualisation (this can be changed later) and more importantly, the groups for the feature filtering. It is a column from the sample metadata table, and a direct feedback the that column's groups is given below the table preview. Note that this should always be the main group studied as it is used in the blank filtering. When going back and forth between visualisation and filtering, it is important to also change the group column to be the main focus groups. *Defaults to the second column in the metadata table (after having selected the ID column, i.e. 3rd column in the raw metadata if the first column is ID)*.  

#### Filter tab
Tab to filter out features which are not significantly above blanks samples. This filter operates on each feature individually.  
**Blank multiplier:** for a given feature, number of times above the blank mean each sample must be to be considered a significative value.  
**Sample threshold:** for a given feautre, proportion of the total amount of samples that must be above the blank values x blank multiplier for that feature to be kept.  
**Group threshold:** secondary threshold for the main group of interest. Works like "Sample threshold" but instead of being in proportion to the total amount of samples, it will be in proportion to the number of samples in each group.  
For more details, see the Processing functions Help menu : Blank and group filtering.  

The effects of the filtering on the feature count and the lipid classes are displayed live in the form of a progress bar and two bar plots.  
The first bar plot is the absolute count of molecules for each lipid class, with the total amount and the amount that remains after filtering.  
The second bar plot is the relative count of molecules remaining in each class after filtering. This is mainly to evaluating the effects of the filtering on minor classes which might be difficult to notice on the absolute compound count.  
