Double bond plot
=======================
---
### Plot and interface
Plot comparing two sample groups based on a lipid class, the double bond and carbon counts. The groups can be selected through the sidebar with "Select group column" (a column from the metadata table containing the groups to be compared). The comparison is done on a group-on-group basis: two groups must be selected in the "Select two groups to compare" slot.  
The markers, representing individual lipid species, are coloured according to -log10(BH(p-value)), meaning that the higher their value is, the more significant that lipid is (low p-value). The marker size is relative to log2(fold change). Values are best examined by downloading the associated table using the download button.  

<img src="./img/visualise_lips_dbplot_1.png" width="49%">
<img src="./img/visualise_lips_dbplot_2.png" width="49%">

### Data processing
Samples of the two groups are selected and for each feature in the *Filtered feature table*, p-values and fold changes are calculated.  
The fold change is calculated using the median value of the second group divided by the median value of the first group, ignoring missing values. In case of groups containing only missing values: 
- Both groups contain only NAs, the fold change is set to 1 by default (low values in both groups).  
- First group contains only NAs (denominator), the fold change is set to slightly above the maximum fold change, i.e. 1.01 x max fold change (high value divided by low value).  
- Second group contains only NAs (numerator), the fold change is set to slighlty below the minimum fold change, i.e. 0.99 x min fold change (low value divided by high value).  

The p-value is calculated using a Wilcoxon test on the z-score normalised values (*Z-scored data table*) between group 1 and group 2 for a given feature. In case of groups containing only NAs:  
- Both groups contain only NAs, the p-value is by default set to 1 (low values in both groups).  
- One group contains only NAs, the p-value is set to slightly below the minimum p-value, i.e. 0.99 x min p-value (low values compared to high values).  

The p-value is then adjusted using the Benjamini-Hochberg procedure.  

The *double bonds table* is then produced from the *Filtered feature table* (containing feature metadata) and adding log2(fold change) and -log10(BH(p-value)).    
