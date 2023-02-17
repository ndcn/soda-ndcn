Volcano plot
=======================
---
### Plot and interface
Plot used to visualise the differences between two sample groups. The groups can be selected through the sidebar with "Select group column" (a column from the metadata table containing the groups to be compared). The comparison is done on a group-on-group basis: two groups must be selected in the "Select two groups to compare" slot.  
The data table produced for the volcano plot can be downloaded using the download button.  
The datapoints, representing the individual lipid species, are coloured according to their lipid class to easily spot class-level differences.  
In volcano plots, data points situated on the top of the graph are more significant (low p-values) and those outside of the -1 to +1 range have large fold changes.  

<img src="./img/visualise_lips_volcano_plot_1.png" width="49%">
<img src="./img/visualise_lips_volcano_plot_2.png" width="49%">

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

The *volcano table* is then produced by calculating the log2(fold change) to be displayed on the x-axis, and -log10(BH(p-value)) to be displayed on the y-axis.  
