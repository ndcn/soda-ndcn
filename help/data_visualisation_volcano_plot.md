Volcano plot
=======================
---
<h3 style="color:gray">Species plot</h3>.  

<details>
<summary><b> Plot and interface </b></summary>  

Plot used to visualise the differences in lipid composition between two sample groups. 
1. **Select data table.**  
Select the data to be used for comparison among the following: *Filtered data table*, *Class normalised data table*, *Total normalised data table*
2. **Select group column.**  
Select a column from the metadata table containing the groups to compare.  
3. **Select two groups to compare.**  
Select the groups to compare (2).  
4. **Select function.**  
Select the way the average value from each group should be calculated, either median or mean (median by default, recommended).  
5. **Download button.**  
Downloads to CSV the *Volcano table*.  

The datapoints, representing the individual lipid species, are coloured according to their lipid class to easily spot class-level differences.  
In volcano plots, data points situated on the top of the graph are more significant (low p-values) and those outside of the -1 to +1 range have large fold changes.  

<img src="./img/visualise_lips_volcano_plot_1.png" width="49%">
<img src="./img/visualise_lips_volcano_plot_2.png" width="49%">

</details>

<details>
<summary><b> Data processing </b></summary>

**Tables used:** {*Filtered data table*, *Class normalised data table*, *Total normalised data table*}, *Filtered feature table*.  
Samples of the two groups are selected and for each feature in the *Filtered feature table*, p-values and fold changes are calculated using one of the selected tables. Features absent from both groups are removed prior to processing.  
  
The fold change is calculated from the selected data table (one of *Filtered data table*, *Class normalised data table*, *Total normalised data table*) using the median value of the second group divided by the median value of the first group, ignoring missing values. In case of groups containing only missing values: 
- First group contains only NAs (denominator), the fold change is set to slightly above the maximum fold change, i.e. 1.01 x max fold change (high value divided by low value).  
- Second group contains only NAs (numerator), the fold change is set to slighlty below the minimum fold change, i.e. 0.99 x min fold change (low value divided by high value).  
  
In the case of medians being 0:  
- Denominator median is 0, fold change becomes Inf. Inf is set to slightly above the maximum fold change, i.e. 1.01 x max fold change (high value divided by low value).  
- Nominator median is 0, fold change becomes 0. 0s are replaced to a value slighlty below the minimum fold change, i.e. 0.99 x min fold change (low value divided by high value).  
- Both nominator and denominator are 0, fold change becomes NA. These are set to 1.  

The p-value is calculated using a Wilcoxon test on the z-scored table (one of *Filtered data table*, *Class normalised data table*, *Total normalised data table*) between group 1 and group 2 for a given feature. In case of groups containing only NAs:  
- One group contains only NAs, the p-value is set to slightly below the minimum p-value, i.e. 0.99 x min p-value (low values compared to high values).  

The p-value is then adjusted using the Benjamini-Hochberg procedure.  

The *Volcano table* is then produced by calculating the log2(fold change) to be displayed on the x-axis, and -log10(BH(p-value)) to be displayed on the y-axis.  

</details>

