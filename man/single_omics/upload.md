### Metadata upload:  
Tab designed for the upload and curation metadata. Once uploaded, two distinct tables are generated and stored in the omics object:
- Imported metadata table: this is the original table, reserved for reset purposes.
- Raw metadata table: the above table filtered by the user, specifically for processing in later steps.

Within the interface, these tables can be selected using the dropdown menu located adjacent to the *Browse* button. Based on the selection, a table preview along with a data summary will be showcased. For easy access and portability, each table can be saved through the 'Download' button. Interactivity with the Raw Metadata Table is enhanced with the widgets located on the right:
- Select Column: This feature lets users designate columns as outlined in the *Metadata File* section.
- Text Patterns: text matching, handy for automatic identification of blanks, QCs, and pooled samples within the *Sample Type* column.
- Sample Filtering:
	- Blanks, QCs, and Pools can be excluded from analysis. This is the suggested approach unless assessing the experiment's quality.
	- Users can either cherry-pick individual samples or select groups to be excluded. Conversely, specific samples can be retained, leading to the exclusion of all others.
	- Real-time visualization of the filtering effects is available via the summary plots.

### Data upload:  
Tab designed for the upload and curation of data files. Upon uploading, several tables are generated and saved. The primary tables among these are:
- Imported Data Table: This serves as the original table and is intended for reset purposes.
- Raw Data Table: the above table filtered by the user, specifically for processing in later steps.
The Raw Data Table is automatically generated using default filtering criteria, retaining only rows from the Raw Metadata Table. There are various derivative tables stemming from this main table. Users can easily select any of these tables from the dropdown menu adjacent to the *Browse* button, with the option to also download them. Both the table preview and data summary dynamically adjust based on the table chosen. For Lipidomics Experiments Only: as filtering is applied to the Raw Data Table, the data summary plot will dynamically display the class distribution in real time.

- **Raw Data Table Filtering**:
**Imputation.** This is designed to fill missing values. Users have the flexibility to skip this step or apply it either pre or post *Blank & Group filtering*.  
- When activated, it functions on individual groups from the *Group column*, utilizing *Imputation Method* and *Minimum Values* defaults, set at 'Median' and 0.6 (60%) respectively.
- If a particular group has a minimum of 60% non-missing values, the absent values within that group will be substituted with the group's median value.
- Note: Imputation before *Blank & Group filtering* may significantly influence the filtering outcome.  
**Blank & Group Filtering.** Its purpose is to weed out features that don't markedly surpass blank signals. It employs the parameters Blank multiplier, Sample threshold, Group threshold, defaulted at 2, 0.8 (80%) and 0.8 (80%) respectively. This is executed in three phases, batch-wise: 1) blank threshold, 2) filtering on the entire dataset and 3) filtering on specific groups within the dataset. 
1. For every batch, a blank threshold is deduced by multiplying the mean blank values by the set multiplier (default being 2). 
2. Filtering on the entire dataset: for a feature to be retained, a minimum of 80% of samples (sample threshold) should register a signal surpassing this blank threshold. If not, the feature undergoes the next test. 
3. If at least 80% of samples within any group of that batch exceed the blank threshold, the feature is preserved. Otherwise, it is omitted from the Raw Data Table.  
Outcomes can be instantly viewed in the preview plot.  
**Normalize to Column.** Once filtering is concluded, signals have the option to be normalized against a numeric column from the metadata.  
**Manual Filtering.** users can either drop or keep individual features from the data. For Lipidomics Experiments Only: features can be picked based on their class.
