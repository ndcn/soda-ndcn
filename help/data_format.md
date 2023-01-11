Formatting your data prior to upload
=======================
SODA accepts for now CSV formatted files. Please make sure your files do no contain comma (,) or any other special characters.<br><br>
Files to upload include:
* Sample metadata
* Lipidomics data

All these tables should contain a shared ID column with which samples can be connected between tables. The data type of ID can be integer or string, as long as the IDs are unique an can be used to communicate between tables (i.e. having the IDs being the same from one table to the other).

## Sample metadata
Here the shared ID column is named "ID". Each ID has associated metadata, that can even be left empty in some cases.

| ID    | Group  | Cell_type |
|-------|--------|-----------|
| S0001 | Sample | A         |
| S0002 | Sample | B         |
| S0003 | Sample | C         |
| S0004 | Blank  |           |
| S0005 | QC     |           |

## Lipidomics data
In relation to the above table, this one also has an ID column. Other columns will include the observed feature for each row.

| ID    | feature_1 | feature_2 |
|-------|-----------|-----------|
| S0001 |           | 2.659393  |
| S0002 | 0.072669  | 3.242203  |
| S0003 | 0.072236  | 2.940228  |
| S0004 |           | 0.93373   |
| S0005 |           | 1.040963  |
