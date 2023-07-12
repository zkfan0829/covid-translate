# covid-translate
This code is translate from UPenn's Vaccine-Effectiveness-Trial-Emulation project which is runing on PEDSnet
https://github.com/Penncil/Vaccine-Effectiveness-Trial-Emulation

Now this code is works on OMOP UF database,
For rerun this code in OMOP database, please following the step

1. Install ODBC driver (Mac)
https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16
2. set connection details in site/site_info.R
3. install all related package
4. set setwd("/Users/fanz/Desktop/covid-translate//") this project folder path in code/drive_new2.R
5. run code/drive_new2.R
