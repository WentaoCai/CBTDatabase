###  About Cattle Bodymap of Transcriptome(CBT) Databsase

This app (CBT Database) has been built by Wentao Cai(caiwentao@caas.cn).
The current release of CBT Database contains 25,530 known gene, 28,533 novel lncRNA genes, 144,363 novel lncRNA transcripts, 3,093,058 RNA editing and 215,754 splicing events of 52 tissues across three developtmental stages.  


*****

###	Use CBT databsase online
+ The whole app is deveplotment by [`{shiny}`](https://github.com/rstudio/shiny).

+ Data visualisation is mainly done with [`{ggplot2}`](https://github.com/tidyverse/ggplot2)

+ CBT databsase is deployed at <a href="http://47.93.213.166:3838/cattle_bodymap/" target="_blank">http://47.93.213.166:3838/cattle_bodymap/</a> or <a href="http://cattlegenomics.online" target="_blank">http://cattlegenomics.online/</a> (The website is under recordation and will be opened soon)for online use.  


*****

###	Launch CBT databsase directly from R and GitHub

Browse the full source code at https://github.com/wentaocai/CBTDatabase.

User can choose to run CBT databsase installed locally for a more preferable experience.

**Step 1: Install R and RStudio**

Before using the application, you must ensure that R and RStudio are installed on your system.

Please check CRAN (<a href="https://cran.r-project.org/" target="_blank">https://cran.r-project.org/</a>) for the installation of R.  
Please check <a href="https://www.rstudio.com/" target="_blank">https://www.rstudio.com/</a> for the installation of RStudio.  

**Step 2: Install the R Shiny package and other packages required by CBT databsase**

Start an R session using RStudio and run these lines:  
```
# try an http CRAN mirror if https CRAN mirror doesn't work  

install.packages("shiny")  
install.packages("shinydashboard")  
install.packages("shinythemes")  
install.packages("shinycssloaders")
install.packages("data.table")  
install.packages("reshape2")  
install.packages("DT")  
install.packages("ggplot2")  
install.packages("dplyr")  
install.packages("tidyr")  
install.packages("htmlwidgets")  
devtools::install_github("dzhang32/ggtranscript")
```

**Step 3: Start the app**  

Users are suggested to download the source code of CBTDatabase from GitHub (https://github.com/wentaocai/CBTDatabase) to a fixed directory of your computer, such as 'E:\CBTDatabase' on Windows.


Then start an R session using RStudio and run these lines:  
```
library(shiny)  
runApp("E:/CBTDatabase/", launch.browser = TRUE)
```

