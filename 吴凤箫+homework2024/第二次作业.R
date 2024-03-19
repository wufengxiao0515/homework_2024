#---------------------------------------------------------------------------------
#script Name: pak_into R
#Purpose: This displays the "tidyverse" package's functions,and get help document.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-03-13
#---------------------------------------------------------------------------------

#finding and selecting packages
install.packages("tidyverse")
library(tidyverse)

#helping yourself
help(package="tidyverse")

#Vignettes Demonstrations
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")

#Searching for help
apropos("tidyverse")
ls("package:tidyverse")
help.search("tidyverse")

#--------------------------------------------------------------------------------