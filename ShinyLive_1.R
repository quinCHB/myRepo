# Thank you Rami Krispin
# Below is from his tutorial
# https://github.com/RamiKrispin/shinylive-r/tree/main


# install.packages("pak")
#pak::pak("posit-dev/r-shinylive") #Dev shinyLive seemed to not work 0.3.0. (0.2.3 does work 4/23/24)
pak::pak("rstudio/httpuv") # dev httpuv worked with CRAN shinylive 
#library(httpuv)
library(shinylive)
#library(httpuv)

#pak::pkg_remove("shinylive")
#pak::pkg_remove("httpuv")
#install.packages("shinylive")
#install.packages("httpuv")

#shinylive::assets_version()

# Version for the packages
# Uncomment the next two lines to see versions
 # packageVersion("shinylive")
 # packageVersion("httpuv")
 

shinylive::export(appdir = "app", destdir = "docs")


# Thank you Ellis Hughes and Patrick Ward for your tutorial
# https://www.youtube.com/watch?v=B3Nggr9X4rY&t=239s

#See deployed application locally
httpuv::runStaticServer( dir="docs", port= 8888)

