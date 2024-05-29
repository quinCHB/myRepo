# State Community Health Services Advisory Committee as of 1_17_2024
df_schsacRaw <- read.csv("https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv") |> 
  janitor::clean_names()

# Community Health Board as of 1_17_2024
df_chbRaw <- read.csv("https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv") |> 
  janitor::clean_names()

#Some CHB's are at the city level and not the county level which is why I do a full join
# And I still want them to display in the global selectInput choice
df_globalSchsacChb <- df_schsacRaw |> 
dplyr::full_join(df_chbRaw, by = dplyr::join_by(county == county)) 

# When I source this I only want to pull the finally data frame which is why I remove the following
rm(df_chbRaw)
rm(df_schsacRaw)