library(readr)
gis_universities <- read_csv("gis_universities.csv")
gis_universities$popup <- ""

for(i in 1:nrow(gis_universities)){
  cn_name = toString(gis_universities$cn[i])
  en_name = toString(gis_universities$name[i])
  s_name = toString(gis_universities$eng[i])
  deg = toString(gis_universities$degrees[i])
  country = toString(gis_universities$country[i])
  link = toString(gis_universities$web[i])
  
  gis_universities$popup[i] = paste0("<center></br><h3>",cn_name,"</h3></center></br>",en_name," (",s_name,")</br>","<b>所在地</b>: ",country,"</br><b>开设学位</b>: ",deg,"</br><a href='",link,"' target='_blank'>项目网页...</a>")
}

