library("jsonlite")
library(dplyr)
library(tidyr)
library(plyr)

main = as.data.frame(fromJSON("nlp.json"))
main = main$`_source`
part_ofdate = data.frame(id = main$nid, date = main$pubdate, cit = main$citation_count)

##Area
max_length = max(sapply(main$fos,length))
fos_filled = as.data.frame(sapply(main$fos, function(x){
   c(x, rep(NA, max_length - length(x)))
}))
colnames(fos_filled) = part_ofdate$id
areas = na.omit(gather(fos_filled, id, area, "9328536":"2611563554"))

part_ofdate$id = as.character(part_ofdate$id)
top_area = left_join(part_ofdate, areas)

field = top_area %>% group_by(area) %>% dplyr::summarise(n=n(), mean = mean(cit))
field$rank = field$n*field$mean
field = arrange(field, desc(rank))
field = field[8:50,] %>% filter(!area %in% c("Data science", "Database", "Medicine", "The Internet", "Ontology", "World Wide Web", "Medicine"))

top_area = top_area %>% filter(area %in% (field$area))

##Authors
max_length = max(sapply(main$authors,length))
author_filled = as.data.frame(sapply(main$authors, function(x){
  c(x, rep(NA, max_length - length(x)))
}))
colnames(author_filled) = part_ofdate$id
authors = na.omit(gather(author_filled, id, author, "9328536":"2611563554"))
authors = authors %>% filter(id %in% top_area$id)

top_author = left_join(authors, top_area)
top_author = top_author %>% group_by(area, author) %>% dplyr::summarise(n=n(), mean = mean(cit))

top_author = top_author %>%
group_by(area) %>%
top_n(n = 3, wt = mean)
top_author = left_join(top_author, authors)

###Org
max_length = max(sapply(main$organisations,length))
org_filled = as.data.frame(sapply(main$organisations, function(x){
  c(x, rep(NA, max_length - length(x)))
}))
colnames(org_filled) = part_ofdate$id
org = na.omit(gather(org_filled, id, org, "9328536":"2611563554"))

top_author = left_join(top_author, org)

##Country
max_length = max(sapply(main$countries,length))
country_filled = as.data.frame(sapply(main$countries, function(x){
  c(x, rep(NA, max_length - length(x)))
}))
colnames(country_filled) = part_ofdate$id
country = na.omit(gather(country_filled, id, country, "9328536":"2611563554"))

top_author = left_join(top_author, country)
top_area = left_join(top_area, country)

write.csv(top_author, file="first.csv")
write.csv(top_area, file="second.csv")

