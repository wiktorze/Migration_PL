rm(list=ls())
# install.packages("pacman")
pacman::p_load(data.table, sf, geosphere, ggplot2)

dt = fread("./Migration_PL/mig_gmina_ext_pop.csv")[, -c("id_dest_new", "id_origin_new")]

boundary = st_read("./gminy/gminy.shp")
boundary = st_transform(boundary, 4326)
boundary = st_make_valid(boundary)
#plot(st_geometry(boundary))

# turn id_dest_new and id_origin_new to character with leading zeros if < 1000000
dt[id_dest < 1000000, id_dest_new := paste0("0", id_dest)]
dt[id_origin < 1000000, id_origin_new := paste0("0", id_origin)]
dt[, id_dest_new := fifelse(is.na(id_dest_new), as.character(id_dest), id_dest_new)]
dt[, id_origin_new := fifelse(is.na(id_origin_new), as.character(id_origin), id_origin_new)]

boundary = boundary[, c("JPT_KOD_JE", "JPT_NAZWA_", "geometry")]
boundary$centroid = st_centroid(boundary)
# plot boundary with centorids
plot(st_geometry(boundary))
plot(st_geometry(boundary$centroid), pch = 4, col = 'red', add = TRUE)

# get latitude and longitude of centroids
boundary$lat_long = st_coordinates(boundary$centroid)
# create matrice with distance between each centroid
distances = distm(boundary$lat_long, fun = distHaversine)
distances = as.data.table(as.matrix(distances))
colnames(distances) = boundary$JPT_KOD_JE
distances$id = boundary$JPT_KOD_JE
head(distances)
setcolorder(distances, c("id", colnames(distances)[1:ncol(distances)-1]))
head(distances)

# melt to long
distances = melt(distances, id.vars = "id", variable.name = "id_dest", value.name = "distance")
head(distances)
# turn to km
distances[, distance := distance/1000]
distances = unique(distances)
# merge with dt
dt_merged = merge(dt, distances, by.x = c("id_origin_new", "id_dest_new"), by.y = c("id", "id_dest"), all.x = T)

# number of unique ids
length(unique(c(dt_merged$id_origin_new)))

# no as a fraction of pop_dest and pop_origin
dt_merged[, no_dest := no/pop_dest]
dt_merged[, no_origin := no/pop_origin]

reg1 = lm(no ~ distance, data = dt_merged)

# get distance that is smaller than 100 km
dt_merged_100 = dt_merged[distance <= 100]
lm(no ~ distance, data = dt_merged_100)

dt_merged_50 = dt_merged[distance <= 50]
lm(no ~ distance, data = dt_merged_50)

# bar plot migration between urban, rural, and urban-rural
pdf("./Migration_PL/mig_gmina_urban_rural.pdf")
urban_rural = dt_merged[, .(no = sum(no)), by = .(urban_dest, urban_origin)]
ggplot(urban_rural, aes(x = urban_dest, y = no, fill = urban_origin)) + 
geom_bar(stat = "identity", position = "dodge") + xlab("Destination") + 
ylab("Number of migrants") +
labs(fill = "Origin")
dev.off()

pdf("./Migration_PL/mig_gmina_distance.pdf")
# add pop_dest to the plot by resizing the points
ggplot(dt_merged, aes(x = distance, y = no, color = urban_dest)) +
  geom_point(alpha = 0.7) +  # Scatter plot with semi-transparent points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Fitted line in red
  xlab("Distance") +  # Customize x-axis label
  ylab("Number of Migrants") +  # Customize y-axis label
  ggtitle("Scatter Plot with Fitted Line") +  # Add a title to the plot
  theme(legend.position = "right")  # Ensure the legend for color is visible
dev.off()

# migration and population - add coefficient number
pdf("./Migration_PL/mig_gmina_pop.pdf")
ggplot(dt_merged, aes(x = pop_dest, y = no_dest)) +
  geom_point(color = "blue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Fitted line in red without confidence interval
  xlab("Population of destination") +  # Customize x-axis label
  ylab("Migrants / population of destination") +  # Customize y-axis label
  ggtitle("Scatter Plot with Fitted Line")  + # Add a title to the plot
  scale_x_continuous(labels = scales::comma) + # x-axis to thousands
    scale_y_continuous(labels = scales::percent) # y-axis to percentage
dev.off()
# Table of rural-urban migration
fwrite(dt_merged[, .(no = sum(no)), by = .(urban_dest, urban_origin)], "./Migration_PL/mig_gmina_urban.csv")
