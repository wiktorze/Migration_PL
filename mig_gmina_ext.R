rm(list=ls())
#install.packages("pacman")
pacman::p_load(data.table, sf)
dt = fread("./Migration_PL/mig_gmina.csv")
#gmina_spatial = st_read("./gminy/gminy.shp")
#plot(st_geometry(gmina_spatial))

gm_cechy = fread("./Migration_PL/powierzchnia_i_ludnosc_w_przekroju_terytorialnym_w_2022_roku_tablice.csv")
# V1 - id, V6 - populacja, V8 - gęstość
gm_cechy = gm_cechy[, .(V1, V6, V8)]
gm_cechy = gm_cechy[V1 != ""]
head(gm_cechy)
tail(gm_cechy)
gm_cechy = gm_cechy[3:nrow(gm_cechy)]

# Rename columns
setnames(gm_cechy, old = colnames(gm_cechy), new = c("id", "pop", "den"))
# Get rid of blank space in id
gm_cechy[, id := gsub(" ", "", id)]
gm_cechy[, id := as.numeric(id)]

# add pop_origin, den_origin
dt = merge(dt, gm_cechy, by.x = "kod_origin", by.y = "id", all.x = T)
colnames(dt)[6] = "pop_origin"
colnames(dt)[7] = "den_origin"
head(dt)

# add pop_dest, den_dest
dt = merge(dt, gm_cechy, by.x = "kod_dest", by.y = "id", all.x = T)
colnames(dt)[8] = "pop_dest"
colnames(dt)[9] = "den_dest"
head(dt)

# control sum
dt[, .(sum(no))]

# save
fwrite(dt, "Migration_PL/mig_gmina_ext.csv")
# mc cp mig_gmina_ext.csv s3/wiktorze/MF/