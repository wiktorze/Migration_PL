rm(list=ls())
# install.packages("pacman")
pacman::p_load(data.table)

dt = fread("./Migration_PL/mig_gmina_ext.csv")

gminy = unique(dt[,.(kod_dest, gmina_dest, pop_dest)])

# suma populacji
gminy[, .(sum(pop_dest, na.rm = T))]
# 35,758,108 - są NA
# Brak gmin miejsko-wiejskich w bazie - porozdzielane na miejskie oraz wiejskie
# co nas zadowala wsm
# Warszawa

# Filter the data.table using the regex pattern
valid_values <- c("1465028", "1465038", "1465048", "1465058", "1465068",
                  "1465078", "1465088", "1465098", "1465108", "1465118",
                  "1465128", "1465138", "1465148", "1465158", "1465168",
                  "1465178", "1465188", "1465198")

# Filter the data.table based on the valid values
filtered_dt <- gminy[kod_dest %in% valid_values]
filtered_dt[, .(sum(pop_dest))]
# Pieknie

# where are the NAs?
dt_na = gminy[is.na(pop_dest)]

# hmmm...
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

# for Wrocław, Łódź, Poznań take 4 first digits of kod_dest and save as kod_new
dt_na[gmina_dest %like% "Wrocław", kod_new := substr(kod_dest, 1, 4)]
dt_na[gmina_dest %like% "Łódź", kod_new := substr(kod_dest, 1, 4)]
dt_na[gmina_dest %like% "Poznań", kod_new := substr(kod_dest, 1, 4)]

# for others take 6 first digits of kod_dest and save as kod_new
dt_na[is.na(kod_new), kod_new := substr(kod_dest, 1, 6)]

# take 6 first digits for gm_cechy
gm_cechy[, kod_new := substr(id, 1, 6)]

# left join dt_na with gm_cechy
dt_na = merge(dt_na, gm_cechy, by = "kod_new", all.x = T)

# Take 4 first digits for gm_cechy
gm_cechy[, kod_new_4 := substr(id, 1, 4)]

# left join dt_na with gm_cechy only for Wrocław, Łódź, Poznań
dt_na = merge(dt_na, gm_cechy, by.x = "kod_new", by.y = "kod_new_4", all.x = T)

# unify pop.x and pop.y, den.x and den.y, id.x and id.y
dt_na[, pop := ifelse(is.na(pop.x), pop.y, pop.x)]
dt_na[, den := ifelse(is.na(den.x), den.y, den.x)]
dt_na[, id := ifelse(is.na(id.x), id.y, id.x)]

# delete the .x and .y columns
dt_na[, c("pop.x", "pop.y", "den.x", "den.y", "id.x", "id.y") := NULL]

# select columns to join the population to the main data.table
dt_na = dt_na[, .(kod_dest, id, pop, den)]
# join the population to the main dt
dt_pop = merge(dt, dt_na, by = "kod_dest", all.x = T)
dt_pop = merge(dt_pop, dt_na, by.x = "kod_origin", by.y = "kod_dest", all.x = T)

# pop_dest = pop.x where pop_dest is NA, den_dest = den.x where den_dest is NA
# pop_origin = pop.y where pop_origin is NA, den_origin = den.y where den_origin is NA
dt_pop[, pop_dest := ifelse(is.na(pop_dest), pop.x, pop_dest)]
dt_pop[, den_dest := ifelse(is.na(den_dest), den.x, den_dest)]
dt_pop[, pop_origin := ifelse(is.na(pop_origin), pop.y, pop_origin)]
dt_pop[, den_origin := ifelse(is.na(den_origin), den.y, den_origin)]
dt_pop[, id_dest := id.x]
dt_pop[, id_origin := id.y]
# remove the .x and .y columns
dt_pop[, c("pop.x", "pop.y", "den.x", "den.y", "id.x", "id.y") := NULL]

# join all other ids
dt_pop[, id_dest := ifelse(is.na(id_dest), kod_dest, id_dest)]
dt_pop[, id_origin := ifelse(is.na(id_origin), kod_origin, id_origin)]

# aggregate by id_dest, id_origin
# Miekinia
dt_pop[id_dest == 218035, id_dest := 218032]
dt_pop[id_dest == 218034, id_dest := 218032]

dt_pop[id_origin == 218035, id_origin := 218032]
dt_pop[id_origin == 218034, id_origin := 218032]

dt_pop[id_dest == 218032, pop_dest := 20313]
dt_pop[id_origin == 218032, pop_origin := 20313]

dt_pop[id_dest == 218032, den_dest := 113]
dt_pop[id_origin == 218032, den_origin := 113]
# Miekinia end
# Dabrowica 1002035 to 1002032
dt_pop[id_origin == 1002035, id_origin := 1002032]

dt_pop[id_origin == 1002032, pop_origin := 1808]

dt_pop[id_origin == 1002032, den_origin := 39]
# Dabrowica end
dt_pop_agg = dt_pop[, .(no = sum(no), pop_dest = first(pop_dest), 
                    den_dest = first(den_dest),
                    pop_origin = first(pop_origin), 
                    den_origin = first(den_origin)), 
                    by = .(id_dest, id_origin)]

# turn everything to numeric
dt_pop_agg[, pop_dest := as.numeric(pop_dest)]
dt_pop_agg[, den_dest := as.numeric(den_dest)]
dt_pop_agg[, pop_origin := as.numeric(pop_origin)]
dt_pop_agg[, den_origin := as.numeric(den_origin)]

dt_pop_agg[, .(sum(pop_origin))]

# Eliminate rows where id_dest == id_origin
dt_pop_agg = dt_pop_agg[id_dest != id_origin]

# control sum
gminy_ctrl = unique(dt_pop_agg[,.(id_dest, pop_dest)])
gminy_ctrl[, .(sum(pop_dest))]
# ponad 2 miliony populacji odzyskane - jest git!

# dzielnice Warszawy
#for ids: 146502 8
#146503 8
#146504 8
#146505 8
#146506 8
#146507 8
#146508 8
#146509 8
#146510 8
#146511 8
#146512 8
#146513 8
#146514 8
#146515 8
#146516 8
#146517 8
#146518 8
#146519 8
#change to 1465011 with population = 1863056 and den = 3602

dzielnice = c("1465028", "1465038", "1465048", "1465058", "1465068",
              "1465078", "1465088", "1465098", "1465108", "1465118",
              "1465128", "1465138", "1465148", "1465158", "1465168",
              "1465178", "1465188", "1465198")
dt_pop_agg[id_dest %in% dzielnice, id_dest := 1465011]
dt_pop_agg[id_origin %in% dzielnice, id_origin := 1465011]

# aggregate again
dt_pop_agg = dt_pop_agg[, .(no = sum(no), pop_dest = first(pop_dest), 
                    den_dest = first(den_dest),
                    pop_origin = first(pop_origin), 
                    den_origin = first(den_origin)), 
                    by = .(id_dest, id_origin)]
dt_pop_agg[, .(sum(no))]
# control sum
gminy_ctrl = unique(dt_pop_agg[,.(id_dest, pop_dest)])
gminy_ctrl[, .(sum(pop_dest))]
# GIT!
# if id consists of 6 digits, add 0 in front
dt_pop_agg[id_dest < 1000000, id_dest_new := paste0("0", id_dest)]
dt_pop_agg[id_origin < 1000000, id_origin_new := paste0("0", id_origin)]
dt_pop_agg[, id_dest_new := fifelse(is.na(id_dest_new), as.character(id_dest), id_dest_new)]
dt_pop_agg[, id_origin_new := fifelse(is.na(id_origin_new), as.character(id_origin), id_origin_new)]
# last number - 
#1 – gmina miejska
#2 – gmina wiejska
#3 – gmina miejsko-wiejska
#4 – miasto w gminie miejsko-wiejskiej
#5 – obszar wiejski w gminie miejsko-wiejskiej
# if last number == 1, urban, if == 2, rural, 3 - urban-rural, 4 - urban in urban-rural, 5 - rural in urban-rural
dt_pop_agg[, urban_dest := fifelse(substr(id_dest_new, 7, 7) == 1, "urban", 
                                   fifelse(substr(id_dest_new, 7, 7) == 2, "rural",
                                           fifelse(substr(id_dest_new, 7, 7) == 3, "urban-rural",
                                                   fifelse(substr(id_dest_new, 7, 7) == 4, "urban",
                                                           fifelse(substr(id_dest_new, 7, 7) == 5, "rural", NA)))))]
# check NAs
dt_pop_agg[is.na(urban_dest)]
# no NAs
dt_pop_agg[, urban_origin := fifelse(substr(id_origin_new, 7, 7) == 1, "urban", 
                                   fifelse(substr(id_origin_new, 7, 7) == 2, "rural",
                                           fifelse(substr(id_origin_new, 7, 7) == 3, "urban-rural",
                                                   fifelse(substr(id_origin_new, 7, 7) == 4, "urban",
                                                           fifelse(substr(id_origin_new, 7, 7) == 5, "rural", NA)))))]

# display number of migrants from urban to rural, rural to urban, urban to urban, rural to rural
dt_pop_agg[, .(no = sum(no)), by = .(urban_dest, urban_origin)]
dt_pop_agg[, .(sum(no))]
# how many are there unique urban ids? rural ids?
nrow(unique(dt_pop_agg[urban_dest == "urban", .(id_dest_new)]))
nrow(unique(dt_pop_agg[urban_dest == "rural", .(id_dest_new)]))
# 944 urban and 2141 rural
# why more? Because we distinguish in rural-urban between urban and rural