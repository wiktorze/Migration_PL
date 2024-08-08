# s3/wiktorze/MF/migracje_wewnatrzwojewodzkie_ludnosci_na_pobyt_staly_wg_kierunkow_w_latach_1998_2023_5.xlsx
#  s3/wiktorze/MF/pl_mig_2023_00_2g.xlsx
# s3/wiktorze/MF/migracje_wewnetrzne_ludnosci_na_pobyt_staly_wg_wojewodztw_plci_migrantow_w_latach_1998_2023_5.xlsx
# s3/wiktorze/MF/gminy.zip
# s3/wiktorze/MF/powierzchnia_i_ludnosc_w_przekroju_terytorialnym_w_2023_roku_tablice_errata.csv
rm(list = ls())
#install.packages("pacman")
pacman::p_load(openxlsx, data.table)
dt_gus = read.xlsx("./Migration_PL/pl_mig_2023_00_2g.xlsx")
dt_gus = as.data.table(dt_gus)
head(dt_gus)
# set colnames, select data
dt_gus = dt_gus[2:nrow(dt_gus), 5:(ncol(dt_gus)-2)]
setnames(dt_gus, as.character(dt_gus[1,]))
dt_gus = dt_gus[3:(nrow(dt_gus)-3)]
tail(dt_gus) 
setnames(dt_gus, old = names(dt_gus)[2], new = "gmina_dest")
# turn to long
dt_gus_long = melt(dt_gus, id.vars = c("Kod", "gmina_dest"))

# set names
setnames(dt_gus_long, old = names(dt_gus_long)[3:4], new = c("kod_origin", "no"))
head(dt_gus_long)

# turn to numeric, get rid of 0s and NAs
dt_gus_long[, no := as.numeric(no)]
dt_gus_long = dt_gus_long[!is.na(no)]
gm_kod = unique(dt_gus_long[, .(Kod, gmina_dest)])
head(dt_gus_long)

# suma kontrolna
dt_gus_long[, .(sum(no))]
max(dt_gus_long$no)

# create gmina_origin
colnames(gm_kod)[1] = "kod_origin"
colnames(gm_kod)[2] = "gmina_origin"
dt_gus_long = merge(dt_gus_long, gm_kod, by = "kod_origin")

# change Kod to kod_dest
colnames(dt_gus_long)[2] = "kod_dest"

# change kod with '4' or '5' as the last digit to '3'
replace_last_digit <- function(x) {
  sub("[45]$", "3", x)
}

# Apply the function to the 'kod' column
dt_gus_long[, kod_origin := replace_last_digit(kod_origin)]
dt_gus_long[, kod_dest := replace_last_digit(kod_dest)]

# How many kod_dest ?
length(unique(dt_gus_long$kod_dest))
# Save clean mig_gmina
fwrite(dt_gus_long, "./Migration_PL/mig_gmina.csv")
max(dt_gus_long$no)
