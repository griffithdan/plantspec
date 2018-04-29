load("data_processing/N_cal_shootout.RData")
load("data_processing/shootout.rda")

save(N_cal_shootout, file = "data/N_cal_shootout.RData")
save(shootout_scans, shootout_wetlab, file = "data/shootout.RData")