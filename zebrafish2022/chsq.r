rm(list = ls())
figname = "fig6"
data = read.table(paste0(figname, ".tsv"), header = TRUE)

# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData4.html
# https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect

chsq = function(dat, gname, simulate.p){
    results = c()
    results.names = c()
    for (dp in min(dat[gname]):max(dat[gname])){
        dat.clean = dat[dat[gname] == dp, ! colnames(dat) %in% c("phase", gname)] 
        dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,] # remove lines that contain only 0's
        if(nrow(dat.clean) < 2) {next} # can't do chsq if less than 2 rows
        results[[sprintf("%d %s", dp, gname)]] = list(ch = chisq.test(dat.clean, simulate.p.value = simulate.p), n = sum(dat.clean))
    }
    return(results)
}

all.results = chsq(data, "dpa", FALSE)
live.results = chsq(data[data["phase"] != 5,], "dpa", FALSE)

# to check if the ratios of dead larvae differ across treatments
# dead.data = data
# dead.data = aggregate(dead.data[,! colnames(dead.data) %in% c("phase", "dpa")], 
#              list(phase=dead.data$phase == 5, dpa=dead.data$dpa), sum)
# dead.results = chsq(dead.data, "dpa", TRUE)

save.image(paste0(figname, ".Rdata"))

