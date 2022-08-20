figname = "fig6"
data = read.table(paste0(figname, ".tsv"), header = TRUE)

# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData4.html
# https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect

chsq = function(dat, simulate.p){
    results = c()
    results.names = c()
    for (dpa in 1:max(dat["dpa"])){
        dat.clean = dat[dat["dpa"] == dpa, ! colnames(dat) %in% c("phase", "dpa")] 
        dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,] # remove lines that contain only 0's
        if(nrow(dat.clean) < 2) {next} # can't do chsq if less than 2 rows
        results[[sprintf("%d dpa", dpa)]] = list(ch = chisq.test(dat.clean, simulate.p.value = simulate.p), n = sum(dat.clean))
    }
    return(results)
}

all.results = chsq(data, FALSE)
live.results = chsq(data[data["phase"] != 5,], FALSE)

save.image(paste0(figname, ".Rdata"))
