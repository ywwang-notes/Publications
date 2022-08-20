figname = "fig6"
data = read.table(paste0(figname, ".tsv"), header = TRUE)

# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData4.html
# https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect

chsq = function(dat, simulated){
    results = c()
    results.names = c()
    for (dpa in 1:max(dat["dpa"])){
        # if(sum(dat["dpa"] == dpa) < 2) {next} # can't do chsq if less than 2 rows
        dat.clean = dat[dat["dpa"] == dpa, ! colnames(dat) %in% c("phase", "dpa")] 
        dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,] # remove lines that contain only 0's
        if(nrow(dat.clean) < 2) {next} # can't do chsq if less than 2 rows
        # results = append(results, list(chisq.test(dat.clean, simulate.p.value = TRUE)))
        results[[sprintf("%d dpa", dpa)]] = list(ch = chisq.test(dat.clean, simulate.p.value = simulated), n = sum(dat.clean))
        # results.names = c(results.names, sprintf("%d dpa", dpa))
    }
    # names(results) = results.names

    return(results)
}

all.results = chsq(data, FALSE)
live.results = chsq(data[data["phase"] != 5,], FALSE)

save.image(paste0(figname, ".Rdata"))
