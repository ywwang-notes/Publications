figname = "fig7d"
data = read.table(paste0(figname, ".tsv"), header = TRUE)

# https://www.pnas.org/doi/10.1073/pnas.1915454117
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426219/
# example: https://www.nature.com/articles/nmeth.3031

fisher = function(dat, gname, simulate.p){
    results = c()
    results.names = c()
    for (dpa in 1:max(dat[gname])){
        dat.clean = dat[dat[gname] == dpa, ! colnames(dat) %in% c("phase", gname)] 
        dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,] # remove lines that contain only 0's
        if(nrow(dat.clean) < 2) {next} # can't do chsq if less than 2 rows
        # results[[sprintf("%d %s", dpa, gname)]] = list(ch = chisq.test(dat.clean, simulate.p.value = simulate.p), n = sum(dat.clean))
        results[[sprintf("%d %s", dpa, gname)]] = 
            list(stats = stats::fisher.test(dat.clean, simulate.p.value=simulate.p), n = sum(dat.clean))
    }

    return(results)
}

# fig7e only
# live.data = data[data["phase"] != 5,]
live.data = data
live.data = aggregate(live.data[,! colnames(live.data) %in% c("phase", "dpw")], 
            list(phase=live.data$phase == 3, dpw=live.data$dpw), sum)
results = fisher(live.data, "dpw", TRUE)

# all.results = fisher(data, "dpa", TRUE)
# live.results = fisher(data[data["phase"] != 5,], "dpa", TRUE)

# stats::fisher.test(live.data[live.data$dpw == 2, ! colnames(live.data) %in% c("phase", "dpw")])
# stats::fisher.test(live.data[live.data$dpw == 4, ! colnames(live.data) %in% c("phase", "dpw")])
# save.image(paste0(figname, ".Rdata"))
