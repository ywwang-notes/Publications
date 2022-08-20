dat = read.table("pub.tsv", header = TRUE)

# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData4.html

all.results = c()

# with dead fish
for (dpa in 1:6){
    dat.clean = dat[dat["dpa"] == dpa, 2:5] 
    dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,]
    all.results[[dpa]] = chisq.test(dat.clean,correct=FALSE)
}

# without dead fish
live.results = c()
# with dead fish
for (dpa in 1:6){
    dat.clean = dat[dat["phase"] != 5 & dat["dpa"] == dpa, 2:5]
    dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,]
    dat.clean = dat.clean[]
    live.results[[dpa]] = chisq.test(dat.clean,correct=FALSE)
}
