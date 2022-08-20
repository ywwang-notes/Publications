dat = read.table("pub.tsv", header = TRUE)

# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData4.html
dat.clean = dat[dat["dpa"] == 1, 2:5] 
dat.clean = dat.clean[apply(dat.clean, 1, sum) > 0,]
chisq.test(dat.clean,correct=FALSE)