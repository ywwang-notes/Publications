rm(list = ls())
data = read.table("fig5b.tsv", sep = "\t", header = TRUE)
var = colnames(data)
fm <- as.formula(paste(var[1], "~", var[2], "*", var[3]))
print(anova(lm(fm, data = data)))
cat("\n")

# Warning: post-hoc analysis should only be considered when exp-wise interaction is significant
# the following code is just for convenience, not for data torturing
fm <- as.formula(paste(var[1], "~", var[2]))
for (dpa in unique(data[,3]))
{
    cat(var[3], "=", dpa, "\n")
    print(anova(lm(fm, data = data[data[,3]==dpa,])))
    treatment = unique(data[,2])
    treatment.n = length(treatment)
    cat("\n")

    # 1-way ANOVA with two factors is equivalent to two-sample t test
    if (treatment.n > 3){
        for (t1 in 1:(treatment.n - 1))
        {
            for (t2 in (t1+1):treatment.n){
                cat("treatment = ", treatment[t1], treatment[t2], "\n")
                print(anova(lm(fm, data = data[data[,3]==dpa & data[,2] %in% c(treatment[t1],treatment[t2]),])))
                cat("\n")
            }
        }
    }
    cat("\n\n")
}