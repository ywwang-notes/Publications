# use after running chsq.r
# usage: Rscript check_results.r xxx.Rdata

rm(list = ls()) # clear global environment
args = commandArgs(trailingOnly=TRUE)
load(args)
print(all.results)
cat("========== With Dead Larvae ==========\n")
print(live.results)
