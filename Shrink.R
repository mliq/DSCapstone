#Shrink.R

library(data.table)

x=readRDS("b.all3.Tfreq.RDS")
x=x[counts>=2]
saveRDS(x,file="b.no1counts.RDS")

x=readRDS("n.no1counts.RDS")
x=x[counts>=3]
saveRDS(x,file="n.no2counts.RDS")

x=readRDS("ALL.no2counts.RDS")
x=x[counts>=4]
saveRDS(x,file="ALL.no3counts.RDS")

x=readRDS("n.no3counts.RDS")
x=x[counts>=5]
saveRDS(x,file="n.no4counts.RDS")

t.all3.Tfreq.RDS
n.all3.Tfreq.RDS
b.all3.Tfreq.RDS
ALL.Tfreq.RDS


x=readRDS("n.no4counts.RDS")
x=x[counts>=6]
saveRDS(x,file="n.no5counts.RDS")

x=readRDS("b.no4counts.RDS")
x=x[counts>=6]
saveRDS(x,file="b.no5counts.RDS")

x=readRDS("t.no4counts.RDS")
x=x[counts>=6]
saveRDS(x,file="t.no5counts.RDS")

x=readRDS("ALL.no4counts.RDS")
x=x[counts>=6]
saveRDS(x,file="ALL.no5counts.RDS")