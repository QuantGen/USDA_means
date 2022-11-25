

```r
rm(list=ls())
 USDA=read.csv('https://raw.githubusercontent.com/QuantGen/USDA_means/main/data/mdata.csv',header=TRUE)
 PHENO=read.csv('https://raw.githubusercontent.com/QuantGen/USDA_means/main/data/PHENO.csv',header = TRUE)
 USDA=USDA[USDA$year_loc%in%PHENO$year_loc,]
 PHENO=PHENO[PHENO$year_loc%in%USDA$year_loc,]
 tmp=match(PHENO$year_loc,USDA$year_loc)
 PHENO$usda_yield=USDA$usda_yield[tmp]

library(lme4)
 PHENO$y=scale(PHENO$yield)
 PHENO$x=scale(PHENO$usda_yield)
 fm0=lmer(y~(1|genotype)+(1|year_loc),data=PHENO)
 fm1=lmer(y~(1|genotype)+(1|year_loc)+x,data=PHENO)
 fm2=lmer(y~(1|genotype)+(1|year_loc)+x+(x-1|genotype),data=PHENO)

```
