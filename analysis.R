library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggh4x)

setwd("~/Desktop/git/data")
simulated_data = lapply(list.files(pattern=".rds"), readRDS)
simulated_data = cbind(Package=rep(c("DLMtool", "FLR", "Rpath"), unlist(lapply(simulated_data, nrow))), do.call("rbind", simulated_data))

setwd("~/Desktop/git/result")
result = grep(list.files(pattern=".rds"), pattern='ensemble', invert=T, value=T) %>%
         map(readRDS) %>% 
         bind_rows()

result = right_join(simulated_data, result, by=c("Stock", "yr"))
perf = result %>%
       mutate(conf_rel_biomass=case_when(true_rel_biomass<0.4 & pt_estimate_rel_biomass<0.4 ~ "TP",
                                         true_rel_biomass<0.4 & pt_estimate_rel_biomass>=0.4 ~ "FN",
                                         true_rel_biomass>=0.4 & pt_estimate_rel_biomass<0.4 ~ "FP",
                                         true_rel_biomass>=0.4 & pt_estimate_rel_biomass>=0.4 ~ "TN"),
              conf_bbmsy=case_when(true_bbmsy<1 & pt_estimate_bbmsy<1 ~ "TP",
                                   true_bbmsy<1 & pt_estimate_bbmsy>=1 ~ "FN",
                                   true_bbmsy>=1 & pt_estimate_bbmsy<1 ~ "FP",                                     true_bbmsy>=1 & pt_estimate_bbmsy>=1 ~ "TN")) %>%
       group_by(yr, model, Package) %>% 
       summarise(Proportion_b=sum(true_rel_biomass<0.4)/length(true_rel_biomass),
                 Proportion_bbmsy=sum(true_bbmsy<1)/length(true_bbmsy),
                 Accuracy_b=(sum(conf_rel_biomass=="TN")+sum(conf_rel_biomass=="TP"))/length(conf_rel_biomass),
                 `FN Rate_b`=ifelse(sum(conf_rel_biomass=="TP")+sum(conf_rel_biomass=="FN")==0, NA, 1-sum(conf_rel_biomass=="TP")/(sum(conf_rel_biomass=="TP")+sum(conf_rel_biomass=="FN"))),
                 `FP Rate_b`=ifelse(sum(conf_rel_biomass=="TN")+sum(conf_rel_biomass=="FP")==0, NA, 1-sum(conf_rel_biomass=="TN")/(sum(conf_rel_biomass=="TN")+sum(conf_rel_biomass=="FP"))),
                 Accuracy_bbmsy=(sum(conf_bbmsy=="TN")+sum(conf_bbmsy=="TP"))/length(conf_bbmsy),
                 `FN Rate_bbmsy`=ifelse(sum(conf_bbmsy=="TP")+sum(conf_bbmsy=="FN")==0, NA, 1-sum(conf_bbmsy=="TP")/(sum(conf_bbmsy=="TP")+sum(conf_bbmsy=="FN"))),
                 `FP Rate_bbmsy`=ifelse(sum(conf_bbmsy=="TN")+sum(conf_bbmsy=="FP")==0, NA, 1-sum(conf_bbmsy=="TN")/(sum(conf_bbmsy=="TN")+sum(conf_bbmsy=="FP")))) %>%
       gather(., measure, Value, Accuracy_b:`FP Rate_bbmsy`) %>%
       mutate(ref=if_else(grepl("_bbmsy", measure), "B/BMSY", "B/K"),
              Proportion=if_else(ref=="B/BMSY", Proportion_bbmsy, Proportion_b),
              measure=gsub("_.*", "", measure),
              Package=factor(Package, levels=c("FLR", "DLMtool", "Rpath")),
              model=factor(model, levels=c("CMSY", "COMSIR", "mPRM", "SRA+", "SRA+CMSY", "SSCOM", "zBRT-8", "zBRT-38", "zBRT-average"))) %>%
       ungroup() %>%
       group_by(ref, measure, Package, model) %>%
       summarise(yr=yr,
                 Value=Value,
                 Proportion=Proportion,
                 correlation=format(round(cor(Value-lag(Value), Proportion-lag(Proportion), use="complete.obs"), 2), nsmall=2))

performance_measure = "Accuracy" # or "FN Rate" or "FP Rate"
ref_pt = "B/BMSY" # or "B/K"

if(performance_measure=="Accuracy"){
  ggplot(perf %>% filter(measure==performance_measure, ref==ref_pt), aes(x=yr, y=Value)) +
    geom_errorbar(aes(ymin=0, ymax=Value, col=Value), size=0.65) +
    geom_path(data=perf %>% filter(measure==performance_measure, ref==ref_pt), aes(x=yr, y=Proportion)) +
    scale_color_gradient(low="red", high="limegreen", na.value="black", limits=c(0,1), breaks=c(0,1), labels=c("Poor", "Good")) +
    facet_nested(Package~model, scales="free") +
    scale_y_continuous(breaks=seq(0,1,0.25), limits=c(0, 1.1))+
    scale_x_continuous(breaks=seq(1950,2010,20), limits=c(1945, 2015))+
    xlab('Year') +
    ylab(performance_measure) +
    geom_text(mapping=aes(x=2010, y=1.1, hjust=1, vjust=1, label=correlation), size=5)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(size=16, angle=90, hjust=0.5, vjust=0.5),
          axis.text.y=element_text(size=16),
          axis.title=element_text(size=18),
          legend.text=element_text(size=16),
          strip.text = element_text(size=16))
}else{
  ggplot(perf %>% filter(measure==performance_measure, ref==ref_pt), aes(x=yr, y=Value)) +
    geom_errorbar(aes(ymin=0, ymax=Value, col=1-Value), size=0.65) +
    geom_path(data=perf %>% filter(measure==performance_measure, ref==ref_pt), aes(x=yr, y=Proportion)) +
    scale_color_gradient(low="red", high="limegreen", na.value="black", limits=c(0,1), breaks=c(0,1), labels=c("Poor", "Good")) +
    facet_nested(Package~model, scales="free") +
    scale_y_continuous(breaks=seq(0,1,0.25), limits=c(0, 1.1))+
    scale_x_continuous(breaks=seq(1950,2010,20), limits=c(1945, 2015))+
    xlab('Year') +
    ylab(performance_measure) +
    geom_text(mapping=aes(x=2010, y=1.1, hjust=1, vjust=1, label=correlation), size=5)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(size=16, angle=90, hjust=0.5, vjust=0.5),
          axis.text.y=element_text(size=16),
          axis.title=element_text(size=18),
          legend.text=element_text(size=16),
          strip.text = element_text(size=16))
}


##### Figure 2
ensemble = readRDS("result_ensemble.rds")

ensemble = ensemble %>%
           mutate(model=factor(model, levels=c("CMSY", "COMSIR", "mPRM", "SSCOM", "E-average", "E-LM", "E-GBM", "E-RF")),
                  trained_on=if_else(model%in%c("CMSY", "COMSIR", "mPRM", "SSCOM"), "Constituent Models", trained_on),
                  trained_on=factor(trained_on, levels=c("Constituent Models", "FLR", "DLMtool", "Rpath")),
                  tested_on=factor(tested_on, levels=c("FLR", "DLMtool", "Rpath")),
                  alr=log(pt_estimate_bbmsy/true_bbmsy,10))

ggplot(ensemble, aes(x=model, y=alr, color=model)) + 
  geom_violin(aes(fill=model),draw_quantiles = c(seq(0.25,0.75,by=0.25)), scale="area", adjust=1, col="black", kernel="rectangular") +
  stat_summary(fun=function(x) quantile(x[x>0],0.5), geom="point", pch=24, color="white") +
  stat_summary(fun=function(x) quantile(x[x<0],0.5), geom="point", pch=25, color="white") +
  facet_grid(tested_on~trained_on, scales="free_x") + 
  coord_cartesian(ylim=c(-1,1)) +
  ylab("Log Accuracy Ratio") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        strip.text=element_text(size=16))


##### Figure 3
flr = result %>%
      filter(Package=="FLR", yr>=1957) %>%
      mutate(lar_bbmsy=log(pt_estimate_bbmsy/true_bbmsy,10)) %>%
      group_by(Stock, model) %>% 
      summarise(mean_lar=mean(abs(lar_bbmsy))) %>% 
      group_by(Stock) %>% 
      summarise(best=model[which.min(mean_lar)]) %>%
      mutate(best=factor(best,levels=c("CMSY", "COMSIR", "mPRM", "SRA+", "SRA+CMSY", "SSCOM", "zBRT-8", "zBRT-38","zBRT-average")),
             LH=case_when(grepl("DE_", Stock) ~ "DE",
                          grepl("LP_", Stock) ~ "LP",
                          grepl("SP_", Stock) ~ "SP"),
             ED=case_when(grepl("_ED0_", Stock) ~ "ED0",
                          grepl("_ED0.6_", Stock) ~ "ED0.6",
                          grepl("_OW_", Stock) ~ "OW",
                          grepl("_RC_", Stock) ~ "RC"),
             UR=if_else(grepl("_UR0_", Stock), "UR0", "UR50"),
             AR=if_else(grepl("_AR_", Stock), "AR", "NR"),
             ID=case_when(grepl("_ID0_", Stock) ~ "ID0",
                          grepl("_ID30_", Stock) ~ "ID30",
                          grepl("_ID60_", Stock) ~ "ID60"),
             VAR=if_else(grepl("_VAR02_", Stock), "VAR02", "VAR06")) %>%
      select(-Stock) %>%
      as.data.frame() %>% 
      mutate_if(is.character,funs(factor(.)))

fit <- rpart(best ~., method="class", data=flr)
prp(fit, faclen=0, cex=1, extra=2)
