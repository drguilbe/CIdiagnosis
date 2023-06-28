rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
savepath<-""
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

####################
#Load/Organize Data#
####################
d<-read.csv(".../CIP_experimental_final.csv") #complete data path

d = d %>% 
  mutate(
    raw_err1 = response_1 - truth
    , raw_err2 = response_2 - truth
    , raw_err3 = response_3 - truth
    , magn_rev = abs(response_3 - response_1)
    , err1 = abs(raw_err1)
    , err2 = abs(raw_err2)
    , err3 = abs(raw_err3)
    , discrete_correct_1 = discrete_response_1 == discrete_answer
    , discrete_correct_2 = discrete_response_2 == discrete_answer
    , discrete_correct_3 = discrete_response_3 == discrete_answer
    , abs_chg_err = abs(err3 - err1)
    , chg_err = err3 - err1
    , chg_err_2_1= err2 - err1 
    , chg_err_3_1= err3 - err1 
    , chg_err_3_2= err3 - err2
  )

d$acc1<-min_max_norm(d$err1 * -1)
d$acc2<-min_max_norm(d$err2 * -1)
d$acc3<-min_max_norm(d$err3 * -1)

d<-d %>% mutate(
    chg_acc_2_1= acc2 - acc1 
  , chg_acc_3_1= acc3 - acc1 
  , chg_acc_3_2= acc3 - acc2
)

#review n
length(unique(d$subject_id))
d$subj_trial_id<-paste(d$trial, d$subject_id, d$net, sep="_")
length(unique(d$subj_trial_id))
d %>% group_by(net) %>% dplyr::summarise(n=length(unique(subject_id)))

d_valid = d %>%
  subset(!is.na(response_1) & !is.na(response_3)) %>%
  mutate(
    qerr1_level = cut(err1, quantile(err1), include.lowest=TRUE)
    ,qacc1_level = cut(acc1, quantile(acc1), include.lowest=TRUE)
    , abs_chg_err=abs(err3 - err1)
    , chg_err=err3 - err1
  )


length(unique(d_valid$subject_id))
d_valid$subj_trial_id<-paste(d_valid$trial, d_valid$subject_id, d_valid$net, sep="_")
length(unique(d_valid$subj_trial_id))

levels(d_valid$qerr1_level) <- c("Q1", "Q2", "Q3", "Q4")
levels(d_valid$qacc1_level) <- c("Q1", "Q2", "Q3", "Q4")

d_valid_onlyr1 = d %>%
  subset(!is.na(response_1)) %>%
  mutate(
    qerr1_level = cut(err1, quantile(err1), include.lowest=TRUE)
    ,qacc1_level = cut(acc1, quantile(acc1), include.lowest=TRUE)
    , abs_chg_err=abs(err3 - err1)
    , chg_err=err3 - err1
  )

length(unique(d_valid$subject_id))/length(unique(d_valid_onlyr1$subject_id))
length(unique(d_valid_onlyr1$subject_id))/length(unique(d$subject_id))

d_valid_agg = d_valid %>% 
  group_by(net, trial, qacc1_level) %>% 
  dplyr::summarise(
    acc1 = mean(acc1, na.rm=T)
    , acc2 = mean(acc2, na.rm=T)
    , acc3 = mean(acc3, na.rm=T)
    , chg_acc_3_1 = mean(chg_acc_3_1, na.rm=T)
    , chg_err = mean(chg_err,na.rm=T)
    , prop_correct1=sum(discrete_correct_1,na.rm=T)/length(discrete_correct_1)
    , prop_correct3=sum(discrete_correct_3,na.rm=T)/length(discrete_correct_3)
    , chg_prop = prop_correct3 - prop_correct1
    , magn_rev = mean (magn_rev)
    , chg_err_2_1= mean(chg_err_2_1, na.rm=T)
    , chg_err_3_1= mean(chg_err_3_1, na.rm=T) 
    , chg_err_3_2= mean(chg_err_3_2, na.rm=T)
    ) 

###############
#General Stats#
###############

#Table 2 (individual level)
table2<-d_valid %>% group_by(net, qerr1_level) %>% 
  dplyr::summarise(
    acc1 = mean(acc1, na.rm=T)
    , acc2 = mean(acc2, na.rm=T)
    , acc3 = mean(acc3, na.rm=T)
    , chg_acc_3_1 = acc3-acc1
    , prop_correct1=sum(discrete_correct_1,na.rm=T)/length(discrete_correct_1)
    , prop_correct3=sum(discrete_correct_3,na.rm=T)/length(discrete_correct_3)
    , chg_prop = prop_correct3 - prop_correct1
  )

table2

#Table 2 (trial level)
table2_trial<-d_valid %>% group_by(net, trial, qacc1_level) %>% 
  dplyr::summarise(
    acc1 = mean(acc1, na.rm=T)
    , acc2 = mean(acc2, na.rm=T)
    , acc3 = mean(acc3, na.rm=T)
    , chg_acc_3_1 = acc3-acc1
    , prop_correct1=sum(discrete_correct_1,na.rm=T)/length(discrete_correct_1)
    , prop_correct3=sum(discrete_correct_3,na.rm=T)/length(discrete_correct_3)
    , chg_prop = prop_correct3 - prop_correct1
  ) %>% group_by(net, qacc1_level) %>% 
  dplyr::summarise(
    acc1 = mean(acc1, na.rm=T)
    , acc2 = mean(acc2, na.rm=T)
    , acc3 = mean(acc3, na.rm=T)
    , chg_acc_3_1 = acc3-acc1
    , prop_correct1=mean(prop_correct1, na.rm=T)
    , prop_correct3=mean(prop_correct3, na.rm=T)
    , chg_prop = prop_correct3 - prop_correct1
  )

table2_trial

##########
#Figure 2#
##########
d_valid_agg_long <- gather(d_valid_agg, measure, measurement, chg_err_2_1:chg_err_3_2, factor_key=TRUE)
d_valid_agg_long$measure<-as.factor(d_valid_agg_long$measure)
levels(d_valid_agg_long$measure)<-c("Round 1 to 2", "Round 1 to 3", "Round 2 to 3")

fig2 = d_valid_agg_long %>% 
  group_by(qacc1_level, measure) %>% 
  dplyr::summarise(
    diff=wilcox.test(measurement[net=="Control"], measurement[net=="Social"],conf.int=T, conf.level = .95)$estimate,
    lower = wilcox.test(measurement[net=="Control"], measurement[net=="Social"],conf.int=T, conf.level = .95)$conf.int[1],
    upper = wilcox.test(measurement[net=="Control"], measurement[net=="Social"],conf.int=T, conf.level = .95)$conf.int[2], 
    pval = wilcox.test(measurement[net=="Control"], measurement[net=="Social"],conf.int=T, conf.level = .95)$p.val
  ) %>%
  mutate(
    label="Improvement Due to Social Information"
    , diffperc = diff/100
    , lowerperc = lower/100
    , upperperc = upper/100
  )

fig2$revision<-fig2$measure
fig2$revision<-factor(fig2$revision, levels=c("Round 1 to 2", "Round 2 to 3","Round 1 to 3"))

ggplot(subset(fig2, revision != "Round 2 to 3"), aes(x=qacc1_level, y=diffperc, shape=revision)) + 
  geom_point(color="black", size=6, position=position_dodge(0.2)) + theme_bw() + 
  geom_errorbar(aes(ymin=lowerperc, ymax=upperperc), linetype="solid", width=0, size=1.5, position=position_dodge(0.2))+
  facet_grid(.~label) + theme_bw() + theme(axis.text.x = element_text(size=30),
                                           axis.text.y = element_text(size=30),
                                           axis.title.x = element_blank(),
                                           axis.title.y = element_blank(),
                                           strip.text.x = element_blank(), 
                                           legend.text = element_text(size=30),
                                           legend.title = element_blank(),
                                           legend.position="top", 
                                           panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(),
                                           panel.background = element_blank()) + 
  coord_flip() + geom_hline(yintercept=0, linetype=2, size=1) + 
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(-0.22, 0.23)) + 
  scale_x_discrete(limits = unique(sort(fig2$qacc1_level)))


##########
#Figure 3#
##########
d_net<-subset(d_valid, net != "Control")
d_net$init_acc_bin<-ntile(d_net$acc1, 10)

fig3<-d_net %>% group_by(init_acc_bin) %>% 
  dplyr::summarise(revision=mean(magn_rev),
                   cilow=t.test(magn_rev)$conf.int[1],
                   cihi=t.test(magn_rev)$conf.int[2])

ggplot(fig3, aes(x=init_acc_bin, y=revision)) + theme_bw() +
  geom_point(color="black", size=8, position=position_dodge(0.2)) +  
  geom_line(size=1) + ylab("Magnitude of Diagnostic Assessment Revision\n(in Percentage Points)") + 
  xlab("Initial Diagnostic Assessment Accuracy\n(Deciles)") + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), 
                linetype="solid", width=0, size=1, position=position_dodge(0.2))+
  theme_bw() + theme(axis.text.x = element_text(size=40),
                     axis.text.y = element_text(size=40),
                     axis.title.x = element_text(size=35),
                     axis.title.y = element_text(size=35),
                     strip.text.x = element_blank(), 
                     legend.text = element_text(size=30),
                     legend.title = element_blank(),
                     legend.position="top",
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks=seq(1,10,1), labels=seq(1,10,1)) + 
  scale_y_continuous(breaks=c(10,20,30)) + 
  coord_cartesian(ylim=c(3,30))

#######
#Fig 4#
#######
d_valid_agg %>% group_by(net) %>% dplyr::summarise(chg_prop=mean(chg_prop))

fig4 = d_valid_agg %>% 
  group_by(qacc1_level) %>% 
  dplyr::summarise(
    diffneg= mean(chg_prop[net=="Control"], na.rm=T) - mean(chg_prop[net=="Social"], na.rm=T),
    lowerneg = t.test(chg_prop[net=="Control"], chg_prop[net=="Social"],conf.int=T, conf.level = .95)$conf.int[1],
    upperneg = t.test(chg_prop[net=="Control"], chg_prop[net=="Social"],conf.int=T, conf.level = .95)$conf.int[2], 
    pval = t.test(chg_prop[net=="Control"], chg_prop[net=="Social"],conf.int=T, conf.level = .95)$p.val
  ) %>%
  mutate(
    label="Improvement Due to Social Information"
    , diffpos = diffneg * -1
    , upperpos = lowerneg * -1
    , lowerpos = upperneg * -1
  )

fig4$qacc1_level<-factor(fig4$qacc1_level, levels=rev(c("Q4", "Q3",  "Q2", "Q1")))

ggplot(fig4, aes(x=qacc1_level, y=diffpos)) + 
  geom_point(color="black", size=6) +  theme_bw() +
  geom_errorbar(aes(ymin=lowerpos, ymax=upperpos), linetype="solid", width=0, size=1.5)+
  facet_grid(.~label) + theme_bw() + theme(axis.text.x = element_text(size=30),
                                          axis.text.y = element_text(size=30),
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          strip.text.x = element_blank(), 
                                          panel.grid.major = element_blank(), 
                                          panel.grid.minor = element_blank(),
                                          panel.background = element_blank()) + 
  coord_flip() + geom_hline(yintercept=0, linetype=2, size=1) + 
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), 
                     limits=c(-0.25, 0.27)) 

prop_treatment_chg_trial<-d_valid %>% group_by(net, vignette, trial) %>% 
  dplyr::summarise(
    acc1 = mean(acc1, na.rm=T)
    , acc2 = mean(acc2, na.rm=T)
    , acc3 = mean(acc3, na.rm=T)
    , chg_acc_3_1 = acc3-acc1
    , prop_correct1=sum(discrete_correct_1,na.rm=T)/length(discrete_correct_1)
    , prop_correct3=sum(discrete_correct_3,na.rm=T)/length(discrete_correct_3)
    , chg_prop = prop_correct3 - prop_correct1
  ) 

wilcox.test(subset(prop_treatment_chg_trial, net=="Control")$chg_prop)
mean(subset(prop_treatment_chg_trial, net=="Control")$chg_prop, na.rm=T)
wilcox.test(subset(prop_treatment_chg_trial, net!="Control")$chg_prop)
mean(subset(prop_treatment_chg_trial, net!="Control")$chg_prop, na.rm=T)

d_valid$improve_treatment<-ifelse(!d_valid$discrete_correct_1 & d_valid$discrete_correct_3, TRUE, FALSE)
d_valid_init_false<-subset(d_valid, !d_valid$discrete_correct_1)
d_valid_init_false$improve_treatment<-as.numeric(d_valid_init_false$improve_treatment)
cor.test(d_valid_init_false$chg_acc_3_1, d_valid_init_false$improve_treatment, method="spearman")
cor.test(d_valid_init_false$chg_acc_3_1, d_valid_init_false$improve_treatment, method="pearson")

############
#Figure S7A#
############
d_net<-subset(d_valid, net=="Social")

ggplot(d_net, aes(x = err1, y = magn_rev)) + 
  geom_point(size=5, alpha=1, shape=16) +
  #geom_jitter(size=5, alpha=1, shape=16, height=4,width=4) +
  geom_smooth(method='lm', formula= y~x, size=3, se=TRUE, color="red", alpha=0.3) +
  theme_bw() + ggtitle("Network Condition") + 
  theme(plot.title = element_text(size = 40)) + 
  ylab("Magnitude of Revision\n(First to Final Assessment)") +
  xlab("Absolute Initial Error of Diagnostic Assessment\n(Pct. Points from Accurate Response)") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        axis.title.y=element_text(size=40),
        legend.position=c(0.82,0.12), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40)) +
  theme(legend.title=element_blank()) 

cor.test(d_net$err1, d_net$magn_rev)

############
#Figure S7B#
############
ggplot(d_net, aes(x = err1, y = chg_err)) + 
  geom_point(size=5, alpha=1, shape=16) +
  geom_smooth(method='lm', formula= y~x, size=3, se=TRUE, color="red", alpha=0.3) +
  ggtitle("Network Condition") + 
  theme(plot.title = element_text(size = 40)) + theme_bw() + 
  ylab("Change in Absolute Error\n(First to Final Diagnostic Assessment)") +
  xlab("Absolute Initial Error of Diagnostic Assessment\n(Pct. Points from Accurate Response)") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        axis.title.y=element_text(size=40),
        legend.position=c(0.82,0.12), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40)) +
  theme(legend.title=element_blank()) + 
  geom_hline(yintercept = 0, linetype="solid", size=2, color="blue")

cor.test(d_net$err1, d_net$chg_err)

#########
#Fig. S8#
#########
d_agg_l1<-d_valid %>% group_by(trial, net, vignette) %>% 
  dplyr::summarise(
    err1=mean(err1, na.rm=T), 
    err2=mean(err2, na.rm=T), 
    err3=mean(err3, na.rm=T)
  ) 

d_agg_l1$chg_error=d_agg_l1$err3 - d_agg_l1$err1
d_agg_l1$improve<-d_agg_l1$chg_error * -1

wilcox.test(subset(d_agg_l1, net=="Control")$improve,
            subset(d_agg_l1, net!="Control")$improve)

d_agg_l1$vignette_cat<-as.factor(d_agg_l1$vignette)

figs8<-d_agg_l1 %>% group_by(net, vignette) %>%
  dplyr::summarise(
    cilow=t.test(improve)$conf.int[1],
    cihi=t.test(improve)$conf.int[2],
    improve=mean(improve, na.rm=T)) 

figs8$vignette<-as.character(as.numeric(as.factor(figs8$vignette)))
figs8$vignette <- factor(figs8$vignette, levels = c(1,6,7,5,2,3,4))

levels(figs8$vignette)<-c("1","2","3","4","5","6","7")

ggplot(figs8, aes(x = vignette, y = improve, fill = net, group=net, shape=net, ymin=cilow, ymax=cihi)) + 
  geom_point(size=10, aes(colour = net),position=position_dodge(0.2)) + theme_bw() + 
  theme(plot.title = element_text(size = 40)) + 
  ylab("Total Improvement in Diagnostic Assessment\n(in Percentage Points)") +
  xlab("Clinical Vignette") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40),
        legend.title=element_blank(), 
        legend.position="top") + 
  coord_cartesian(ylim=c(0,10))

###########
#Figure S9#
###########
d_net_agg<-d_net %>% group_by(trial, vignette, net) %>% 
  dplyr::summarise(
    mean_response_1 = mean(response_1, na.rm=T),
    mean_response_3 = mean(response_3, na.rm=T),
    mean_err_1 = mean_response_1 - truth,
    mean_err_3 = mean_response_3 - truth,
    mean_chg_err=mean_err_3-mean_err_1, 
    abs_mean_chg_err=abs(mean_err_3) - abs(mean_err_1),
    abs_chg_err=mean(abs_chg_err,na.rm=T), 
    chg_err = mean(chg_err,na.rm=T), 
    final_abs_err = mean(abs(err3),na.rm=T),
  ) %>% group_by(net, vignette) %>% 
  dplyr::summarise(
    abs_chg_err=mean(abs_chg_err), 
    chg_err = mean(chg_err),
    final_abs_err = mean(final_abs_err),
    mean_abs_chg_err=mean(abs_mean_chg_err), 
    mean_chg_err = mean(mean_chg_err),
    mean_final_err = mean(abs(mean_err_3))
  ) %>% group_by(net) %>% 
  dplyr::summarise(
    abs_chg_err=mean(abs_chg_err), 
    chg_err = mean(chg_err),
    final_abs_err = mean(final_abs_err),
    mean_abs_chg_err=mean(mean_abs_chg_err), 
    mean_chg_err = mean(mean_chg_err),
    mean_final_err = mean(mean_final_err)
  ) 

d_net_agg$improve<-d_net_agg$chg_err*-1

#Bootstrap
d_cntrl<-subset(d_valid, net!="Social")

d_cntrl_boot<-data.frame()
for(sim in 1:20){
  print(sim)
  for(nsubjs in seq(2,40,2)){
    d_cntrl_agg<-d_cntrl %>%
      mutate(nsubjs=nsubjs) %>% 
      group_by(trial, vignette, net, nsubjs) %>% 
      sample_n(nsubjs, replace=TRUE) %>% 
      dplyr::summarise(
        abs_chg_err=mean(abs_chg_err,na.rm=T), 
        chg_err = mean(chg_err,na.rm=T),
        final_abs_err = mean(abs(err3),na.rm=T),
        mean_response_1 = mean(response_1, na.rm=T),
        mean_response_3 = mean(response_3, na.rm=T),
        mean_err_1 = truth-mean_response_1,
        mean_err_3 = truth-mean_response_3,
        mean_chg_err=mean_err_3-mean_err_1, 
        abs_mean_chg_err=abs(mean_err_3) - abs(mean_err_1),
      ) %>% group_by(net, vignette, nsubjs) %>% 
      dplyr::summarise(
        abs_chg_err=mean(abs_chg_err), 
        chg_err = mean(chg_err),
        final_abs_err = mean(final_abs_err),
        mean_abs_chg_err=mean(abs_mean_chg_err), 
        mean_chg_err = mean(mean_chg_err),
        mean_final_err = mean(abs(mean_err_3))
      ) %>% group_by(net, nsubjs) %>% 
      dplyr::summarise(
        abs_chg_err=mean(abs_chg_err), 
        chg_err = mean(chg_err),
        final_abs_err = mean(final_abs_err),
        mean_abs_chg_err=mean(mean_abs_chg_err), 
        mean_chg_err = mean(mean_chg_err),
        mean_final_err = mean(mean_final_err)
      )
    d_cntrl_agg$sim<-sim
    d_cntrl_boot<-rbind(d_cntrl_boot, d_cntrl_agg)
  }
}

d_cntrl_boot_long <- gather(d_cntrl_boot, measure, measurement, abs_chg_err:mean_final_err, factor_key=TRUE)

figS9<-d_cntrl_boot_long %>% group_by(net, nsubjs, measure) %>% 
  dplyr::summarise(
    cilow=t.test(measurement)$conf.int[1],
    cihi=t.test(measurement)$conf.int[2],
    measurement=mean(measurement,na.rm=T)
  )

ggplot(subset(figS9, measure=="mean_abs_chg_err"), 
       aes(x = nsubjs, y = measurement, ymin=cilow, ymax=cihi)) + 
  geom_point(size=8, shape = 16) + geom_line(size=2)+
  geom_errorbar(width = 0, size = 1) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 40)) + 
  xlab("Number of Subjects in Control Trial") +
  ylab("Average Change in Absolute Diagnostic Error\n(Group Level)") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        legend.position=c(0.2,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40)) +
  theme(legend.title=element_blank()) + 
  geom_hline(yintercept = d_net_agg$mean_abs_chg_err, color="blue", 
             linetype="dashed", size=3) 

##########################
##Clinician demographics##
##########################
phys_demo<-read.csv(".../CIP_demographics.csv") #complete data path

table(phys_demo$gender)/sum(table(phys_demo$gender))
table(phys_demo$primary)/sum(table(phys_demo$primary))
table(phys_demo$specialty)/sum(table(phys_demo$specialty))
table(phys_demo$state)/sum(table(phys_demo$state))
table(phys_demo$bin_enumeration)/sum(table(phys_demo$bin_enumeration))
table(phys_demo$proprietor)/sum(table(phys_demo$proprietor))
table(phys_demo$credential)/sum(table(phys_demo$credential))

####################
#General Statistics#
####################

#trial count per clinician 
d_trial_count<-d_valid %>% group_by(subject_id) %>% dplyr::summarise(num_trials=length(unique(trial)))
table(d_trial_count$num_trials)/sum(table(d_trial_count$num_trials))

d_trial_count_net<-subset(d_valid, net != "Control") %>% group_by(subject_id) %>% dplyr::summarise(num_trials=length(unique(trial)))
table(d_trial_count_net$num_trials)/sum(table(d_trial_count_net$num_trials))

attrition<-subset(d, !is.na(response_1) & is.na(response_3))
nrow(attrition)/nrow(d)

##Extended table by vignette (supplementary)
d_valid_agg_vignette = d_valid %>% 
  group_by(net, qerr1, vignette) %>% 
  dplyr::summarise(
    acc1=round(mean(acc1, na.rm=T), 2),
    acc3=round(mean(acc3, na.rm=T), 2), 
    chg_acc = acc3 - acc1, 
    prop_correct1=round(sum(discrete_correct_1,na.rm=T)/length(discrete_correct_1), 2), 
    prop_correct3=round(sum(discrete_correct_3,na.rm=T)/length(discrete_correct_3), 2), 
    chg_prop = prop_correct3 - prop_correct1
    )

d_valid_agg_vignette$vignette<-as.character(as.numeric(as.factor(d_valid_agg_vignette$vignette)))
d_valid_agg_vignette$vignette <- factor(d_valid_agg_vignette$vignette, levels = c(1,6,7,5,2,3,4))
levels(d_valid_agg_vignette$vignette)<-c("1","2","3","4","5","6","7")

d_valid_agg_vignette<-d_valid_agg_vignette %>% arrange(vignette, net, qerr1)
d_valid_agg_vignette$net<-as.factor(d_valid_agg_vignette$net)
levels(d_valid_agg_vignette$net)<-c("Control", "Network")
data.frame(d_valid_agg_vignette)



