# 
# --- SULTRA 2017 BIG FIVE PLOTS
# 


Sultra.BigFive.status <- 
  rbind.data.frame(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1,],
                   as.data.frame(matrix(rep(NA,14),ncol=14,dimnames=list(NULL,colnames(BigFive.SettleGroup)))),
                   BigFive.MPAGroup) %>%
  


# - FOOD SECURITY
BHS.fs.statusplot <- 
  rbind.data.frame(BHS.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(MPAID=NA,MPAName="  ",
                                    matrix(rep(NA,21),ncol=21,
                                           dimnames=list(NULL,
                                                         colnames(BHS.ContData.Techreport.status.PLOTFORMAT)[3:23])),
                                    MPALevel="Dummy")) %>%
  ggplot(aes(x=MPAName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=FS),
            label=BHS.statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=FS.ref),
            label=BHS.statusplot.asterisks$FS.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  geom_text(aes(x=length(MPAName),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["FS"] + theme(axis.ticks=element_blank(),
                                                        panel.background=element_rect(fill="white",
                                                                                      colour="#909090"),
                                                        panel.border=element_rect(fill=NA,
                                                                                  size=0.25,
                                                                                  colour="#C0C0C0"),
                                                        panel.grid.major.x=element_blank(),
                                                        panel.grid.major.y=element_blank(),
                                                        axis.title=element_text(size=10,
                                                                                angle=0,
                                                                                face="bold",
                                                                                colour="#303030"),
                                                        axis.text=element_text(size=8,
                                                                               angle=0,
                                                                               colour="#303030"))

# - MATERIAL ASSETS
BHS.ma.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=MAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=MA),
            label=BHS.statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=MA.ref),
            label=BHS.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(BHS.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
BHS.pa.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=PAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=PA),
            label=BHS.statusplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=PA.ref),
            label=BHS.statusplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["PA"] + plot.theme

# - MARINE TENURE
BHS.mt.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=MTMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=MT+(0.05*MT)),
            label=BHS.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=MT.ref),
            label=BHS.statusplot.asterisks$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
BHS.se.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=SEMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=SE),
            label=BHS.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=SE.ref),
            label=BHS.statusplot.asterisks$SE.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1.1)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["SE"] + plot.theme
