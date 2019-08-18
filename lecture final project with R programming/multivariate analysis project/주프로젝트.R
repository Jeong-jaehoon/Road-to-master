EPL
attach(EPL)
head(EPL)

corrplot(cor_EPL, method = "ellipse")
vif(lm_1)

lm_1<-lm(value ~ acceleration+aggression+agility+balance+ballcontrol+composure+crossing+curve+dribbling+fkaccuracy+finishing+headingaccuracy+interceptions
         +jumping+longpassing+longshots+marking+penalties+positioning+reactions+shortpassing+shotpower+skillmoves+slidingtackle+sprintspeed+stamina+standingtackle+strength+vision+volleys+weakfoot)
summary(lm_1)
cov.EPL<-cov(EPL[3:32])
cor.EPL<-cor(EPL[3:32])
eigen_EPL_cor<-eigen(cor(cor.EPL))
eigen_EPL_cov<-eigen(cov(cov.EPL))

Difference1<-eigen_EPL_cov$values[-30]-sum(eigen_EPL_cov$values[-1])
Difference2<-eigen_EPL_cor$values[-30]-sum(eigen_EPL_cor$values[-1])

Proportion1<-eigen_EPL_cov$values/sum(eigen_EPL_cov$values)
Proportion2<-eigen_EPL_cor$values/sum(eigen_EPL_cor$values)

Cumulative1<-cumsum(eigen_EPL_cov$values/sum(eigen_EPL_cov$values))
Cumulative2<-cumsum(eigen_EPL_cor$values/sum(eigen_EPL_cor$values))

total1<-data.frame(Eigenvalues=eigen_EPL_cov$values,
                  Difference=c(Difference1,NA),
                  Proportion=Proportion1,
                  Cumulative=Cumulative1)
total2<-data.frame(Eigenvalues=eigen_EPL_cor$values,
                   Difference=c(Difference2,NA),
                   Proportion=Proportion2,
                   Cumulative=Cumulative2)
total1
total2

princomp_EPL_xs<-princomp(EPL[3:32])
princomp_EPL_xs
princomp2_EPL_xs<-princomp(EPL[3:32])
summary(princomp_EPL_xs)
Score<-princomp_EPL_xs$scores
Score

plot(Score[1,1],Score[1,2],pch='*',xlab="Prin1", ylab="Prin2")
text(Score[1,1],Score[1,2], EPL$name, pos=2)
library(ggplot2)

##GGPLOT
p <- ggplot(Score_dt, aes(Score[,1], Score[,2])) +
  geom_point(color = 'red') +
  theme_classic(base_size = 10)

p + geom_text(aes(label = EPL$name),size = 3.5)

install.packages("ggrepel")
require("ggrepel")
set.seed(42)

p + geom_text_repel(aes(label = EPL$name),size = 3.5) 

Score_dt<-as.data.frame(Score)

pc1<-predict(princomp_EPL_xs)[,1]
pc2<-predict(princomp_EPL_xs)[,2]
pc3<-predict(princomp_EPL_xs)[,3]
pc4<-predict(princomp_EPL_xs)[,4]

head(EPL[1])
head(EPL[23])
head(EPL[11])
head(EPL[31])

head(EPL[12])
head(EPL[14])
head(EPL[24])
head(EPL[])

lm_2<-lm(value ~ pc1 + pc2 + pc3 + pc4)
summary(lm_2)

ballcontrol dribbling finishing // interceptions slidingtackle standingtackle

princomp_attack<-princomp(EPL[c(7,11,13)])
princomp_defence<-princomp(EPL[c(15,26,29)])

pc_a1<-predict(princomp_attack)[,1]
pc_a2<-predict(princomp_attack)[,2]
pc_d1<-predict(princomp_defence)[,1]
pc_d2<-predict(princomp_defence)[,2]

lm_3<-lm(value ~ pc_a1+pc_a2+pc_d1+pc_d2+acceleration+aggression+agility+balance+composure+crossing+curve+fkaccuracy+headingaccuracy
         +jumping+longpassing+longshots+marking+penalties+positioning+reactions+shortpassing+shotpower+skillmoves+sprintspeed+stamina+strength+vision+volleys+weakfoot
        )
summary(lm_3)
pc1
data.frame()
predict(lm_2, 549)
