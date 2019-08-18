##종합  
##################################################대학생 
tablek<-data.frame(x = univ$졸업후계획 , y = univ$성별)
CrossTable(tablek$x, tablek$y,chisq=T)

tablek<-data.frame(x = univ$졸업후계획 , y = univ$new전공) 
CrossTable(tablek$x, tablek$y,chisq=T)

tablek<-data.frame(x = univ$진로준비1  , y = univ$new전공)
CrossTable(tablek$x, tablek$y,chisq=T)


##################################################취준생  
##pvalue 가 0.06 으로 애매한 수치 
tablek<-data.frame(x = neet$대학진학  , y = neet$성별)
CrossTable(tablek$x, tablek$y,chisq=T)

##pvalue 가 0.09 으로 애매한 수치
tablek<-data.frame(x = neet$취업어려움  , y = neet$성별) 
CrossTable(tablek$x, tablek$y,chisq=T)

tablek<-data.frame(x = neet$회사형태, y = neet$학력)
CrossTable(tablek$x, tablek$y,chisq=T)

##################################################취업청년
#성별에 따른 직장선택주요사항3순위  
tablek<-data.frame(x = officer$A10_3, y = officer$S3)
CrossTable(tablek$x, tablek$y,chisq=T)

#하겱에따른 직장선택시주요사항1순위 
tablek<-data.frame(x = officer$A10_1, y= officer$S7)
CrossTable(tablek$x, tablek$y,chisq=T)

##################################################창업청년  
#성별에 따른 창업여부
tablek<-data.frame(x = startup$A1, y = startup$s3)
CrossTable(tablek$x, tablek$y,chisq=T)
