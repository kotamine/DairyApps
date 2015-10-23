

library(ggplot2)
library(scales)


a <- data.frame("vars"=c("v1","v1","v2", "v2"), "values"=c(1.2,2.1,1.4,1.8),"type"= c(0,1,0,1)) 

ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + 
  # ggtitle("Positive Impacts")+ 
  geom_text(aes(label=values,ymax=max(values)*1.05), vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  scale_fill_brewer(palette="Paired", breaks=c(1,0), labels=c("Robots","Current")) +
  theme_minimal() +
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("Milk \n income"," Labor \n savings")    
  ) + 
  theme(
    axis.title.x=element_blank(), 
    axis.text.x = element_text(hjust=0.5), 
    axis.title.y=element_blank(),  #removes y-axis label
    text=element_text(family="serif", size=12),                       #changes font on entire graph
    plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
    legend.title=element_blank(), 
    legend.position=c(-0.05,-0.05)
  )



aa <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 

ggplot(data=aa, aes(x=vars, y=values, fill=factor(type))) + 
  geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
  coord_flip() +
  geom_text(aes(label=values), vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  theme_minimal() + 
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("var \n 1"," var \n 2")    
  ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.text.x = element_text(hjust=0.5), 
      axis.title.y=element_blank(),  #removes y-axis label
      text=element_text(family="serif", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
      legend.title=element_blank(), 
      legend.position=c(-0.05,-0.05)
    )


aa$label <- apply(cbind(aa$values),2,round,1)
aa$label <- apply(aa$label, 2,formatcomma) 
aa$label <- apply(aa$label, 2, function(x) { paste0("$", x) })

ggplot(data=aa, aes(x=vars, y=values, fill=factor(type))) + 
  geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
  coord_flip() +
  ggtitle("Cost of capital") + 
  geom_text(aes(label=label,ymax=max(values)*1.05), 
            vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  theme_minimal() + 
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("Housing \n recovery","Robot \n recovery")    
  ) + 
  theme(
    axis.title.x= element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(hjust=0.5), 
    axis.title.y=element_blank(),  #removes y-axis label
    text=element_text(family="sans", size=14),                       #changes font on entire graph
    plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
    legend.title=element_blank(), 
    legend.position=c(-0.05,-0.05)
  )



ggplot(data=a, aes(x=vars, y=values, fill=type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=values), vjust=.5, hjust=.5, color="white", size=3) +
  scale_fill_brewer(palette="Reds") + theme_minimal()




