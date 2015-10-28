

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



A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- A[FALSE,]
colnames(A)
A
A <- rbind(A,c("v1",2,0))
A

A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- as.matrix(A)

A <- nulls(1,3) 
A <- A[FALSE,]
colnames(A)
A
A <- rbind(A,c("v1",2,0))
A
A <- data.frame(A)
A

my_add_row <- function(A,a) {
# A: data.frame
# a: row vector that is going to be appended to A   
  A <- as.matrix(A)
  A <- rbind(A,a)
  data.frame(A)
}



A <- nulls(1,3)
colnames(A) <- c("vars","values","type")
A <- A[FALSE,]
A
A <- rbind(A,c("v1",2,0))


A
A <- data.frame(A)
A

A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- A[FALSE,]
A <- as.matrix(A)
A
A <- rbind(A,c("v1",2,0))


df <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

A <- data.frame( vars=character(),
                 values=double(),
                 type=factor())
A <- rbind(A,a)
a <- c("v1",2,0)


A <- data.frame(Column1 = numeric(0), Column2 = numeric(0), Column3=character())
A


A <- data.frame(Column1 = numeric(0))
A
a <- matrix(c(2,0.1113,"B"),nrow=1)
colnames(a) <- c("v1","v2","v3")
A <- rbind(A,a)
A <- rbind(A,a)
A

A <- apply(A[,-3],2,as.numeric)
apply(A,2,round,0)


b <- matrix(c(4,5),nrow=1)
# colnames(b) <- c("v1","v3")
A <- rbind(A,b)
A



A <- data.frame(Row1 = numeric(0))
A
a <- matrix(c(2,0),ncol=1)
rownames(a) <- c("v1","v2")
A <- cbind(A,a)


a <- matrix(c(2,0.113,"abc"),nrow=1)
apply(a,2,round, 2)

apply(a, 2, is.numeric)

apply(a[is.numeric(a)],2,round, 2)



list1 <- c("a","b","c")

A <- list()
lapply(list1, 
       function(x) {
         str <- paste0("A$",x, " <- paste(' ",
                       x, "1')")
         eval(parse(text=str))
       }
)




library(shiny)
runApp(list(
  ui = fluidPage(lapply(1:10, function(i) {
    uiOutput(paste0('x', i))
  })),
  server = function(input, output) {
    lapply(1:10, function(i) {
      output[[paste0('x', i)]] = renderUI(paste('Hi, this is', i))
    })
  }
)) 



x <- c(1:10)

paste("x")


A <- data.frame(Column1 = numeric(0))
A
a <- matrix(c(2,0),nrow=1)
colnames(a) <- c("v1","v2")



