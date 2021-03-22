PO_Covid<-function(dataset)
{
  dataset%>% 
    ggplot(aes(x = Date, y = Confirmed,colour=Models)) +
    #geom_ribbon(aes(ymin=Confirmed-UB, ymax=Confirmed+UB),alpha=0.2) +
    geom_line(aes(linetype=Models,colour=Models),size=1.1) +
    scale_color_manual(values=c("#000000","#FF0000","#0033FF","#00FF00")) + 
    scale_linetype_manual(values=c("solid", "twodash","dashed","dotdash"))+
    #stat_summary(fun.data=mean_cl_boot, geom="smooth", colour="red")
    #facet_wrap(~State, scales = "free",nrow=2)+
    xlab("Date") +  ylab("Cumulative confirmed cases")+
    theme_bw(base_size = 30,base_family="Times New Roman")+
    theme(legend.direction = "vertical",
          legend.title = element_blank(),
          legend.position = c(0.35, 0.91),
          legend.background = element_rect(fill="transparent",colour=NA),
          legend.text = element_text(size=30),
          axis.text=element_text(size=30),
          axis.text.y = element_text(angle = 90),
          text=element_text(family="Times New Roman"),
          axis.title=element_text(size=30))+
    geom_vline(aes(xintercept=as.numeric(Date[dim(dataset)[1]-6])))+
    annotate("text", x = dataset$Date[dim(dataset)[1]-15], y = min(round(dataset[,1],2)), label = "Training ",size=10,family="Times New Roman")+
    annotate("text", x = dataset$Date[dim(dataset)[1]-2], y = min(round(dataset[,1],2)), label = "Test",size=10,family="Times New Roman")+
    scale_x_date(date_labels = "%d/%m")
}