
#' KM plot
#'
#' KM plot
#' @param  time   A numeric vector representing the survival time  of a set of samples
#' @param  event  A numeric vector representing the survival status of a set of samples.
#' @param  label  the subtypes label of each sample
#' @param  xlab
#' @param  ylab
#' @export
#' @examples
#' data(GSE62254_data)
#' data1<-apply(GSE62254_gene.expression, 2, function(x) (x-mean(x))/sd(x))
#' A<-affs(data1)
#' Sda<-self.diffusion(A,5)
#' label<-spectralClustering(Sda,4)
#' km.plot(GSE62254_survival_time,GSE62254_survival_event,label,xlab="time",ylab = "OS")

km.plot <- function(time,event,label,xlab,ylab){

  time<-time
  event<-event

  label<-as.integer(label)

  df <- data.frame(time=time, event=event, group=label)

  #browser()
  survstats <- survdiff(Surv(time, event) ~ label,data=df)
  survstats$p.value <- 1 - pchisq(survstats$chisq, length(survstats$n) - 1)

  label<-factor(label)

  fit <- survfit(Surv(time, event) ~ label, data = df)

  color <- (ggsci::pal_npg("nrc"))(length(unique(label)))

  # color<-color

 legend.labs <- na.omit(levels(droplevels(label[!(is.na(time)|is.na(event))])))
 p<- ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = df,             # data used to fit survival curves.
    risk.table = TRUE,       # show risk table.
    pval = FALSE,             # show p-value of log-rank test.
    xlab = xlab, ylab = ylab,
    palette = color, legend = "top",
    legend.title = " ", legend.labs = legend.labs,
    risk.table.title = element_blank(), risk.table.y.text = FALSE,
   # ggtheme = theme(text = element_text(family = "Arial")),
    ggtheme = theme_classic(),
    tables.theme = theme_cleantable()
  )

 p$plot <-p$plot+ annotate("text",x = Inf, y = Inf, label = ifelse(survstats$p.value == 0, "italic(P)<1%*%10^{-22}",
                                                                                  paste0("italic(P)==", fancy_scientific(survstats$p.value
                                                                                                                        ))), hjust = 1.2, vjust = 2, parse = TRUE)
 return(p$plot)

}


fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, digits = 3,scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
