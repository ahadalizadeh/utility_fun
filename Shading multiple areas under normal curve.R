# Shading multiple areas under normal curve
norm_curve <-   function(zstart = NULL,
                         zend = NULL ,
                         mean = 0,
                         sd = 1,
                         fill = "red",
                         alpha = 1,
                         plot = NULL, 
                         start_lab= NULL, 
                         end_lab= NULL,
                         lowest = -5,
                         highest =5) {
  mean.1 <- mean
  sd.1 <- sd
  library(ggplot2)
  
  if (is.null(zstart))
    zstart = mean.1 + lowest * sd.1
  if (is.null(zend))
    zend = mean.1 + highest * sd.1
  my_col = fill
  x <- seq(from = mean.1 + lowest * sd.1,
           to = mean.1 + highest * sd.1,
           by = .01)
  MyDF <- data.frame(x = x, y = dnorm(x, mean = mean.1, sd = sd.1))
  
  break_lab<- function(x, lab=NULL)
    ifelse(is.null(lab),return(x),return(lab)) # x is zstart or zend
   ord= order(  c(lowest:highest, zstart, zend) )
   
  shade_curve <-
    function(MyDF,
             zstart,
             zend,
             fill = fill,
             alpha = alpha) {
      geom_area(
        data = subset(MyDF, x >=   zstart
                      & x <   zend),
        aes(y = y , x = x),
        fill = fill,
        color = NA,
        alpha = alpha
      )
    }
  
  if (is.null(plot)) {
    p1a <- ggplot() +
      geom_line(data = MyDF, aes(x = x, y = y)) +
      geom_segment(aes(
        x = zstart,
        y = 0,
        xend = zstart,
        yend = dnorm(zstart, mean = mean.1, sd = sd.1)
      )) +
      geom_vline(xintercept = 0, linetype =3)+
      geom_segment(aes(
        x = zend,
        y = 0,
        xend = zend,
        yend = dnorm(zend, mean = mean.1, sd = sd.1)
      )) +
      
      shade_curve(
        MyDF = MyDF,
        zstart = zstart,
        zend = zend,
        fill = my_col,
        alpha = alpha
      ) +
      scale_x_continuous(breaks = sort(c(lowest:highest, zstart, zend)),
                         labels = c(lowest:highest,break_lab(zstart,start_lab) ,
                                    break_lab(zend,end_lab))[ord])+
     
      scale_y_continuous(breaks = NULL) +
      theme_bw() + ylab("") + xlab("") +
      theme(panel.grid = element_blank())
    p1a$nplot <- 1
    p1a$mean.1 <- mean.1
    p1a$sd.1 <- sd.1
    
  } else {
    breaks <- ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks
    labels <- ggplot_build(plot)$layout$panel_scales_x[[1]]$labels
    ord = order(  c(breaks, zstart, zend) )
    nplot =   plot$nplot + 1
    
    p1a =  plot + geom_line(data = MyDF,
                            aes(x = x, y = y),
                            linetype =  plot$nplot) +
      geom_segment(aes(
        x = zstart,
        y = 0,
        xend = zstart,
        yend = dnorm(zstart, mean = mean.1, sd = sd.1)
      ),
      linetype =  plot$nplot) +
      geom_segment(aes(
        x = zend,
        y = 0,
        xend = zend,
        yend = dnorm(zend, mean = mean.1, sd = sd.1)
      ),
      linetype =  plot$nplot) +
      
      shade_curve(
        MyDF = MyDF,
        zstart = zstart,
        zend = zend,
        fill = my_col,
        alpha = alpha
      ) +
      scale_x_continuous(breaks = sort(c(breaks, zstart, zend)),
                         labels = c(labels,break_lab(zstart,start_lab) ,
                                         break_lab(zend,end_lab))[ord])  
    p1a$nplot  <- plot$nplot
    
  }
  p1a
  
}

cat("RUN:\n norm_curve(zstart = NULL, zend = NULL ,\n
                         mean = 0,sd = 1, fill = \"red\", \n
                         alpha = 1, plot = NULL)")
# g =norm_curve  ()  
# g =norm_curve  (zend=1.5, fill="blue", alpha=0.1)  
# g= norm_curve  (   mean.1 = 1, sd.1 = 1, fill="red",plot = g)  
# g= norm_curve  (zstart=2, mean.1 = 0, sd.1 = 1,  fill="green",plot = g, alpha=0.1)  
# g 






