pop.pyramid.bayesPop.pyramid <- function(pop.object, main=NULL, show.legend=TRUE, 
                                         pyr1.par=list(border='black', col=NA, density=NULL, height=0.9),
                                         pyr2.par =list(density=-1, height=0.3), 
                                         col.pi = NULL, ann=par('ann'), axes=TRUE, grid=TRUE, 
                                         cex.main=0.9, cex.sub=1, cex=1, cex.axis=1,
                                         x_at=NULL, x_labels=NULL, legend_pos, legend_text = NULL, ...) {
  cur.par <- par(lend = 1,
                 mar = c(2,3,3,3),
                 oma = c(1,1,1,1))
  if((is.null(pop.object$pyramid) || length(pop.object$pyramid) == 0) && is.null(pop.object$CI)) 
    stop('Nothing to be plotted. Either pyramid or CI must be given in pop.object.')
  age.labels <- rownames(if(!is.null(pop.object$pyramid[[1]])) pop.object$pyramid[[1]] 
                         else {if(!is.null(pop.object$pyramid[[2]])) pop.object$pyramid[[2]] else pop.object$CI[[1]][[1]]$low})
  if(is.null(age.labels)) stop('Row names must be given to determine age labels.')
  lages <- length(age.labels)
  nquant <- length(pop.object$CI[[1]])
  draw.past <- (length(pop.object$pyramid) > 1) && !is.null(pop.object$pyramid[[2]])
  draw.median <- !is.null(pop.object$pyramid[[1]])
  pyr1.par.default <- list(border='black', col=NA, density=NULL, height=0.9)
  for(item in names(pyr1.par.default)) if(is.null(pyr1.par[[item]])) pyr1.par[[item]] <- pyr1.par.default[[item]]
  col.pyr2.default <- adjustcolor('#f46d43', alpha.f=0.3)
  pyr2.par.default <- list(border=col.pyr2.default, col=col.pyr2.default, density=-1, height=0.3)
  for(item in names(pyr2.par.default)) if(is.null(pyr2.par[[item]])) pyr2.par[[item]] <- pyr2.par.default[[item]]
  pyr1.half.height <- pyr1.par[['height']]/2
  pyr2.half.height <- pyr2.par[['height']]/2
  pyr1q.half.height <- max(0.1, min(1, pyr1.half.height + pyr1.half.height/4))
  pyr2q.half.height <- max(0.1, min(1, pyr2.half.height + pyr2.half.height/4))
  cols <- lwd <- legend <- c()
  quantiles <- pop.object$CI[[1]]
  pyr1 <- pop.object$pyramid[[1]]
  pyr2 <- if(draw.past) pop.object$pyramid[[2]] else NULL
  #if(draw.median)
  #	cohort.labels <- if("_cohorts_" %in% colnames(pyr1)) pyr1[,"_cohorts_"] else age.labels
  #else cohort.labels <- if(!is.null(pyr2) && "_cohorts_" %in% colnames(pyr2)) pyr2[,"_cohorts_"] else age.labels
  with(pop.object, {
    maxx <- pop.max	
    proportion <- !is.null(is.proportion) && is.proportion
    male <- LRcolnames[1]
    female <- LRcolnames[2]
    plot(c(-maxx, maxx), c(-0.5, lages-0.5), type='n', axes=FALSE, xlab = "", ylab = "", ann=ann)
    if(ann) mtext(LRmain, at=c(-maxx/2, maxx/2), side=3, cex=cex.sub)
    age.axis.at <- 0:(lages-1)
    labels <- bayesPop:::.get.xtick.labels.for.pyramid(maxx, proportion) 
    xat <- c(-labels, labels[1:length(labels)])
    if(axes) {
      if(is.null(x_at)){
        x_at <- xat
      }
      if(is.null(x_labels)){
        x_labels <- c(labels, labels[1:length(labels)])
      }
      
      if(is.null(legend_text)){
        legend_text <- legend
      }
      axis(1, at=x_at, labels= x_labels, cex.axis=cex.axis)
      axis(2, at=age.axis.at, labels=age.labels, las=2, cex.axis=.9)
      axis(4, at=age.axis.at, labels=age.labels, las=2, cex.axis=.9)
    }
    if(grid) {#grid(length(labels))
      gridxat <- xat[seq(1, length(xat), by=2)]
      segments(gridxat, rep(-0.5-lages/25, length(gridxat)), gridxat, lages-1+lages/25, col="lightgray", lty = "dotted")
      gridyat <- age.axis.at[seq(1, lages, by=2)]
      segments(rep(-maxx-2*maxx/25, length(gridyat)), gridyat, rep(maxx+2*maxx/25, length(gridyat)), 
               gridyat, col="lightgray", lty = "dotted")
    }
    if(nquant > 0) {			
      if(is.null(col.pi)) {
        if(nquant < 10) {
          # from RcolorBrewer: brewer.pal(9, "YlGnBu")
          col.pi.default <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
          if(nquant < 5) # remove the extreme colors at both ends
            col.pi.default <- col.pi.default[2:(length(col.pi.default)-3)]
          cols <- if(nquant == 2) c("#C7E9B4", "#41B6C4") else col.pi.default[sort(seq(length(col.pi.default), 1, length=nquant))]
        } else { # more than 9 PIs
          cols <- rainbow(nquant, start=0.15)
        }
      } else cols <- rep(col.pi, nquant)[1:nquant]
      for(i in 1:nquant) {
        rect(-quantiles[[i]]$high[,male], age.axis.at-pyr1q.half.height, 
             -quantiles[[i]]$low[,male], age.axis.at+pyr1q.half.height, col=cols[i],
             border= NA)
        rect(quantiles[[i]]$low[,female], age.axis.at-pyr1q.half.height, 
             quantiles[[i]]$high[,female], 
             age.axis.at+pyr1q.half.height, col=cols[i], border=NA)
      }
      legend <- c(legend, paste(names(quantiles), '% PI', sep=''))
      lwd <- c(lwd, rep(5, nquant))
    }
    if(draw.median) {
      rect(-pyr1[,male], age.axis.at-pyr1.half.height, rep(0, lages), age.axis.at+pyr1.half.height,
           col=pyr1.par$col, border=pyr1.par$border, density=pyr1.par$density)
      segments(-pyr1[,male], age.axis.at-pyr1.half.height, -pyr1[,male], age.axis.at+pyr1.half.height, 
               col=pyr1.par$border, lwd=3)
      rect(rep(0, lages), age.axis.at-pyr1.half.height, pyr1[,female], age.axis.at+pyr1.half.height, #lwd=2,
           col=pyr1.par$col, border=pyr1.par$border, density=pyr1.par$density)
      segments(pyr1[,female], age.axis.at-pyr1.half.height, pyr1[,female], age.axis.at+pyr1.half.height, 
               col=pyr1.par$border, lwd=3)
      lwd <- c(3, lwd)
      cols <- c(pyr1.par$border, cols)
      legend <- c(names(pyramid)[1], legend)
    }		
    if(draw.past) {
      rect(-pyr2[,male], age.axis.at-pyr2.half.height, rep(0, lages), age.axis.at+pyr2.half.height,
           col=pyr2.par$col, border=pyr2.par$border, density=pyr2.par$density)
      segments(-pyr2[,male], age.axis.at-pyr2.half.height, -pyr2[,male], age.axis.at+pyr2.half.height, 
               col=pyr2.par$border, lwd=3)
      rect(rep(0, lages), age.axis.at-pyr2.half.height, pyr2[,female], age.axis.at+pyr2.half.height,
           col=pyr2.par$col, border=pyr2.par$border, density=pyr2.par$density)
      segments(pyr2[,female], age.axis.at-pyr2.half.height, pyr2[,female], age.axis.at+pyr2.half.height, 
               col=pyr2.par$border, lwd=3)
      legend <- c(legend, names(pyramid)[2])
      cols <- c(cols, pyr2.par$border)
      lwd <- c(lwd, 3)
    }
    lines(c(0,0), c(age.axis.at[1]-pyr1.half.height, age.axis.at[length(age.axis.at)]+pyr1.half.height), col='black')	
    if(show.legend && ann) legend(legend_pos, legend=legend_text, bty='n', fill=cols, border = cols, cex=cex)
    if(is.null(main)) main <- if(exists('label')) label else ""
    if(ann) title(main, line=1, cex.main=cex.main)
  })	
  par(cur.par)
}

.get.xtick.labels.for.pyramid <- function(maxx, proportion){
  if(proportion) {
    rmaxx <- round(maxx*100)
    nticks <- min(rmaxx+1,11)
    dec <- 2
    if(rmaxx <= 5) {
      nticks <- 2*rmaxx + 1
      dec <- 3
    }
    return(round(seq(0, maxx, length=nticks), dec))
  }
  return(round(signif(seq(0, maxx, length=min(11,maxx)),2)))
}