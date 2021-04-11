perform_leave_one_out_analysis <- function(inDF) {
    
    ### perform leave one out analysis, for different vegetation type
    inDF1 <- subset(inDF, Type=="angiosperm")
    inDF2 <- subset(inDF, Type=="gymnosperm")
    
    inDF1 <- inDF1[complete.cases(inDF1$WUE_var),]
    inDF2 <- inDF2[complete.cases(inDF2$WUE_var),]
    
    res1 <- rma.uni(WUE_resp, WUE_var, data=inDF1)
    res2 <- rma.uni(WUE_resp, WUE_var, data=inDF2)
    
    test1 <- leave1out(res1)
    test2 <- leave1out(res2)
    
    test1 <- as.data.frame(test1)
    test2 <- as.data.frame(test2)
    
    ### inspect, visually, which data point is outlier
    ## angiosperm
    plot(test1$estimate)
    abline(a=res1$b,b=0, col="red")
    
    p1 <- ggplot(test1, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res1$b), color="black", 
                   linetype="dashed", size=1)
    
    plot(p1)
    
    # comments: checking the z value and the confidence interval,
    # it doesn't seem that there is an outlier in
    # the angiosperm dataset, although the mean of the raw dataset
    # doesn't match perfectly with the peak of the PDF (estimated
    # based on the leave-one-out analysis). 
    # The mis-match indicates a likelihood of low value outlier
    # in the dataset. We can remove one point to recheck the match.
    
    # check which VPD value the potential outlier is
    removedDF <- inDF1[which.min(test1$estimate),]
    
    # remove it and check effect
    inDF1 <- inDF1[-which.min(test1$estimate),]
    res1 <- rma.uni(WUE_resp, WUE_var, data=inDF1)
    test1 <- leave1out(res1)
    test1 <- as.data.frame(test1)
    plot(test1$estimate)
    abline(a=res1$b,b=0, col="red")
    p1 <- ggplot(test1, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res1$b), color="black", 
                   linetype="dashed", size=1)
    
    plot(p1)
    
    # comments: no significant effect observed, we don't want to remove it
    inDF1 <- rbind(inDF1, removedDF)
    
    ## angiosperm
    plot(test2$estimate)
    abline(a=res2$b,b=0, col="red")
    
    p2 <- ggplot(test2, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res2$b), color="black", 
                   linetype="dashed", size=1)
    
    plot(p2)
    
    # there is likely a high value
    removedDF <- inDF2[which.max(test2$estimate),]
    
    # remove it and check effect
    inDF2 <- inDF2[-which.max(test2$estimate),]
    res2 <- rma.uni(WUE_resp, WUE_var, data=inDF2)
    test2 <- leave1out(res2)
    test2 <- as.data.frame(test2)
    plot(test2$estimate)
    abline(a=res2$b,b=0, col="red")
    p2 <- ggplot(test2, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res2$b), color="black", 
                   linetype="dashed", size=1)
    
    plot(p2)

    # comments: still no significant effect observed, we don't want to remove it
    inDF2 <- rbind(inDF2, removedDF)
    
    ###############################################################################
    
    ### Now, let's look at PFT specific response
    inDF1 <- subset(inDF, PFT=="ENF")
    inDF2 <- subset(inDF, PFT=="DBF")
    inDF3 <- subset(inDF, PFT=="EBF")
    
    ### repeat the same analysis above and check effect of outlies
    inDF1 <- inDF1[complete.cases(inDF1$WUE_var),]
    inDF2 <- inDF2[complete.cases(inDF2$WUE_var),]
    inDF3 <- inDF3[complete.cases(inDF3$WUE_var),]
    
    res1 <- rma.uni(WUE_resp, WUE_var, data=inDF1)
    res2 <- rma.uni(WUE_resp, WUE_var, data=inDF2)
    res3 <- rma.uni(WUE_resp, WUE_var, data=inDF3)
    
    test1 <- leave1out(res1)
    test2 <- leave1out(res2)
    test3 <- leave1out(res3)
    
    test1 <- as.data.frame(test1)
    test2 <- as.data.frame(test2)
    test3 <- as.data.frame(test3)
    
    ### inspect, visually, which data point is outlier
    ## ENF
    plot(test1$estimate)
    abline(a=res1$b,b=0, col="red")
    p1 <- ggplot(test1, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res1$b), color="black", 
                   linetype="dashed", size=1)
    plot(p1)
    
    ## DBF
    plot(test2$estimate)
    abline(a=res2$b,b=0, col="red")
    p2 <- ggplot(test2, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res2$b), color="black", 
                   linetype="dashed", size=1)
    plot(p2)
    
    ## EBF
    plot(test3$estimate)
    abline(a=res3$b,b=0, col="red")
    p3 <- ggplot(test3, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res3$b), color="black", 
                   linetype="dashed", size=1)
    plot(p3)
    
    ### Not sure if the leave-one-out analysis yields anything useful at this stage.
    ### We can further divide the data into different season but I suspect that it
    ### won't help. 
    ### Currently the analysis is based on VPD bin of 0.4 kPa. Maybe a different
    ### bin size may change the result.
    ### What can be done simpler, is to plot a density plot of VPD for each individual
    ### dataset. That will give a distribution of VPD values, which would identify outliers
    ### of VPD. Then we can simply remove these points, and then group the
    ### remaining VPD into different bins. 
    ### Check its effect too.
    
    
    
    
}