splaglm<-function(xt,yt,wyt,listw,method,K,NT,T,inde, zero.policy = FALSE,quiet=quiet, 
    interval = c(-1, 0.999), tol.solve = 1e-10, tol.opt = .Machine$double.eps^0.5,can.sim){
 if (method == "eigen") {
        if (!quiet) 
            cat("Computing eigenvalues ...\n")
			        if (listw$style %in% c("W", "S") && can.sim) {
         					   eig <- eigenw(similar.listw(listw))
				            similar <- TRUE
        										}
			        else eig <- eigenw(listw)
        if (!quiet) 
            cat("\n")
        if (is.complex(eig)) 
            eig.range <- 1/range(Re(eig))
        else eig.range <- 1/range(eig)
      XpX<-crossprod(xt)
		b0<-solve(XpX,crossprod(xt,yt)) ####y on X
		b1<-solve(XpX,crossprod(xt,wyt)) ####Wy on x
		e0<-yt - xt%*% b0
		e1<-wyt - xt%*% b1
		e0e0<-crossprod(e0)
		e1e1<-crossprod(e1)
		e0e1<-t(e1)%*%e0
        opt <- optimize(conclikpan,lower = eig.range[1] + 
            .Machine$double.eps, upper = eig.range[2] - .Machine$double.eps, tol= .Machine$double.eps, maximum = TRUE, eig = eig, e0e0 = e0e0, 
            e0e1 = e0e1, e1e1 = e1e1, N = NT/T, T=T, NT=NT,quiet=quiet)
            #print(opt)
        rho <- opt$maximum
        names(rho) <- "rho"
        LL <- opt$objective
        optres <- opt
    }
    else {
        opt <- dosparsepanel(listw = listw, y = yt, x = xt, wy = wyt, 
            K = K,quiet = quiet, tol.opt = tol.opt, method = method, 
            interval = interval, can.sim = can.sim, zero.policy = zero.policy,NT=NT,T=T)
        rho <- c(opt$maximum)
        names(rho) <- "rho"
        LL <- c(opt$objective)
        similar <- opt$similar
        optres <- opt$opt
    }
	lm.lag <- lm((yt - rho * wyt) ~ xt - 1)
	p <- lm.lag$rank
    r <- residuals(lm.lag)
    fit <- yt - r
    names(r) <- names(fit)
	betas <- coefficients(lm.lag)
	names(betas) <- colnames(xt)
	SSE <- deviance(lm.lag)
	s2 <- SSE/NT
if (method != "eigen") {
#	parm.opr <- c(s2, rho, betas)
#Hess <- optim(parm.opr,fulllik,method="L-BFGS-B",lower=parm.opr - .Machine$double.eps,upper=parm.opr + .Machine$double.eps,
#hessian=TRUE,control=list(maxit=1,fnscale=-1), yt=yt, wyt= wyt, xt= xt, NT= NT, K=K, meth=method)
#Hess <- optim(parm.opr,fulllik,method="SANN",
#hessian=TRUE,control=list(maxit=1), yt=yt, wyt= wyt, xt= xt, NT= NT, K=K)
#res.se <- diag(solve(Hess$hessian))
#print(betas/res.se[-c(1:2)])
#print(rho/res.se[2])
rho.se <- NULL
rest.se <- NULL
asyvar1 <- NULL
    }
    else {
        tr <- function(A) sum(diag(A))
        W <- listw2mat(listw)
        A <- solve(diag(NT/T) - rho * W)
        WA <- W %*% A
        one  <- T*(tr(WA %*% WA) + tr(t(WA) %*% WA))
		  lag<-function(q) trash<-unlist(tapply(q,inde,function(TT) WA %*% TT, simplify=TRUE))
		  lag2<-function(q) trash<-unlist(tapply(q,inde,function(TT) t(WA)%*%TT, simplify=TRUE))
		  WAxt<-apply(xt,2,lag)
        WAWAxt<-apply(WAxt,2,lag2)
        xtWAWAxt <- crossprod(xt,WAWAxt)
        xtWAxt <- crossprod(xt,WAxt)
        xtxt <- crossprod(xt) 
        two <- 1/as.numeric(s2) * t(betas) %*% xtWAWAxt  %*% betas
		  V <- one + two
		  zero <- rbind(rep(0, length(betas)))
        col1 <- rbind(NT/(2 * (s2^2)), T*tr(WA)/s2, t(zero))
        three <- (1/as.numeric(s2)) * xtWAxt %*% betas
        col2 <- rbind(T*tr(WA)/s2, V, three )
        col3 <- rbind(zero, t(three), 1/as.numeric(s2)* xtxt)
        asyvar <- cbind(col1, col2, col3)
        asyv <- solve(asyvar, tol = tol.solve)
		rownames(asyv) <- colnames(asyv) <- c("sigma","rho", colnames(xt))
        rho.se <- sqrt(asyv[2, 2])        
        rest.se <- sqrt(diag(asyv))[-c(1:2)]
        asyvar1 <- asyv[-1,-1]
        rownames(asyvar1) <- colnames(asyvar1) <- c("rho", colnames(xt))
    }
    	return<-list(coeff=betas,rho=rho,s2=s2, rest.se=rest.se, rho.se=rho.se,asyvar1=asyvar1)
}