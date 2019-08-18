sperrorlm<-function(xt,yt,wyt,WX,listw,method,K,NT,T,zero.policy = FALSE,quiet=quiet, 
    interval = c(-1, 0.999), tol.solve = 1e-10, tol.opt = .Machine$double.eps^0.5,can.sim){
	if (method == "eigen") {
        if (!quiet) 
            cat("Computing eigenvalues ...\n")
        if (listw$style %in% c("W", "S") & can.sim) {
            eig <- eigenw(similar.listw(listw))
            similar <- TRUE
        }
        else eig <- eigenw(listw)
        if (!quiet) 
            cat("\n")
        if (is.complex(eig)) 
            eig.range <- 1/range(Re(eig))
        else eig.range <- 1/range(eig)
        opt <- optimize(sarpanelerror, lower = eig.range[1] + .Machine$double.eps, 
            upper = eig.range[2] - .Machine$double.eps, maximum = TRUE, 
            tol = tol.opt, eig = eig, y = yt, wy = wyt, x = xt, 
            WX = WX, n = NT/T, NT=NT,T=T, quiet = quiet)
        lambda <- opt$maximum
        names(lambda) <- "lambda"
        LL <- opt$objective
        #print(opt)
    }
    else if (method == "spam") {
        if (listw$style %in% c("W", "S") & can.sim) {
            csrw <- listw2U_spam(similar.listw_spam(listw))
            similar <- TRUE
        }
        else csrw <- as.spam.listw(listw)
        I <- diag.spam(1, NT/T, NT/T)
        opt <- optimize(sarpanelerror.sp, interval = interval, 
            maximum = TRUE, tol = tol.opt, csrw = csrw, I = I, 
            y = yt, wy = wyt, x = xt, WX = WX, n = NT/T, NT=NT,T=T, quiet = quiet)
        lambda <- opt$maximum
        names(lambda) <- "lambda"
        LL <- opt$objective
    }
    else if (method == "Matrix") {
        if (listw$style %in% c("W", "S") & can.sim) {
            csrw <- listw2U_Matrix(similar.listw_Matrix(listw))
            similar <- TRUE
        }
        else csrw <- as_dsTMatrix_listw(listw)
        csrw <- as(csrw, "CsparseMatrix")
        I <- as_dsCMatrix_I(NT/T)
        opt <- optimize(sarpanelerror.M, interval = interval, maximum = TRUE, 
            tol = tol.opt, csrw = csrw, I = I, y = yt, wy = wyt, 
            x = xt, WX = WX, n = NT/T, NT=NT,T=T, quiet = quiet)
        lambda <- opt$maximum
        names(lambda) <- "lambda"
        LL <- opt$objective
    }
    lm.target <- lm(I(yt - lambda * wyt) ~ I(xt - lambda * WX) - 
        1)
    r <- as.vector(residuals(lm.target))
    p <- lm.target$rank
    s2 <- crossprod(r)/NT
    rest.se <- (summary(lm.target)$coefficients[, 2]) * sqrt((NT - p)/NT)     
    betas <- coefficients(lm.target)
    names(betas) <- colnames(xt)  
    if (method == "eigen") {
        tr <- function(A) sum(diag(A))
        W <- listw2mat(listw)
        A <- solve(diag(NT/T) - lambda * W)
        WA <- W %*% A
        asyvar <- matrix(0, nrow = 2 + p, ncol = 2 + p)
        asyvar[1, 1] <- NT/(2 * (s2^2))
        asyvar[2, 1] <- asyvar[1, 2] <- T*tr(WA)/s2
        asyvar[2, 2] <- T*(tr(WA %*% WA) + tr(t(WA) %*% WA))
        asyvar[3:(p + 2), 3:(p + 2)] <- 1/as.numeric(s2) * (t(xt - lambda *WX) %*% (xt - lambda * WX)) 
        asyv <- solve(asyvar, tol = tol.solve)
        rownames(asyv) <- colnames(asyv) <- c("sigma","lambda", colnames(xt))
        lambda.se <- sqrt(asyv[2, 2])
        asyvar1 <- asyv[-1,-1]
        rownames(asyvar1) <- colnames(asyvar1) <- c("lambda", colnames(xt))
    }
    else{
    	    lambda.se <- NULL
    asyvar1 <- matrix(,(length(betas)),(length(betas)))
    diag(asyvar1) <- rest.se
        rownames(asyvar1) <- colnames(asyvar1) <- colnames(xt)
}
	return<-list(coeff=betas,lambda=lambda,s2=s2, rest.se=rest.se, lambda.se=lambda.se,asyvar1=asyvar1)
}