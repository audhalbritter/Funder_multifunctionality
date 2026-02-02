# adapted function (L66)
glmm.hp_adapted <- function(mod,type = "adjR2",commonality = FALSE)
  {
    # initial checks
    if (!inherits(mod, c("merMod","lme","glmmTMB","glm","lm"))) stop("glmm.hp only supports lme, merMod, glmmTMB or glm objects at the moment")
    if(inherits(mod, "merMod"))
    {# interaction checks
      Formu <- strsplit(as.character(mod@call$formula)[3],"")[[1]]
      if("*"%in%Formu)stop("Please put the interaction term as a new variable (i.e. link variables by colon(:) ) and avoid the asterisk (*) in the original model")
      varname <- strsplit(strsplit(as.character(mod@call$formula)[3],"(",fixed=T)[[1]][1]," ")[[1]]
      ivname <- varname[seq(1,length(varname),2)]
    }

    if(inherits(mod, "lme"))
    {# interaction checks
      Formu <- strsplit(as.character(mod$call$fixed)[3],"")[[1]]
      if("*"%in%Formu)stop("Please put the interaction term as a new variable (i.e. link variables by colon(:)) and  avoid the asterisk (*) in the original model")
      ivname <- strsplit(as.character(mod$call$fixed)[3]," + ",fixed=T)[[1]]
    }

    if(inherits(mod, "glmmTMB"))
    {# interaction checks
      Formu <- strsplit(as.character(mod$call$formula)[3],"")[[1]]
      if("*"%in%Formu)stop("Please put the interaction term as a new variable (i.e. link variables by colon(:)) and  avoid the asterisk (*) and colon(:) in the original model")
      varname <- strsplit(strsplit(as.character(mod$call$formula)[3],"(",fixed=T)[[1]][1]," ")[[1]]
      ivname <- varname[seq(1,length(varname),2)]
    }

    if(inherits(mod, c("glm","lm")))
    {# interaction checks
      Formu <- strsplit(as.character(mod$call$formula)[3],"")[[1]]
      if("*"%in%Formu)stop("Please put the interaction term as a new variable (i.e. link variables by colon(:)) and  avoid the asterisk (*) and colon(:) in the original model")
      ivname=attr(mod$terms, "term.labels")
    }


    iv.name <- ivname
    nvar <- length(iv.name)
    if (nvar < 2)
      stop("Analysis not conducted. Insufficient number of predictors.")

    totalN <- 2^nvar - 1
    binarymx <- matrix(0, nvar, totalN)
    for (i in 1:totalN) {
      binarymx <- creatbin(i, binarymx)
    }

    outr2  <- r.squaredGLMM(mod)
    if(inherits(mod, "lm")&!inherits(mod, "glm"))
    {if(type=="adjR2")outr2  <- summary(mod)$adj.r.squared
    if(type=="R2")outr2  <- summary(mod)$r.squared
    }
    r2type  <-  row.names(outr2)
    nr2type   <-  length(r2type)
    if(nr2type==0)
    {nr2type <- 1
    if(commonality)
    {r2type <- 'commonality.analysis'}
    else
    {r2type <- 'hierarchical.partitioning'}
    }
    #ifelse(class(mod)=="merMod",dat <- eval(mod@call$data),dat <- eval(mod$call$data))
    if(inherits(mod, "merMod"))
    #{dat <- eval(mod@call$data)
    {dat <-mod$frame
    #if(sum(is.na(dat[,ivname]))>0){dat <- dat[-which(rowSums(is.na(dat[,ivname]))>0),]}
    #dat <- na.omit(eval(mod@call$data))
    if(!inherits(dat, "data.frame")){stop("Please change the name of data object in the original (g)lmm analysis then try again.")}
    to_del <- paste(paste("-", iv.name, sep= ""), collapse = " ")
    # reduced formula
    modnull<- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
    mod_null <-  stats::update(object = mod, formula. = modnull, data = dat)
    }

    if(inherits(mod, "lme"))
    {dat <- eval(mod$call$data)
    mod_null <- stats::update(object = mod,data=dat,fixed=~1)
    }


    if(inherits(mod, c("glmmTMB","glm","lm")))
    {dat <- na.omit(eval(mod$call$data))
    if(!inherits(dat, "data.frame")){stop("Please change the name of data object in the original (g)lmm analysis then try again.")}
    to_del <- paste(paste("-", iv.name, sep= ""), collapse = " ")
    # reduced formula
    modnull<- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
    mod_null <-  stats::update(object = mod, formula. = modnull, data = dat)
    }


    outputList  <- list()
    outputList[[1]] <- outr2
    for (k in 1:nr2type)
    {
      commonM <- matrix(nrow = totalN, ncol = 3)
      for (i in 1:totalN) {
        tmp.name <- iv.name[as.logical(binarymx[, i])]
        if(inherits(mod, "merMod")|inherits(mod, "glmmTMB"))
        {to_add <- paste(paste("+", tmp.name, sep= ""), collapse = " ")
        modname <- stats::update(stats::formula(mod_null), paste(". ~ . ", to_add, sep=""))
        modnew  <- stats::update(object = mod_null, formula. = modname, data = dat)
        commonM[i, 2]  <- MuMIn::r.squaredGLMM(modnew)[k,1]
        }

        if(inherits(mod, "lme"))
        {to_add <- paste("~",paste(tmp.name,collapse = " + "),sep=" ")
        modnew  <- stats::update(object = mod_null, data = dat,fixed=to_add)
        commonM[i, 2]  <- MuMIn::r.squaredGLMM(modnew)[k,1]
        }

        if(inherits(mod, "glm"))
        {to_add <- paste("~",paste(tmp.name,collapse = " + "),sep=" ")
        modnew  <- stats::update(object = mod_null, data = dat,to_add)
        commonM[i, 2]  <- MuMIn::r.squaredGLMM(modnew)[k,1]
        }

        if(inherits(mod, "lm")&!inherits(mod, "glm"))
        {to_add <- paste("~",paste(tmp.name,collapse = " + "),sep=" ")
        modnew  <- stats::update(object = mod_null, data = dat,to_add)
        if(type=="adjR2")commonM[i, 2]  <- summary(modnew)$adj.r.squared
        if(type=="R2")commonM[i, 2]  <- summary(modnew)$r.squared
        }

      }

      commonlist <- vector("list", totalN)

      seqID <- vector()
      for (i in 1:nvar) {
        seqID[i] = 2^(i-1)
      }


      for (i in 1:totalN) {
        bit <- binarymx[1, i]
        if (bit == 1)
          ivname <- c(0, -seqID[1])
        else ivname <- seqID[1]
        for (j in 2:nvar) {
          bit <- binarymx[j, i]
          if (bit == 1) {
            alist <- ivname
            blist <- genList(ivname, -seqID[j])
            ivname <- c(alist, blist)
          }
          else ivname <- genList(ivname, seqID[j])
        }
        ivname <- ivname * -1
        commonlist[[i]] <- ivname
      }

      for (i in 1:totalN) {
        r2list <- unlist(commonlist[i])
        numlist  <-  length(r2list)
        ccsum  <-  0
        for (j in 1:numlist) {
          indexs  <-  r2list[[j]]
          indexu  <-  abs(indexs)
          if (indexu != 0) {
            ccvalue  <-  commonM[indexu, 2]
            if (indexs < 0)
              ccvalue  <-  ccvalue * -1
            ccsum  <-  ccsum + ccvalue
          }
        }
        commonM[i, 3]  <-  ccsum
      }

      orderList <- vector("list", totalN)
      index  <-  0
      for (i in 1:nvar) {
        for (j in 1:totalN) {
          nbits  <-  sum(binarymx[, j])
          if (nbits == i) {
            index  <-  index + 1
            commonM[index, 1] <- j
          }
        }
      }

      outputcommonM <- matrix(nrow = totalN + 1, ncol = 2)
      totalRSquare <- sum(commonM[, 3])
      for (i in 1:totalN) {
        outputcommonM[i, 1] <- round(commonM[commonM[i,
                                                     1], 3], digits = 4)
        outputcommonM[i, 2] <- round((commonM[commonM[i,
                                                      1], 3]/totalRSquare) * 100, digits = 2)
      }
      outputcommonM[totalN + 1, 1] <- round(totalRSquare,
                                            digits = 4)
      outputcommonM[totalN + 1, 2] <- round(100, digits = 4)
      rowNames  <-  NULL
      for (i in 1:totalN) {
        ii  <-  commonM[i, 1]
        nbits  <-  sum(binarymx[, ii])
        cbits  <-  0
        if (nbits == 1)
          rowName  <-  "Unique to "
        else rowName  <-  "Common to "
        for (j in 1:nvar) {
          if (binarymx[j, ii] == 1) {
            if (nbits == 1)
              rowName  <-  paste(rowName, iv.name[j], sep = "")
            else {
              cbits  <-  cbits + 1
              if (cbits == nbits) {
                rowName  <-  paste(rowName, "and ", sep = "")
                rowName  <-  paste(rowName, iv.name[j], sep = "")
              }
              else {
                rowName  <-  paste(rowName, iv.name[j], sep = "")
                rowName  <-  paste(rowName, ", ", sep = "")
              }
            }
          }
        }
        rowNames  <-  c(rowNames, rowName)
      }
      rowNames  <-  c(rowNames, "Total")
      rowNames <- format.default(rowNames, justify = "left")
      colNames <- format.default(c("Fractions", " % Total"),
                                 justify = "right")
      dimnames(outputcommonM) <- list(rowNames, colNames)

      VariableImportance <- matrix(nrow = nvar, ncol = 4)
      # VariableImportance <- matrix(nrow = nvar, ncol = 2)
      for (i in 1:nvar) {
        VariableImportance[i, 3] <-  round(sum(binarymx[i, ] * (commonM[,3]/apply(binarymx,2,sum))), digits = 4)
        #VariableImportance[i, 1] <-  round(sum(binarymx[i, ] * (commonM[,3]/apply(binarymx,2,sum))), digits = 4)
      }

      VariableImportance[,1] <- outputcommonM[1:nvar,1]
      VariableImportance[,2] <- VariableImportance[,3]-VariableImportance[,1]

      total=round(sum(VariableImportance[,3]),digits = 4)
      #total=round(sum(VariableImportance[,1]),digits = 4)
      VariableImportance[, 4] <- round(100*VariableImportance[, 3]/total,2)
      #VariableImportance[, 2] <- round(100*VariableImportance[, 1]/total,2)
      #dimnames(VariableImportance) <- list(iv.name, c("Individual","I.perc(%)"))
      dimnames(VariableImportance) <- list(iv.name, c("Unique","Average.share","Individual","I.perc(%)"))

      if(commonality)
      {outputList[[k+1]]<-outputcommonM}

      else
      {outputList[[k+1]]<-VariableImportance}
    }
    names(outputList) <- c("r.squaredGLMM",r2type)
    if(inherits(mod, "lm")&!inherits(mod, "glm")){names(outputList) <- c("Total.R2",r2type)}
    outputList$variables <- iv.name
    if(commonality){outputList$type="commonality.analysis"}
    if(!commonality){outputList$type="hierarchical.partitioning"}
    class(outputList) <- "glmmhp" # Class definition
    outputList
  }
