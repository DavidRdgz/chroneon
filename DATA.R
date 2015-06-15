source("features.R")
library(ggplot2)

DATA <- function(my.df = 0)
{
        data <- list(
            raw.data = my.df,
            feature.data = 0,
            feature.matrix = 0,
            train.index = 0,
            test.index = 0,
            feature.pca.matrix = 0,
            pca = 0
       )

        ## Set the name for the class
        class(data) <- append(class(data),"DATA")
        return(data)
}

add.label <- function(df, label){
	data.frame(df, list(l = rep(strsplit(label, "_")[[1]][5], nrow(df))))
}

set_raw_dataa <- function(ob, ...) UseMethod("set_raw_dataa")
set_raw_dataa.DATA<- function(ob, dir,  ...){ 
    args <- list(...)
    object <- ob
    setwd(dir)

    tmp.list <- list()
    gesture.tmp <- c() 
    counter <- 1
    for (file in list.files()){
        tmp1 <- read.table(gsub("/", "_", file), header = TRUE, sep = ",")
        print(ncol(tmp1))
        tmp.list[[file]] <- as.matrix(tmp1[,3:10])
    }

    setwd("../..")
    object$raw.data <- tmp.list
    return(object)
}

get_gestures <- function(ob, ...) UseMethod("get_gestures")
get_gestures.DATA <- function(ob, ...){
    args <- list(...)
    object <- ob
    names(object$raw.data)
}

set_feature_data <- function(ob, window, slide, ...) UseMethod("set_feature_data")
set_feature_data.DATA <- function(ob, window, slide, ...){
    args <- list(...)
    object <- ob

    tmp.list <- list() 
    for (i in names(object$raw.data)){
        tmp.list[[i]] <- feature_matrix(object$raw.data[[i]], window, slide) 
    }
    object$feature.data <- tmp.list
    return(object)
}

set_feature_matrix <- function(ob,...) UseMethod("set_feature_matrix")
set_feature_matrix.DATA  <- function(ob, ...){
    args <- list(...)
    object <- ob

    object$feature.matrix <-dfs_label(object$feature.data) 
    return(object)
}

set.pca <- function(ob, ...) UseMethod("set.pca")
set.pca.DATA <- function(ob, ...) {
    args <- list(...)
    object <- ob

    d <- object$feature.matrix
    
    x.train <- d[, !names(d) %in% c("l")]
    pr.d <- prcomp(x.train, scale = TRUE)
    x.train <- predict(pr.d, x.train)
    x.train <- round(x.train, 4)
    object$feature.pca.matrix <- cbind(as.data.frame(x.train),"l"= d$l)
    return(object)
}

get.pca <- function(ob, ...) UseMethod("get.pca")
get.pca.DATA <- function(ob, ...) {
    args <- list(...)
    object <- ob

    d <- object$feature.matrix
    
    x.train <- d[, !names(d) %in% c("l")]
    pr.d <- prcomp(x.train, scale = TRUE)
    object$pca <- pr.d
    return(object)
}

