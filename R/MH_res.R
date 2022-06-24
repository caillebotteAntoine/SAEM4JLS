
#' Metropolis Hastings Object
#'
#' @field value matrix.
#' @field acceptation data.frame.
#' @field chain data.frame.
#'
#' @return
#' @export
#'
#' @examples
MH_res <- setClass(
  Class = "MH_res",
  contains = "matrix",
  slots = list( acceptation = 'matrix',
                chain = 'matrix')
)

#--- Show ---#
setMethod(show, 'MH_res',
          function(object) show(as(object, 'matrix')) )

#--- Getter ---#
setMethod(`$`, 'MH_res', function(x, name){
  switch( EXPR = name ,
          'acceptation' = x@acceptation,
          'chain' = x@chain ) })

#--- addacceptation ---#
setGeneric('addacceptation', function(object, a) standardGeneric("addacceptation" ) )
setMethod(addacceptation, signature = c('MH_res', 'matrix'),
          function(object, a) { object@acceptation <- rbind(a, t(a[nrow(a),] + t(object@acceptation))) ; object} )
setMethod(addacceptation, signature = c('MH_res', 'numeric'),
          function(object, a) { object@acceptation <- c(a, a[length(a)] + object@acceptation) ; object} )
setMethod(addacceptation, signature = c('MH_res', 'integer'),
          function(object, a) { object@acceptation <- c(a, a[length(a)] + object@acceptation) ; object})


#--- addchain ---#
setGeneric('addchain', function(object, c) standardGeneric("addchain" ) )
setMethod(addchain, signature = c('MH_res', 'matrix'),
          function(object, c) { object@chain <- rbind(c, object@chain) ; object})

#--- setoffset ---#
setGeneric('setoffset', function(object, offset) standardGeneric("setoffset" ) )
setMethod(setoffset, signature = c('MH_res', 'numeric'),
          function(object, offset){
            if(ncol(object@chain)!=0)
              object@chain[,1:ncol(object)] <- t(t(object@chain[,1:ncol(object)]) + offset)

            return(t(t(object)+ offset))
          } )
setMethod(setoffset, signature = c('matrix', 'numeric'), function(object, offset) return(t(t(object)+ offset)) )



#--- link ---#
setGeneric('link', function(object1, object2) standardGeneric("link" ) )
setMethod(link, c('matrix', 'MH_res'), function(object1, object2) object2 )

setMethod(link, c('MH_res', 'MH_res'),
          function(object1, object2){
            x <- object2

            x <- SAEM4JLS::addacceptation(x, object1$acceptation)
            x <- SAEM4JLS::addchain(x, object1$chain)
            return(x)
          } )

#--- getacceptation ---#
setGeneric('getacceptation', function(object) standardGeneric("getacceptation" ) )
setMethod(getacceptation, 'MH_res',
          function(object){
            dim <- base::nrow(object)
            iter <- 1:base::nrow(object$acceptation)

            if(ncol(object$acceptation) == dim) dim = 1

            data.frame(rate = object$acceptation/(iter*dim),
                       iteration = iter)
          } )

#--- getchain ---#
setGeneric('getchain', function(object) standardGeneric("getchain" ) )
setMethod(getchain, 'MH_res',
          function(object){
            dim <- base::nrow(object)
            n <- base::nrow(object$chain)

            object$chain %>% data.frame(row.names = NULL) %>%
              mutate(id = rep(1:dim, each = n%/%dim)) %>%
              mutate(iteration = rep(1:(n%/%dim), dim ))
          } )






