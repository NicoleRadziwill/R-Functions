# These functions provide a FLEXIBLE FLATTENER for JSON files. They were written by https://stackoverflow.com/users/4272464/bgoldst
# and are sourced from https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe

tl <- function(e) { if (is.null(e)) return(NULL); ret <- typeof(e); if (ret == 'list' && !is.null(names(e))) ret <- list(type='namedlist') else ret <- list(type=ret,len=length(e)); ret; };
mkcsv <- function(v) paste0(collapse=',',v);
keyListToStr <- function(keyList) paste0(collapse='','/',sapply(keyList,function(key) if (is.null(key)) '*' else paste0(collapse=',',key)));

extractLevelColumns <- function(
    nodes, ## current level node selection
    ..., ## additional arguments to data.frame()
    keyList=list(), ## current key path under main list
    sep=NULL, ## optional string separator on which to join multi-element vectors; if NULL, will leave as separate columns
    mkname=function(keyList,maxLen) paste0(collapse='.',if (is.null(sep) && maxLen == 1L) keyList[-length(keyList)] else keyList) ## name builder from current keyList and character vector max length across node level; default to dot-separated keys, and remove last index component for scalars
) {
    cat(sprintf('extractLevelColumns(): %s\n',keyListToStr(keyList)));
    if (length(nodes) == 0L) return(list()); ## handle corner case of empty main list
    tlList <- lapply(nodes,tl);
    typeList <- do.call(c,lapply(tlList,`[[`,'type'));
    if (length(unique(typeList)) != 1L) stop(sprintf('error: inconsistent types (%s) at %s.',mkcsv(typeList),keyListToStr(keyList)));
    type <- typeList[1L];
    if (type == 'namedlist') { ## hash; recurse
        allKeys <- unique(do.call(c,lapply(nodes,names)));
        ret <- do.call(c,lapply(allKeys,function(key) extractLevelColumns(lapply(nodes,`[[`,key),...,keyList=c(keyList,key),sep=sep,mkname=mkname)));
    } else if (type == 'list') { ## array; recurse
        lenList <- do.call(c,lapply(tlList,`[[`,'len'));
        maxLen <- max(lenList,na.rm=T);
        allIndexes <- seq_len(maxLen);
        ret <- do.call(c,lapply(allIndexes,function(index) extractLevelColumns(lapply(nodes,function(node) if (length(node) < index) NULL else node[[index]]),...,keyList=c(keyList,index),sep=sep,mkname=mkname))); ## must be careful to translate out-of-bounds to NULL; happens automatically with string keys, but not with integer indexes
    } else if (type%in%c('raw','logical','integer','double','complex','character')) { ## atomic leaf node; build column
        lenList <- do.call(c,lapply(tlList,`[[`,'len'));
        maxLen <- max(lenList,na.rm=T);
        if (is.null(sep)) {
            ret <- lapply(seq_len(maxLen),function(i) setNames(data.frame(sapply(nodes,function(node) if (length(node) < i) NA else node[[i]]),...),mkname(c(keyList,i),maxLen)));
        } else {
            ## keep original type if maxLen is 1, IOW don't stringify
            ret <- list(setNames(data.frame(sapply(nodes,function(node) if (length(node) == 0L) NA else if (maxLen == 1L) node else paste(collapse=sep,node)),...),mkname(keyList,maxLen)));
        }; ## end if
    } else stop(sprintf('error: unsupported type %s at %s.',type,keyListToStr(keyList)));
    if (is.null(ret)) ret <- list(); ## handle corner case of exclusively empty sublists
    ret;
}; ## end extractLevelColumns()
## simple interface function
flattenList <- function(mainList,...) do.call(cbind,extractLevelColumns(mainList,...));
