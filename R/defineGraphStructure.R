# Internal: functions for defining paths
###################################################################
###################################################################
merge_alt_nodes <- function(pths){
  if(length(pths)<=1)
    return(pths)

  neigh <- get_neighbors(pths)

  mat <- apply(neigh,2,function(x){
      apply(neigh,2,function(y){
          all(x==y)
      })
  })

  words <- apply(mat,1,function(x){
      if(sum(x)<=1)
        return(NA)

      paste(rownames(mat)[x], collapse = ",")
  })

  words   <- unique(words[!is.na(words)])

  if(length(words)>0){
      words   <- strsplit(words, ",")
      replace <- sapply(words, function(w){paste0("(",paste(w,collapse = "_"),")")})

      vecs <- strsplit(pths,">")

      pths <- sapply(vecs, function(vec){
              for(i in 1:length(words)){
                  vec[vec %in% words[[i]]] <- replace[i]
              }

              paste(unique(vec), collapse = ">")
          })
  }

  return(unique(pths))
}


###################################################################
###################################################################
merge_sequence_nodes <- function(pths){

  neigh <- get_neighbors(pths)

  degrees <- apply(neigh,c(1,2), function(x){length(strsplit(x,",")[[1]])})

  keep <- apply(degrees,2, function(x){any(x==1)})

  if(sum(keep)<2)
    return(pths)

  startPoints <- names(which(degrees["in",]>=1 & degrees["out",]==1))

  keep <- sapply(startPoints, function(x){
      if(degrees["in",x]>1)
        return(T)

      y <- neigh["in",x]

      return(!any(startPoints == y))
  })

  startPoints <- startPoints[keep]


  if(length(startPoints)==0)
    return(pths)

  words <- sapply(startPoints, function(x){
     res <- c(x)
     x <- neigh["out",x]
     if(x != "Inf")
     {
         while(degrees["in",x] <= 1)
         {
            res <- c(res,x)

            if(neigh["out",x]=="Inf" | degrees["out",x]>1)
                break

            x <- neigh["out",x]
         }
     }
     if(length(res)==1)
        return(NA)

     return(paste(res, collapse = ","))
  })

  words   <- unique(words[!is.na(words)])

  if(length(words)>0){
     words   <- strsplit(words, ",")
     replace <- sapply(words, function(w){paste0("(",paste(w,collapse = "+"),")")})

     vecs <- strsplit(pths,">")

     pths <- sapply(vecs, function(vec){
             for(i in 1:length(words))
             {
                 vec[vec %in% words[[i]]] <- replace[i]
             }

             paste(unique(vec), collapse = ">")
      })
 }

 return(unique(pths))
}


########################################
########################################
get_paths <- function(subc){
    x <- unlist(lapply(subc, rownames))
    names(x) <- NULL
    return(x)
}


###################################################################
###################################################################
paths_to_vectors <- function(paths){
    strsplit(as.character(paths), ">")
}


###################################################################
###################################################################
pths_to_edges <- function(pths){

  pathCount <- 0
  edgs <- matrix(data=NA, nrow = 0, ncol = 3)

  nodesPos <- nodes_from_paths(pths)

  for (p in pths){

    pathCount <- pathCount + 1;

    nds <- paths_to_vectors(p)[[1]]
    nds <- unlist(sapply(nds,function(n){
      simplify(n)
    }))

    if(length(nds)>1){
      for(i in 1:(length(nds)-1)){

        if(i==length(nds)-2){
          endingE <- "tee"
        }else{
          endingE <- "vee"
        }
        if(!(nds[i] == 'START') && !(nds[i+1] == 'END')){
          str <- paste('n_',nodesPos[nodesPos[,1]==nds[i],2],'',' -> ',
                       'n_',nodesPos[nodesPos[,1]==nds[i+1],2],' [arrowhead = ',endingE,']', sep = '')
        }else{
          str <- paste('n_',nodesPos[nodesPos[,1]==nds[i],2],'',' -> ',
                       'n_',nodesPos[nodesPos[,1]==nds[i+1],2],' [style=invisible arrowhead =none]', sep = '')
        }
        cId = paste0(nodesPos[nodesPos[,1]==nds[i],2],'>',nodesPos[nodesPos[,1]==nds[i+1],2])

        if(cId%in%edgs[,1]){
          edgs[edgs[,1]==cId,3] = paste(edgs[edgs[,1]==cId,3],pathCount,sep=',')
        }else{
          edgs <- rbind(edgs,c(cId,str,pathCount))
        }
      }
    }else{
      edgs <- cbind(nds[1],"","")
    }
  }
  colnames(edgs) <- c("id","edge","label")
  return(edgs)
}


###################################################################
###################################################################
pths_to_nodes <- function(pths){

  nodesPos <- nodes_from_paths(pths)
  colorsN <-rainbow(length(nodesPos[,1]))

  coloredNodes <- paste0(unlist(apply(nodesPos,1,function(n){

    if(!(n[1] %in% c('START','END'))){
      paste0('n_',n[2], ' [color=',color.id(colorsN[as.numeric(n[2])])[1], ' label="',n[1], '"] ', sep='')
    }else
      paste0('n_',n[2], ' [color=white label=""] ', sep='')
  })),collapse='\n')

  return(coloredNodes)
}


###################################################################
###################################################################
nodes_from_paths <- function(pths){

  nodes <- unique(str_sort(unlist(sapply(pths, function(p){
    strsplit(p,'>')[[1]]
  })), numeric = T))

  nodes <- str_sort(unlist(sapply(nodes,function(n){
    simplify(n)
  })))

  nodesPos <- cbind(nodes,c(1:length(nodes)))
  return(nodesPos)
}


###################################################################
###################################################################
get_classes <- function(paths){
    sort(unique(unlist(paths_to_vectors(paths))))
}


###################################################################
###################################################################
get_neighbors <- function(paths, classes = NULL){
    if(is.null(classes))
        classes <- get_classes(paths)

    vecs <- paths_to_vectors(paths)

    result <- sapply(classes, function(cl){
        tmp <- sapply(vecs, function(vec){
            index <- which(vec == cl)

            if(length(index)==0 || length(vec) <= 1)
                return(rep(NA,2))

            a <- vec[index-1]
            b <- vec[index+1]
            if (index == 1)
                a <- -Inf

            if(index == length(vec))
                b <- Inf

            return(c(a,b))
        })

        in.list  <- paste(sort(unique(tmp[1,!is.na(tmp[1,])])), collapse = ",")
        out.list <- paste(sort(unique(tmp[2,!is.na(tmp[2,])])), collapse = ",")

        return(c(in.list, out.list))
    })

    if(!is.matrix(result))
        result <- matrix(result, nrow = 2)

    colnames(result) <- classes
    rownames(result) <- c("in","out")

    return(result)
}


###################################################################
###################################################################
simplify_paths <- function(pths){

  pths <- unlist(sapply(pths, function(p){
    paste0(unlist(sapply(strsplit(p,'>')[[1]],function(n){
      n <- simplify(n)
    })),collapse = ">")
  }))
  return(matrix(sort(pths)))
}


###################################################################
###################################################################
gen_dot_string_pths <- function(pths,edges,nodes, name){
  pthsId <- unique(unlist(sapply(edges[,3],function(l){strsplit(l,',')})))

  colorsPth <- cbind(pthsId,rainbow(length(pthsId)))

  res <- paste0('digraph "',name,'" {rankdir=LR label="',name,'" node [shape=Mrecord]\n', collapse = '')
  #res <- 'digraph {rankdir=LR label= node [shape=Mrecord]\n'

  if(edges[2]!=""){
    str <- paste0(unlist(apply(edges,1,function(e){
      firstPath = strsplit(e[3],',')[[1]][1]
      colorP <- color.id(colorsPth[colorsPth[,1]==firstPath,2])[1]

      paste0(e[2],' [color="',colorP,'"]',collapse='')
    })),collapse='\n')
  }else{
    str <- ""}

  extr.nds <- first_last_nodes(pths)
  res <- paste0(res,nodes,str,extr.nds,'}', collapse = '\n')

  res <- gsub("\\{START\\|", "\\{", res)
  res <- gsub("\\|END\\}", "\\}", res)
  return(res)

}


###################################################################
###################################################################
first_last_nodes <- function(pths){
  nodesPos <- nodes_from_paths(pths)
  first <- unique(unlist(sapply(pths,function(p){strsplit(as.character(p),'>')[[1]][1]})))

  last <- unique(unlist(sapply(pths,function(p){tail(strsplit(as.character(p),'>')[[1]],1)})))

  mid <- unique(unlist(sapply(pths,function(p){
    if(length(strsplit(as.character(p),'>')[[1]])>2){
      strsplit(as.character(p),'>')[[1]][2:(length(strsplit(as.character(p),'>')[[1]])-1)]
    }})))

  last <- setdiff(setdiff(last,first),mid)

  first <- paste0("n_",nodesPos[nodesPos[,1] %in% first,2])
  if(length(last>0)){
    last <- paste0("n_",nodesPos[nodesPos[,1] %in% last,2])
  }
  rank = '{rank = same; '

  first = paste0(rank,paste('"',first,'"',sep = '', collapse = ' '),'}')
  if(length(last)>0){
    last = paste0(rank,paste('"',last,'"',sep = '', collapse = ' '),'}')
  }

  fl <- paste0(first,last,collapse = '\n')

  return(fl)
}


###################################################################
###################################################################
map_old_labs <- function(pths, numLabs, catLabs){
  pths <- unlist(sapply(pths, function(p){
    paste0(unlist(sapply(strsplit(p,'>')[[1]],function(n){
      n <- catLabs[which(n == numLabs)[[1]]]
    })),collapse = ">")
  }))
  return(matrix(sort(pths)))
}


###################################################################
###################################################################
remove_brackets <- function(str){
  x <- strsplit(str, "")[[1]]
  if(x[1] == "("){
    x <- x[-c(1, length(x))]
  }
  
  paste(x, collapse = "")
}


###################################################################
###################################################################
simplify_internal <- function(str, sep = c("_","+")){
  x <- strsplit(str, "")[[1]]
  if(x[1] != "(")
    return(list(sep = "", str = str))
  
  x <- x[-c(1, length(x))]            # extract content between external brackets
  
  comp <- c()
  tmp <- c()
  
  bracket <- 0
  my.sep = ""
  
  for(i in 1:length(x)){
    if(x[i] %in% sep & bracket == 0){           # check if _ or +
      if(length(tmp)>0){                      #2: tmp = c("4")    10: tmp = c()
        my.str <- paste(tmp, collapse = "") #2: my.str = "4"
        comp <- c(comp,my.str)              #2: comp = "4"
      }
      tmp <- c()                              #2: tmp =c()        10: tmp = c()
      my.sep = x[i]                           #2: my_sep="+"      10: my_sep="+"
    }else{
      if(x[i] == "("){                        #3: x[i] = "("
        tmp <- c(tmp, x[i])                 #3: tmp = c("(")
        bracket = bracket+1                 #3: bracket = 1
      }
      
      if(x[i] == ")"){
        tmp <- c(tmp, x[i])                 #9: tmp = c("(","0","_","1","_","5")
        bracket = bracket-1                 #9: bracket = 0
        if(bracket == 0){
          if(length(tmp)>0){
            my.str <- paste(tmp, collapse = "")     #9: my.str = "(0_1_5)"
            comp <- c(comp,my.str)                  #9: comp = c("4","(0_1_5)"))
          }
          tmp <- c()                                  #9: tmp = c()
        }#else{
        #   print("TODO: if the internal sign is + -> eliminate the parenthesis")
        #}
      }
      
      if(!(x[i] %in% c("(",")"))){
        tmp <- c(tmp, x[i])                 #1: tmp = c("4")
        #4: tmp = c("(","0")
        #5-8: tmp = c("(","0","_","1","_","5")
        #11: tmp = c("6")
      }
    }
  }
  
  
  if(length(tmp) > 0){                             #length(tmp) == 1
    comp <- c(comp,paste(tmp, collapse = ""))   # comp = c("4","(0_1_5)","6")
  }
  
  
  for(i in 1:length(comp)){
    tmp <- simplify_internal(comp[i], sep=sep)  #1: tmp = list(sep="", str="")
    
    if(tmp$sep == my.sep){
      #if(tmp$sep == my.sep[i])
      comp[i] = remove_brackets(tmp$str)
    }else{
      comp[i] = tmp$str                       #1: comp[1]=""
    }
  }
  
  result <- list(sep = my.sep,
                 str= paste("(",paste(comp, collapse = my.sep), ")", sep = ""))
  
  return(result)
}


###################################################################
###################################################################
simplify <- function(str, sep = c("_","+")){
  tmp <- simplify_internal(str, sep = c("_","+"))$str
  leftP  <- strsplit(tmp,"\\(")[[1]]
  rightP <- strsplit(tmp,"\\)")[[1]]
  
  if(!grepl("\\+",leftP[2])  && !grepl("\\+",rightP[length(rightP)])){
    #print("ok")
    tmp <- if(substr(tmp,1,1)=="(") substr(tmp, 2, nchar(tmp)-1) else tmp
  }
  tmp <- str_replace_all(str_replace_all(tmp,"\\+", "|"),"_", "|")
  tmp <- str_replace_all(str_replace_all(tmp,"\\)", "\\}"),"\\(", "\\{")
  return(tmp)
}
