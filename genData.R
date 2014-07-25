genNodes=function(n) {
  nodes=data.frame()
  for(i in 1:n)
    nodes=rbind(nodes,data.frame(paste("U",toString(i),sep=""),Sys.time(),
                                 0,0,sample.int(100,1)[1],
                                 stringsAsFactors = FALSE))
  colnames(nodes)=c("names","time","followers","followings","posts")
  return(nodes)
}


genLinks=function(nodes) {
  links=data.frame()
  n=nrow(nodes)
  for(i in 1:n)
    for(j in 1:n)
      if( i!=j && sample(1:3,1)==1 )
        {
          newLink=data.frame(nodes$name[i],nodes$name[j],
                             Sys.time(),
                             sample.int(100,1)[1],sample.int(100,1)[1],
                             stringsAsFactors = FALSE)
          links=rbind(links,newLink)

#         print(c(id,i,j,n))
#         print(nodes)

          nodes$followers[j]=nodes$followers[j]+1
          nodes$followings[i]=nodes$followings[i]+1
          nodes$posts[[i]]=nodes$posts[[i]]+newLink[[4]]+newLink[[5]]
          

      }
  colnames(links)=c("n1","n2","time","retweets","mentions")
  return(list(nodes,links)) 
} 

if (length(commandArgs(T))>0) {
  n=commandArgs(T)[1]
  nodes=genNodes(n)
  grafo=genLinks(nodes)
  
  cat("nodedef>")
  write.table(grafo[[1]],sep=",",row.names=F)
  cat("edgedef>")
  write.table(grafo[[2]],sep=",",row.names=F)
  } else 
      print("genData <number of vertex>")


# gdf output
#print("nodedef> \"name\",\"time\",\"followers\",\"followings\",\"posts\" ")


