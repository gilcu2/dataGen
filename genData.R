genNodes=function(n) {
  nodes=NULL
  for(i in 1:n)
    nodes=rbind(nodes,data.frame(paste("U",toString(i),sep=""),Sys.time(),
                                 0,0,0,
                                 stringsAsFactors = FALSE))
  colnames(nodes)=c("name","time","followers","followings","posts")
  return(nodes)
}


genLinks=function(nodes) {
  links=NULL
  n=nrow(nodes)
  for(i in 1:n)
    for(j in 1:n)
      if( i!=j && sample(1:3,1)==1 )
        {
          newLink=data.frame(nodes$name[i],nodes$name[j],
                             Sys.time(),
                             sample(1:100,1),sample(10:100,1),
                             stringsAsFactors = FALSE)
          links=rbind(links,newLink)

#         print(c(id,i,j,n))
#         print(nodes)

          nodes$followers[j]=nodes$followers[j]+1
          nodes$followings[i]=nodes$followings[i]+1
          nodes$posts[i]=nodes$posts[i]+newLink[4]+newLink[5]

      }
  colnames(links)=c("n1","n2","time","retweets","mentions")
  return(list(nodes,links)) 
} 

n=commandArgs(T)[1]
nodes=genNodes(n)
genLinks(nodes)

# gdf output
#print("nodedef> \"name\",\"time\",\"followers\",\"followings\",\"posts\" ")


