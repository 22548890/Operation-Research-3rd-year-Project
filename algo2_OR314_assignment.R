mat=mat2
no_of_edges_per_color=rowSums(mat)
no_of_colors_per_node=colSums(mat)

#split nodes equally into g1 and g2
group1<-NULL
group2<-NULL
no_of_nodes=length(no_of_colors_per_node)
no_of_colours=length(no_of_edges_per_color)


for (i in 1:no_of_nodes){
  if (i<=no_of_nodes/2){
    group1[length(group1)+1]<-i
  } else {
    group2[length(group2)+1]<-i
  }
}

max=no_of_nodes
b=1
bb=0
change=1
count=0
cnt=0
old_group1=group1
while ((b==1)&&(count<=max)) {
  count=count+1
  #g1
  
  if (cnt>2){
    no=0
    for (i in 1:length(group1)){
      if (old_group1[i]==group1[i]){
        no=no+1
      }
    }
    if (no==length(group1)){
      b=0
      break
    }
    old_group1=group1
    cnt=0
    
  }
  cnt=cnt+1
  group1_scores<-NULL
  group2_scores<-NULL
  for (y in group1){
    score=0
    for (x in 1:no_of_colours){
      if (mat[x,y]==1){
        for (yy in group1){
          if(yy!=y){#skips first since know =1
            if(mat[x,yy]==1){
              score=score-1
              break #in color to its group
            }
          }
          
        }
      }
    } 
    #check score of other group
    for (v in group2){
      score2=0
      for (w in 1:no_of_colours){
        if (mat[w,v]==1){
          for (vv in group2){
            if (vv!=v){
              if(mat[w,vv]==1){#found another color to g2
                score2=score2+1
                break #out color to other group = more reson to swap
              }
            }
          }
        }
      }
    }
    score=score+score2
    #add score of each col for group 1
    group1_scores[length(group1_scores)+1]<-score #score of in/out degrees
  }
  #max value of score must exit and be added to g2
  index_max_score_group1=-1
  max_score_group1=-100
  for (i in 1:length(group1_scores)){#finds max score and index
    if (group1_scores[i]>max_score_group1){
      max_score_group1=group1_scores[i]
      index_max_score_group1=i
    }
  }
  if (change==1){#just recalculating not changing
    group2[length(group2)+1]<-group1[index_max_score_group1] #add max score of g1 to group2
    group1=group1[-index_max_score_group1] #remove that col that was added to g2
  }
  
  ###Group 2 has extra item, now find largest to equal again
  #same as first just inverse groups and scoring
  
  for (y in group2){
    score=0
    for (x in 1:no_of_colours){
      if (mat[x,y]==1){
        for (yy in group2){
          if(yy!=y){#skips first since know =1
            if(mat[x,yy]==1){
              score=score-1
              break #in color to its group
            }
          }
          
        }
      }
    } 
    #check score of other group
    for (v in group1){
      score2=0
      for (w in 1:no_of_colours){
        if (mat[w,v]==1){
          for (vv in group1){
            if (vv!=v){
              if(mat[w,vv]==1){#found another color to g2
                score2=score2+1
                break #out color to other group = more reson to swap
              }
            }
          }
        }
      }
    }
    score=score+score2
    #add score of each col for group 1
    group2_scores[length(group2_scores)+1]<-score #score of in/out degrees
  }
  #max value of score must exit and be added to g2
  index_max_score_group2=-1
  max_score_group2=-100
  for (i in 1:length(group2_scores)){#finds max score and index
    if (group2_scores[i]>max_score_group2){
      max_score_group2=group2_scores[i]
      index_max_score_group2=i
    }
  }
  if(change==1){
    group1[length(group1)+1]<-group2[index_max_score_group2] #add max score of g2 to group1
    group2=group2[-index_max_score_group2] #remove that col that was added to g1
  }
  
  ##Iteration complete groups are in equality
  #now recalculate score to see if termination is fit
  if(bb==1){
    b=0
  }
  
  if (change==1){
    change = 0
  } else {
    #check termination if not then change=1
    sum_groupScores=1
    for (i in 1:length(group1)){
      if(group1_scores[i]>0){
        sum_groupScores=1
        break
      }
      if (group2_scores[i]>0){
        sum_groupScores=1
        break
      }
      sum_groupScores=-1
    }
    if (sum_groupScores<=0) {
      #termination satisfies
      bb=1
      change=1
    } else {
      #not optimal
      change=1
    }
  }
  
}
#group1 contains nodes
print(group1)
#group2 contains nodes
print(group2)

