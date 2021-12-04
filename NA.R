library(stats)
library(igraph)


#network characteristics

netsize=10 #network size
iin=1 #infected casews at the start
tsteps=730 #2 years approximately
runs=50 # number of simulations

#Create a random network, using the Watts-Strogatz model

#We can use barabasi.game or erdos.renyi.game for different network, but for 
#this instance we consider that the small world approach works better for a real
#society. We will not take into consideration the dynamics of social interaction
# where new people meet or people stop interacting


#We set new attributes that beter describe the Infected of the people and later,
#their isolation

#dead V(netg) will be removed, based on the attribute "alive"

#1. Create the network based on the chosen specs.

netg <- barabasi.game(netsize, m = 5, directed=FALSE)

#2.Create the attributes required, Infected, Alive and Isolation

set_vertex_attr(netg, "Infected", index = V(netg), value = TRUE)
set_vertex_attr(netg, "Carrier", index = V(netg), value = TRUE)
set_vertex_attr(netg, "Alive", index = V(netg), value = TRUE)
set_vertex_attr(netg, "Isolation", index = V(netg), value = TRUE)

#3. Initialise the attributes accordingly

V(netg)$Infected=FALSE
V(netg)$Carrier=FALSE
V(netg)$Alive=TRUE
V(netg)$Isolation=FALSE



#The next step is to create functions that will make the the dynamic process
#of simulating the spread of the virus

netmulation<-function(netg,netsize,iin,runs){
  
  linf<- c()
  lcaf<- c()
  lisf<- c()
  
  for (runt in 1:runs) {
    
    
    
    V(netg)$Infected=FALSE
    V(netg)$Carrier=FALSE
    V(netg)$Alive=TRUE
    V(netg)$Isolation=FALSE
  
  #Initialization
  
    n1<-sample(1:netsize,iin)
    V(netg)$Infected[n1]<-TRUE
    V(netg)$Carrier[n1]<-TRUE
  
    lin<-c()
    lca<-c()
    lis<-c()
  
    for (t in 1:tsteps) {
    
    
    #Probabilities
    
      p1<-(1 + (2/5)*sinpi(((t - 14)/134)))*(1 + cospi((t + 14)/168 ))/20 #Infection rate
      p11<-(1 + (2/5)*sinpi(((t - 14)/134)))*(1 + cospi((t + 14)/168 ))/4 #Infection rate for carriers
    
      p2<-1/14 #Healing factor
      p22<-1/14 #Healing factor for carriers
    
      p3<-0.0013*((1 + (t/1460))/100) #Mortality
    
      p4<-1/3 #Isolation
      p41<-1/4#Getting out of the isolation
    
      p5<-1/3 #Carrier falling sick
    
      lister<-c()
      lister2<-c()
      lister3<-c()
    
      for (i in 1:netsize) {
        if(V(netg)$Alive[i]==TRUE){
        #List of infected people
          if(V(netg)$Carrier[i]==TRUE){
            lister2<-append(lister2,i)
            if(V(netg)$Infected[i]==TRUE)
              lister<-append(lister,i)
        }
      }
    }
    
    #Isolated People
    
      for (iii in 1:netsize) {
        if(V(netg)$Isolation[iii]==TRUE)
          lister3<-append(lister3,iii)
      
    }
    
      eg1<-ego(netg,order=1,lister) #First neigbhours for each infected person
      eg2<-ego(netg,order=1,lister2) #First neigbhours for each carrier person
    
      l1<- length(eg1) #Number of Infected per step
      l11<- length(eg2) #Number of Carriers per step
    
      l2<- length(lister)
      l22<- length(lister2)
    
      l3<-length(lister3) #Number of Isolated people
    
    
    
      if(l22<1){
        break
    }
    
    #Death Process
      for (n in 1:l1){
        if(runif(1,0,1)<p3)
          V(netg)$Alive[i]==FALSE
      
    }
    
    #Healing Process
    
      for (k in 1:l22) {
        if(V(netg)$Infected[lister2[k]]==TRUE){
          if(runif(1,0,1)<p2){
            V(netg)$Carrier[lister2[k]]=FALSE
            V(netg)$Infected[lister2[k]]=FALSE
        }}
        else
          if(runif(1,0,1)<p22){
            V(netg)$Carrier[lister2[k]]=FALSE
        }
    }
    
    #Carrier fallin sick
    
      for (jj in 1:l22) {#Check if person is sick or not
        counter2<-lister2[jj]
        if(V(netg)$Infected[counter2]==FALSE){# If not isolated the spread of the disease goes on as normal
          if(V(netg)$Carrier[counter2]==TRUE){
            if(runif(1,0,1)<p5){
              V(netg)$Infected[counter2]=TRUE
          }}}}
    
    
    #Infection Spreading
    
    
      for (j in 1:l22) {#Check if person is isolated or not
        counter1<-lister2[j]
        if(V(netg)$Isolation[counter1]==FALSE){# If not isolated the spread of the disease goes on as normal
          if(V(netg)$Carrier[counter2]==TRUE){
            if(V(netg)$Infected[counter1]==TRUE){#If a person is infected
              egon_net<-eg2[[j]]
              le<-length(egon_net)
              for (j1 in 1:le) {
                if(runif(1,0,1)<p1){
                  V(netg)$Carrier[egon_net[j1]]=TRUE
              }}}
            else { #From Carriers
              egon_net<-eg2[[j]]
              le<-length(egon_net)
              for (j1 in 1:le) {
                if(runif(1,0,1)<p11){
                  V(netg)$Carrier[egon_net[j1]]=TRUE
              }}}
        }}}
    
    
    #Isolation Process
    
      for (ii in 1:netsize) {
        if(runif(1,0,1)<p4)
          V(netg)$Isolation[ii]=TRUE
    }
    
      for (jk in 1:l3) {
        if(runif(1,0,1)<p41)
          V(netg)$Isolation[lister3[jk]]=FALSE
    }
    
      lin<-append(lin,l1)
      lin2<-list(lin)
    
      lca<-append(lca,l11)
      lca2<-list(lca)
    
      lis<-append(lis,l3)
      lis2<-list(lis)
    
    }
    
    linf<- append(linf,lin2)
    lcaf<- append(lcaf,lca2)
    lisf<- append(lisf,lis2)
    
    
  }
  
  Results <- list("Infected" = linf, "Carriers" = lcaf,"Isolated"=lisf)
  
  return(Results)
  
}

#Execution

tt1<-netmulation(netg,netsize,iin,runs)

sink("Results_Infected0.txt",append = T)
print(tt1$Infected)
sink()

sink("Results_Carriers0.txt",append = T)
print(tt1$Carriers)
sink()

sink("Results_Isolated0.txt",append = T)
print(tt1$Isolated)
sink()