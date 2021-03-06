###############################################################
# ~Decision Making Model~
# by enrico.crema@gmail.com                                      
#                                        
# Parameters:                                       
# decisionType ::: Type of decision, either "greedy" or "probabilistic"
# memory ::: Boolean, whether the agents move based on current state of the system, or their cognitive maps
# xDim ::: x limit of the world
# yDim ::: y limit of the world
# nAgents ::: Number of agents at initialisation                                        
# timeSteps ::: Number of timesteps in the model
# resourceGrowthRate ::: Amount increase of resources per timeStep
# maxEnergy ::: maximum possible storable energy. Also maximum possible cell values in the resource scape. 
# spendEnergy ::: ammount of enegry spent by each agent each timestep.


nAgents=100;spendEnergy=9;maxEnergy=100;resourceGrowthRate=2;
               timeSteps=1000;xDim=30;yDim=30;memory=FALSE;
               decisionType=c("greedy","probabilistic");
plot=TRUE;verbose=TRUE



main<-function(nAgents=100,spendEnergy=9,maxEnergy=100,resourceGrowthRate=2,
               timeSteps=1000,xDim=30,yDim=30,memory=FALSE,
               decisionType=c("greedy","probabilistic"),
               plot=TRUE,verbose=TRUE)
    {
        population=numeric() #placeholder for recording population size 
        resource=matrix(round(runif(xDim*yDim,0,maxEnergy)),nrow=xDim,ncol=yDim) #initialise resource scape
        maxResource=resource #maximum possible resource value per cell
        #initialise agents as a data.frame:
        agents=data.frame(energy=rep(maxEnergy/2,nAgents),x=ceiling(runif(nAgents,0,xDim)),y=ceiling(runif(nAgents,0,xDim)))

        #Create cognitive map list in case of memory
           if(memory==TRUE)
               {
                cognitiveMaps=vector("list",length=nAgents)
                for (x in 1:nAgents){cognitiveMaps[[x]]=matrix(NA,nrow=xDim,ncol=yDim)}
               }

        #Start of the actual simulation#
        if(verbose==TRUE){pb <- txtProgressBar(min = 1, max = timeSteps, style = 3)}

        for (t in 1:timeSteps)
            {

                #STEP 1: Observe, Move, and Collect Energy#
                newOrder=sample(1:nrow(agents)) 
                agents=agents[newOrder,]  #shuffle agent order
                if(memory==TRUE){cognitiveMaps=cognitiveMaps[newOrder]}  #shuffle also cognitive map list

                
                for (a in 1:nrow(agents))
                            {
                                #agents move
                                if (memory==FALSE)
                                    {    
                                        agents[a,2:3]=neighbourhood(xcor=agents[a,2],ycor=agents[a,3],
                                                  xLimit=c(1,xDim),yLimit=c(1,yDim),
                                                  resourceMatrix=resource,type=decisionType)
                                    }

                                if (memory==TRUE)
                                    {
                                        #agents learn and move

                                        tmp=getEnvironmentAndMove(xcor=agents[a,2],ycor=agents[a,3],
                                            xLimit=c(1,xDim),yLimit=c(1,yDim),
                                            resourceMatrix=resource,type=decisionType,myMap=cognitiveMaps[[a]])
                                        
                                        cognitiveMaps[[a]]=tmp[[1]]
                                        agents[a,2:3]=tmp[[2]]
                                    }
                                
                                #agents consume
                                collection=maxEnergy-agents[a,1]
                                if(collection<=resource[agents[a,2],agents[a,3]])
                                    {resource[agents[a,2],agents[a,3]]= resource[agents[a,2],agents[a,3]]-collection}
                                if(collection>resource[agents[a,2],agents[a,3]])
                                    {collection=resource[agents[a,2],agents[a,3]]
                                     resource[agents[a,2],agents[a,3]]=0}
                                agents[a,1]=collection+agents[a,1]
                            }
                
              
                #STEP 2: Reproduce#
                if(any(agents$energy==maxEnergy))
                    {
                        mothers=which(agents[,1]==maxEnergy)
                        agents[mothers,1]=agents[mothers,1]/2
                        agents<-rbind(agents,agents[mothers,])
                        if(memory==TRUE){cognitiveMaps<-c(cognitiveMaps,cognitiveMaps[mothers])}
                    }

                #STEP 3: Spend Energy#
                agents$energy=agents$energy-spendEnergy


                #STEP 4: Death#
                if(any(agents$energy<=0))
                    {
                      death=which(agents[,1]<=0)
                      if(length(death)==nrow(agents))
                            {
                                agents=agents[-death,]
                                print("extinction!")
                                return(population)
                            }
                       agents=agents[-death,]
                       if(memory==TRUE){cognitiveMaps=cognitiveMaps[-death]} #remove relevant cognitive maps
                     
                    }

                #STEP 5: Resource Growth#
                                       
                resource=resource+resourceGrowthRate
                index=which((resource-maxResource)>0,arr.ind=TRUE)
                resource[index]=maxResource[index]


                #Record Population Size:
                population[t]=nrow(agents)
                
                #Optional Plot Function#
                if(plot==TRUE)
                    {
                        par(mfrow=c(1,2))
                        plot(1:t,population[1:t],type="l",xlab="time",ylab="population",
                             main=paste("Avg.Energy=",round(mean(agents$energy),2)))
                        image(x=1:xDim,y=1:yDim,z=resource,main=paste("Avg.Resource=",round(mean(resource),2)),zlim=c(0,maxEnergy))
                        points(agents$x,agents$y,pch=20,cex=2)
                    }

                if(verbose==TRUE){setTxtProgressBar(pb, t)}
            }
        if(verbose==TRUE){close(pb)}
        return(population)
    }






#utility functions #
neighbourhood<-function(xcor,ycor,xLimit,yLimit,resourceMatrix,type=c("greedy","probabilistic"))
{
step=c(-1,0,1)
xcor1=xcor+step
ycor1=ycor+step
address=expand.grid(x=xcor1,y=ycor1)
if(sum((address$x<xLimit[1]|address$x>xLimit[2]),na.rm=TRUE)>0){address[which(address$x<xLimit[1]|address$x>xLimit[2]),]=NA}
if(sum((address$y<yLimit[1]|address$y>yLimit[2]),na.rm=TRUE)>0){address[which(address$y<yLimit[1]|address$y>yLimit[2]),]=NA}
destinationResource=apply(address,1,function(x,y){
    if(!is.na(x[1]))
        {return(y[x[1],x[2]])}
    if(is.na(x[1]))
        return(NA)},y=resourceMatrix)
if(type=="greedy")
    {
        goto=which(destinationResource==max(destinationResource,na.rm=TRUE))
        if(length(goto)>1){goto=sample(goto,size=1)}
        finaladdress=address[goto,]
    }
if(type=="probabilistic")
    {
        destinationResource[which(is.na(destinationResource))]=0
        goto=sample(1:9,size=1,prob=destinationResource)
        if(length(goto)>1){goto=sample(goto,size=1)}
        finaladdress=address[goto,]
    }
return(finaladdress)
}



getEnvironmentAndMove<-function(xcor,ycor,xLimit,yLimit,resourceMatrix,myMap,type=c("greedy","probabilistic"))
    {
        step=c(-1,0,1)
        xcor1=xcor+step
        ycor1=ycor+step
        address=expand.grid(x=xcor1,y=ycor1)
        
        if(sum((address$x<xLimit[1]|address$x>xLimit[2]),na.rm=TRUE)>0)
            {address[which(address$x<xLimit[1]|address$x>xLimit[2]),]=NA}
        if(sum((address$y<yLimit[1]|address$y>yLimit[2]),na.rm=TRUE)>0)
            {address[which(address$y<yLimit[1]|address$y>yLimit[2]),]=NA}
        if(any(is.na(address$x))){address=address[-which(is.na(address$x)),]}

        
        for (x in 1:9)
            {
                past=myMap[address[x,1],address[x,2]]
                current=resourceMatrix[address[x,1],address[x,2]]
                if(is.na(past)){myMap[address[x,1],address[x,2]]=current}
                else if (!is.na(past)){myMap[address[x,1],address[x,2]]=c(past+current)/2}
            }

        destinationResource=apply(address,1,function(x,y){return(y[x[1],x[2]])}
           ,y=myMap)

        if(type=="greedy")
            {
                goto=which(destinationResource==max(destinationResource,na.rm=TRUE))
                if(length(goto)>1){goto=sample(goto,size=1)}
                finaladdress=address[goto,]
            }
        if(type=="probabilistic")
            {
                destinationResource[which(is.na(destinationResource))]=0
                goto=sample(1:nrow(address),size=1,prob=destinationResource)
                if(length(goto)>1){goto=sample(goto,size=1)}
                finaladdress=address[goto,]
            }
        
        return(list(myMap,finaladdress))
    }
