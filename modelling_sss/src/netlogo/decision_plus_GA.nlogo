extensions [matrix] ;; load matrix extension to use for agent memtal maps

;; define variables
globals [outFile worldMap] ;; name and path of output csv file, generic mental map used by all agents
breed [agents agent] ;; plural and singular form of agent referent
agents-own [energy myMap] ;; current energy level of an agent, mental map matrix of agent
patches-own [resource maxResource] ;; current resources of a patch, maximum resources that can exist on a patch


to setup
  ;; set up global values, agents, patches, and world map matrices
  clear-all
  set outFile "" ;; variable holding name of file to save csv of output
  if timeSteps = "" [set timeSteps 0] ; change empty max tick limit entry into an integer
  setup-patches ;; call procedure to instantiate patches and patch variables of gridded landscape
  create_agents ;; cal procedure to instantiate forager agents and agent variables
  set worldMap matrix:make-constant world-height world-width -1 ;; default world map to be used in updating agent world maps
  if saveOutput [ 
    ;; routine to allow user to select file for csv output
    set outFile user-new-file ;; call user dialog to select output file and output file directory
    if outFile != "" [
      if file-exists? outFile [ ;; overwrite file with existing name
        file-close-all
        file-delete outFile]
      file-open outFile ;; open output file so that data can be added periodally
      file-type "agent" file-type "," file-type "timestep" file-type "," file-print "energy" ;; headers for csv file
      ]
    ]
  if randomAgents [set nAgents random 199 + 1]
  reset-ticks ;; start time steps ("ticks") at 0
end

to setup-patches
  ;; procedure to instantiate patches and patch variables of gridded landscape
  ask patches [
    set maxResource random 100  ;; maximum potential resource on each patch
    set resource maxResource    ;; initialize each patch with maximum potential resource
    set pcolor scale-color green resource 100 0 ;; set patch color to a shade of green to indicate resource amount (dark is more)
    ]
end

to create_agents
  ;; procedure to instantiate forager agents and agent variables
  create-agents nAgents [ ;; create a number of agents determined by user-selectable variable nAgents (slider in GUI)
    set energy maxEnergy / 2 ;; start each agent with 1/2 of the maximum possible energy that agents can possess
    setxy random-xcor random-ycor ;; place agents randomly in gridded landscape (on patches)
    set color scale-color red energy 0 maxEnergy ;; set agent color to a shade of red to indicate amount of energy (dark is more)
    set shape "circle" ;; set default agent shape
    set size 1 ;; set default agent size
    set myMap matrix:make-constant world-height world-width -1 ;; give agent a mental map of the world without any information (-1 is no information) 
    ]
end

to go
  ;; procedure that cycles simulation through time steps. Loops endlessly unless stopped by user or if ticks surpass value of timeSteps variable 
  ask agents [
    ;; behavior of each agent during each time step
    if decisionType = "greedy" [greedy] ;; call "greedy" resource search behavior
    if decisionType = "probabalistic" [probabalistic] ;; call"probablistic: research search behavior
    if energy >= maxEnergy [reproduce] ;; if energy level of agent reaches maxEnergy value (set by user), call "reproduce" procedure
    set energy energy - energyCost ;; energy cost for living and moving; decrease agent stored energy by energyCost value (set by user) each time step
    if energy <= 0 [die] ;; if energy level of agent falls to 0, agent dies
    set color scale-color red energy 0 maxEnergy ;; reset agent color to indicate new energy level after foraging and living/moving
    ]
  ask patches [regrow] ;; call "regrow" procedure to increment resources
  tick
  if saveOutput and ticks mod outputInterval = 0 [output] ;; add agent information to output csv file
  if (timeSteps > 0 and ticks = timeSteps) or count agents = 0 [stop] ;; stop simulation if time steps exceed timeSteps value
end

to greedy 
  ;; greedy resource search algorithm. Find neighborhood patch with most resources, move there, and consume resources
  let allNeighbors (patch-set neighbors patch-here) ;; define neighborhood to include current patch and 8 surrounding patches
  if memory [set allNeighbors update-world allNeighbors] ;; if "memory" variable is set, use mental map to perceive resources on neighborhood patches
  let best max [resource] of allNeighbors ;; identify neighborhood patch with most resources
  move-to one-of allNeighbors with [resource = best] ;; move to patch with most resources
  consume ;; call "consume" procedure
end

to probabalistic
  ;; probabalistic resource search algorithm. Use relative amounts of resources on neghborhood patches 
  ;;  to determine probability of moving to a patch to consume resources
  let allNeighbors (patch-set neighbors patch-here) ;; define neighborhood to include current patch and 8 surrounding patches 
  if memory [set allNeighbors update-world allNeighbors] ;; if "memory" variable is set, use mental map to perceive resources on neighborhood patches  
  let resourceList [] ;; initiate temporary list to hold resource values from all neghborhood patches
  let index 0 ;; temporary variable that holds reference to each neighborhood patch
  ask allNeighbors [
    ;; repeat index of patch a number of times equal to the value of resources on that patch and add all these values to resourceList
    set resourceList sentence (n-values resource [index]) resourceList
    set index index + 1
    ]
  if length resourceList > 0 [ ;; error trap to catch bad lists
    ;; pick an entry from resourceList randomly. The chance of picking a patch index value is proportional to the number of times
    ;;   it is repeated, which is determined by the resource value of the patch
    let choice one-of resourceList
    let neighborlist []
    ask allNeighbors [set neighborList lput self neighborList] ;; list of neighborhood patches 
    move-to item choice neighborList ;; move to patch with probability determined by resource abundance
  ]
  consume ;; call "consume" procedure
end

to consume
  ;; agent consumes resource and resource value added to stored energy of agent. Agent can only consume up to maxEnergy level
  let need maxEnergy - energy ;; determine how much energy can be consumed by agent
  ifelse need < [resource] of patch-here
  [set energy energy + need ;; if energy needs of agent are less than available resouce, consume up to maxEnergy level
    ask patch-here [set resource resource - need]] ;; reduce patch resource level by amount consumed
  [set energy energy + [resource] of patch-here ;; if energy needs are greater than or equal to amount of resources on patch, consume all resouce on patch and increase agent energy by that amount
    ask patch-here [set resource 0]]
end

to regrow
  ;; regrow patch resource if not being consumed by agent
  if resource < maxResource [
    set resource resource + resourceGrowthRate ;; increment patch resource by "resourceGrowthRate" (set by User) up to maxResource level for that patch
    set pcolor scale-color green resource 0 maxResource ;; reset color to indicate resource amount on patch
    ]
end

to-report update-world [current]
  ;; Algorithm to update values in agent mental map and return mental map resource values for neighborhood
  ;; Input ("current") is neighborhood of calling agent (8 surrounding patches plus patch agent is on)
  ;; Mental map stored as matrix
  ;; Start with no values in cells of mental map (-1 in each matrix cell)
  ;; Update mental map neighborhood values as average of observed values and any previous values (memory) 
  set worldMap myMap ;; store mental map temporarily into global "worldMap" matrix
  ask current [
    ;; update mental map and neighborhood resource values
    ifelse matrix:get worldMap pycor pxcor = -1 
      [matrix:set worldMap pycor pxcor resource] ;; if no prior information about resources in cell (value = -1), set it to currently observed value
      [matrix:set worldMap pycor pxcor (resource + matrix:get worldMap pycor pxcor) / 2 ;; average value from mental map (prior experience) with currently obeserved value.
       set resource matrix:get worldMap pycor pxcor]
    ]
  set myMap worldMap ;; copy updated values back from temporary mental map to agent mental map
  report current ;; return updated neighborhood resource values to caller
end

to reproduce
  ;; agent reproduces
  let originalEnergy energy
    hatch 1 [ ;; agent clones self 
      set energy originalEnergy / 2 ;; agent offsprint/clone gets half of original agent energy
    ]
    set energy originalEnergy / 2 ;; reduce agent energy to half original amount (because other half went to offspring)
end

to output
  ;; output information about each agent to csv file for each time step or time step interval
  ask agents [
    file-type who file-type "," file-type ticks file-type "," file-print energy ;; information about each agent to output (agent ID, time step, energy)
  ]
  if timeSteps > 0 and ticks = timeSteps [
    file-close ;; close output file if simulation stops
    stop ;; stop simulation if time steps (ticks) reaches value of timeSteps
  ]

end
