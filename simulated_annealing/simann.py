import random
import math
import time as TIME

# =============================================================================
# Simulated Annealing with Monotonic Log Cooling
# Author: Daniel Marchetto
# Time: 12/3/2021
# =============================================================================

#Candidate creator
def switch(path,node_count):
    index = random.sample(range(0, node_count), 2) #Generates index of elements to swap
    path[index[0]] = path[index[0]] + path[index[1]] #These following rows swap elements without extra space
    path[index[1]] = path[index[0]] - path[index[1]]
    path[index[0]] = path[index[0]] - path[index[1]]
    return path[:]

#Cooling schedule
def temperature(initialT,k):
    return max(initialT/math.log(k+1), 0.01) #Log monotonic cooling scheduling

#Probability measure
def probability(delta,initialT,k):
    prob = math.exp(-(delta/temperature(initialT,k))) #Probability to replace current with candidate
    return prob 

#Distance comparison measure
def comparison(currentPathDistance,candidatePathDistance):
    return candidatePathDistance - currentPathDistance #Measures if candidate path is better than current path distance

#Hamiltonian path distance
def pathDistance(path,distances,node_count):
    pathD = distances[path[0]][path[(node_count-1)]] #Distance from last element to first
    for i in range(1,node_count): #Adds distances from first to second, second to third, etc.
        pathD += distances[path[i-1]][path[i]]
    return pathD

#Simulated Annealing 
def LS1(time,seed,inst,alg,node_count,distances):
    
    #Initialization Steps
    trace = {} #Trace dictionary initializtion
    startTime = TIME.time() #Start of run time
    timeRemaining = time - round(TIME.time() - startTime, 2) #Time left on execution

    random.seed(seed) #Random seed initialization
    initialPath = list(range(0,node_count)) #Hamiltonian path initialization
    random.shuffle(initialPath) #Randomly orders initial path

    initialT = 10000 #Temperature initialization
    k = 1 #Tracks number of iterations
    currentPath = initialPath[:]
    currentPathDistance = pathDistance(currentPath,distances,node_count)
    bestPath = currentPath[:]
    bestDistance = currentPathDistance


    #Annealing Steps
    while timeRemaining > 0.01: #Runs annealing until time runs out (with a buffer)
        candidatePath = switch(currentPath,node_count)  #Creates candidate path
        candidatePathDistance = pathDistance(candidatePath,distances,node_count) #Measures candidate path length
        delta = comparison(pathDistance(currentPath,distances,node_count),pathDistance(candidatePath,distances,node_count)) #Difference between current and candidate paths
        #Replaces current path if candidate is better than current
        if delta < 0: 
            currentPath = candidatePath[:]
            currentPathDistance = candidatePathDistance
        #Replaces current path with certain probability if candidate path is worse than current
        elif probability(delta,initialT,k) >= random.uniform(0,1):
            currentPath = candidatePath[:]
            currentPathDistance = candidatePathDistance
        #Updates best path if it is better
        if currentPathDistance < bestDistance:
            bestDistance = currentPathDistance
            bestPath = currentPath[:]
			trace.update({round(TIME.time() - startTime, 2): bestDistance})#Updates trace
        timeRemaining = time - round(TIME.time() - startTime, 2) #Updates time remaining
        k += 1 #Updates iteration number

    quality = bestDistance
    path = bestPath
    return quality, path, trace