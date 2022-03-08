import argparse
import math
from typing import DefaultDict


from two_opt import opt


# Executible for project
# Run:
# python tsp_main.py -alg ***** -inst DATA/* -time *** -seed ***


def distance(source, target):
    d = math.pow((target["x"] - source["x"])**2 + (target["y"] - source["y"])**2, 0.5)
    return int(d) if d % 1 < .5 else math.ceil(d)

#main function
if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    #parser.add_argument('-alg', required=True, type=str, help='algorithm to use (BnB, Approx, LS1, or LS2)')
    parser.add_argument('-inst', required=True, type=str, help='the filepath of the instance to use')
    parser.add_argument('-time', required=True, type=int, help='cutoff time for the algoirthm')
    parser.add_argument('-seed', required=False, type=int, help='random seed to use')
    args = parser.parse_args()

    alg = "LS1"
    inst = args.inst
    time = args.time 

    seed = None
    if args.seed is not None:
        seed = args.seed

    node_count = 0
    # List of all nodes, where node = {"id": _, "x": _, "y": _}
    nodes = []
    with open(inst) as f:
        lines = f.readlines()
        header = lines[:5]
        node_count = int(header[2].split()[1])
        data = lines[5:]

        for i, line in enumerate(data):
            if i+1 > node_count:
                break
            vals = line.split()
            node = {"id": int(vals[0])-1, "x": float(vals[1]), "y": float(vals[2])}
            nodes.append(node)

    distances = [[math.inf for i in range(node_count)] for j in range(node_count)]
    for source in nodes:
        for target in nodes:
            if source["id"] != target["id"]:
                d = distance(source, target)
                distances[source["id"]][target["id"]] = d

    # quality: cost of best solution; Ex: 277952
    # path: list of nodes in the order that achieves the best solution; Ex: 0,2,9,6,5,3,7,8,4,1
    # trace: dict of {time: quality}, where each pair represents a time when a new improved solution is found; Ex: {3.45: 102, 7.94: 95}
	from simann import switch, temperature, probability, comparison, pathDistance, LS1
	quality, path, trace = LS1(time,seed,inst,alg,node_count,distances)


    # Creating output files
    out_file_base = "{}_{}_{}".format(inst.split(".")[0], alg, time)
    if alg not in ["BnB", "Approx"]:
        out_file_base += "_{}".format(seed)

    with open(out_file_base + ".sol", 'w') as sol_file:
        sol_file.write("{}\n".format(quality))
        path_str = ""
        for node_id in path:
            path_str += "{},".format(node_id)
        
        path_str = path_str[:-1] + "\n"
        sol_file.write(path_str)

    with open(out_file_base + ".trace", 'w') as trace_file:
        for t, q in trace.items():
            trace_file.write("{}, {}\n".format(t, q))