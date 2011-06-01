#!/usr/bin/python
from itertools import product

#input file location
input="input.txt"

types_to_combination = {
	'A': ['A', 'O'], 
	'B': ['B', 'O'], 
	'AB': ['A', 'B'], 
	'O': ['O', 'O']}
	
rhesus_to_combination = {
	'+': ['+', '-'],
	'-': ['-', '-']}
	
combination_to_types = {
	('A', 'A') : 'A', 
	('A', 'B') : 'AB', 
	('B', 'A') : 'AB', 
	('A', 'O') : 'A', 
	('O', 'A') : 'A', 
	('B', 'B') : 'B', 
	('B', 'O') : 'B', 
	('O', 'B') : 'B', 
	('O', 'O') : 'O'}
	
combination_to_rhesus = {
	('+', '+') : '+', 
	('+', '-') : '+', 
	('-', '+') : '+', 
	('-', '-') : '-'}

def get_type_and_rhesus(p):
	return p.strip()[:-1], p.strip()[-1:]

def types_to_string(types):
	if len(types) == 0: return "IMPOSSIBLE"
	return '{' + ", ".join(map(lambda x: x[0]+x[1] ,types)) + '}'

def determine_children(p1, p2):
	p1_type, p1_rhesus = get_type_and_rhesus(p1)
	p2_type, p2_rhesus = get_type_and_rhesus(p2)
	
	possible_types = set([combination_to_types[combination] for combination in product(types_to_combination[p1_type], types_to_combination[p2_type])])
	possible_rhesus = set([combination_to_rhesus[combination] for combination in product(rhesus_to_combination[p1_rhesus], rhesus_to_combination[p2_rhesus])])
	
	return list(product(possible_types, possible_rhesus))
	
def determine_parent(p, c):
	p_type, p_rhesus = get_type_and_rhesus(p)
	c_type, c_rhesus = get_type_and_rhesus(c)
	p_type , p_rhesus = types_to_combination[p_type], rhesus_to_combination[p_rhesus]
	c_type , c_rhesus = types_to_combination[c_type], rhesus_to_combination[c_rhesus]
	
	types = []
	if c_type[0] in p_type: types.append(c_type[1])
	if c_type[1] in p_type: types.append(c_type[0])
	if len(types)== 0: return []
	types = set([combination_to_types[combination] for combination in reduce(lambda x,y: x+y,[filter(lambda keys: type in keys ,combination_to_types.keys()) for type in types])])
	
	rhesus = []
	if c_rhesus[0] in p_rhesus: rhesus.append(c_rhesus[1])
	if c_rhesus[1] in p_rhesus: rhesus.append(c_rhesus[0])
	if len(rhesus)== 0: return []
	rhesus = set([combination_to_rhesus[combination] for combination in reduce(lambda x,y: x+y,[filter(lambda keys: r in keys ,combination_to_rhesus.keys()) for r in rhesus])])
	
	return list(product(types, rhesus))
	

#read the input file
f = open(input, 'r')
i = 1
for line in f:
	line=line.strip()
	if line == "E N D": break
	split = line.split(" ")
	if split[0] == "?" : print "%d: "%(i) + line.replace("?", types_to_string(determine_parent(split[1], split[2])))
	if split[1] == "?" : print "%d: "%(i) + line.replace("?", types_to_string(determine_parent(split[0], split[2])))
	if split[2] == "?" : print "%d: "%(i) + line.replace("?", types_to_string(determine_children(split[0], split[1])))
	i += 1

f.close()
	