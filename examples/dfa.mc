-- This is an example file for generating and visualizing
-- a DFA model

-- You need to source this file in order to visualize models.
include "../src/models/modelVisualizer.mc"


mexpr
-- Define functions to display your States and Labels and equality checks for States and Labels
let string2string = (lam b. b) in
let eqString = setEqual eqchar in
let char2string = (lam b. [b]) in

-- Defining the components of a DFA
<<<<<<< HEAD
=======
let alfabeth = ['0','1'] in
>>>>>>> OCaml server (#11)
let states = ["s0","s1","s2","s3"] in
let transitions = [
    ("s0","s1",'1'),
    ("s1","s1",'1'),
    ("s1","s2",'0'),
    ("s2","s1",'1'),
    ("s2","s3",'0'),
    ("s3","s1",'1')
    ] in
let startState = "s0" in
let acceptStates = ["s3"] in

-- constructing the DFA
<<<<<<< HEAD
let dfa = dfaConstr states transitions startState acceptStates eqString eqchar in
=======
let dfa = dfaConstr states transitions alfabeth startState acceptStates eqString eqchar in
>>>>>>> OCaml server (#11)


-- The input for simulation is given here as the second argument
-- in the constructor.
-- Last argument is a list that maps states to displayNames.
-- The purpose is to visualize a state's name differently if
-- needed. This examples maps the state "s0" to the output name
-- of "start state".
visualize [
	-- accepted by the DFA
	DFA(dfa,"10010100",string2string, char2string,[("s0","start state"),("s3","accept state")]),
	-- not accepted by the DFA
	DFA(dfa,"101110",string2string, char2string, []),
	-- not accepted by the DFA
	DFA(dfa,"1010001",string2string, char2string, [])
]
