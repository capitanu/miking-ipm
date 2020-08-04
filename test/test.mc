include "../src/models/modelVisualizer.mc"

mexpr
let string2string = (lam b. b) in
let eqString = setEqual eqchar in
let char2string = (lam b. [b]) in

-- create your DFA
let alfabeth = ['0','1'] in
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

-- create your BTree
let btree = btreeConstr (Node(2, Node(3, Nil (), Leaf 4), Leaf 5)) eqi in

visualize [
    -- accepted by the DFA
    DFA(dfa,"1",string2string, char2string),
    -- accepted by the DFA
    DFA(dfa,"101110",string2string, char2string),
    -- not accepted by the DFA 
    DFA(dfa,"1010001",string2string, char2string),
    Digraph(digraph, char2string,int2string),
    Graph(graph,int2string,string2string),
    BTree(btree, int2string),
    NFA(nfa, "1021", string2string, char2string),
    NFA(nfa, "102", string2string, char2string)
]