include "../src/models/model.mc"

mexpr 
	let char2string = (lam b. [b]) in

	-- create your directed graph
	let digraph = foldr (lam e. lam g. digraphAddEdge e.0 e.1 e.2 g) 
	(foldr digraphAddVertex (digraphEmpty eqchar eqi) ['A','B','C','D','E']) 
                [('A','B',2),('A','C',5),('B','C',2),('B','D',4),('C','D',5),('C','E',5),('E','D',2)] in
  
	let digraphModel = Digraph(digraph, char2string,int2string) in


-- create your NFA
let nfaAlphabet = ['0','1','2','3'] in
let nfaStates = ["a","b","c","d","e","f"] in
let nfaTransitions = [("a","b",'1'),("b","c",'0'),("c","d",'2'),("c","e",'2'),("d","a",'1'),("e","f",'1')] in
let nfaStartState = "a" in
let nfaAcceptStates = ["a"] in

let nfa = nfaConstr nfaStates nfaTransitions nfaAlphabet nfaStartState nfaAcceptStates (setEqual eqchar) eqchar in


-- create your BTree
let btree = BTree (Node(2, Node(3, Nil (), Leaf 4), Leaf 5)) in

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
