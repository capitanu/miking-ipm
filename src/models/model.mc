include "types/btree.mc"
include "types/dfa.mc"
include "types/nfa.mc"
include "todot.mc"

-- Represents models that can be visualized and its associated data.
-- Also provides toDot functions for all types defined below
type Model
    con Digraph : (Digraph,    vertex2str, edge2str ) -> Model
    con DFA     : (DFA, input, state2str,  label2str) -> Model
    con Graph   : (Graph,      vertex2str, edge2str ) -> Model
    con NFA     : (NFA, input, state2str,  label2str) -> Model
    con BTree   : (BTree, node2str) -> Model

-- prints a graph in dot.
let graphPrintDot = lam graph. lam v2str. lam l2str. lam direction. lam graphType. lam vSettings.
    let delimiter = if ((setEqual eqchar) graphType "graph") then "--" else "->" in
    let dotVertices = map (lam v. 
        let extra = find (lam x. graph.eqv x.0 v) vSettings in
        initDotVertex (v2str v) (match extra with Some e then e.1 else "")
    ) (graphVertices graph) in
    let dotEdges = map (lam e. initDotEdge (v2str e.0) (v2str e.1) (l2str e.2) delimiter "") (graphEdges graph) in
    printDot graphType direction (getStdNodeSettings ()) dotVertices dotEdges

-- converts and prints the given model in dot. vSettings is a seqence of 
-- two element tuples, the first element refers to the name of the vertex, 
-- the second should be a string with custom graphviz settings.
let modelPrintDotWithOptions = lam model. lam direction. lam vSettings.
    match model with Graph(graph,v2str,l2str) then
        graphPrintDot graph v2str l2str direction "graph" vSettings
    else match model with Digraph(digraph,v2str,l2str) then
        graphPrintDot digraph v2str l2str direction "digraph" vSettings
    else match model with NFA(nfa,input,state2str,label2str) then
        nfaPrintDot nfa state2str label2str direction vSettings
    else match model with DFA(dfa,input,state2str,label2str) then
        nfaPrintDot dfa state2str label2str direction vSettings
    else match model with BTree(tree, node2str) then
        btreePrintDot tree node2str direction vSettings
    else ""

-- converts and prints the given model in dot.
let modelPrintDot = lam model. lam direction.
    modelPrintDotWithOptions model direction []

mexpr
let alfabeth = ['0','1'] in
let states = ["a","b","c"] in
let transitions = [("a","b",'1'),("b","c",'0'),("c","a",'1')] in
let startState = "a" in
let acceptStates = ["a", "c"] in
let dfa = dfaConstr states transitions alfabeth startState acceptStates (setEqual eqchar) eqchar in
let model = DFA(dfa, "1011", lam b. b, lam b. [b]) in 
utest match model with DFA(d,i,s2s,t2s) then i else "" with "1011" in
utest match model with DFA(d,i,s2s,t2s) then d.acceptStates else "" with ([(['a']),(['c'])]) in 
()
