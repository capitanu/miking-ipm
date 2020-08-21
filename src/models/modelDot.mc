-- This file provides toDot functions for all models defined in model.mc.
include "model.mc"

-- constructor for dotEdge
let initDotEdge = lam from. lam to. lam label. lam delimiter. lam extra.
    {from = from, to = to, label = label, delimiter = delimiter, extra = extra}

-- constructor for dotVertex
let initDotVertex = lam name. lam settings.
    {name = name,settings=settings}

-- concatenates a list of strings
let concatList = lam list.
    foldl concat [] list

-- gets the quote
let getQuote = lam id.
    match id with () then "\"" else "\\\""

-- formats a dotEdge to dot
let edgeToDot = lam e. lam modelID.
    let quote = getQuote modelID in
    let class = match modelID with () then "" else concatList ["class=",quote,"model",(int2string modelID),"edge",quote," ",
                                                              "id=",quote,e.from,e.label,e.to,quote," "] in
    concatList [e.from," ",e.delimiter," ",e.to," [label=",quote,e.label,quote," ",class,e.extra,"];"]

-- formats a dotVertex to dot
let vertexToDot = lam v. lam modelID.
    let quote = getQuote modelID in
    let class = match modelID with () then "" else concatList ["class=model",(int2string modelID),"node"," "] in
    concatList [v.name,"[","id=",quote,v.name,quote," ",class,v.settings,"];"]

-- let edgeToSubgraph = lam lst. lam modelID.
--     let w = foldl (lam str. lam x. concat str (foldl concat [] [["subgraph {\n",
--     "rank=same;",
--     vertexToDot x.1 modelID,
--     vertexToDot x.0 modelID,
--     "\n};"
--     ]])) "" lst in
--     utest w with [] in w

-- prints a given model in dot syntax
let getDot = lam graphType. lam direction. lam vertices. lam edges. lam id. lam extra.
    let output = foldl concat [] [[graphType," {\n",extra,"\n","rankdir=",direction,";"],
        (map (lam v. vertexToDot v id) vertices),
        (map (lam e. edgeToDot e id) edges),
        ["}"]
    ] in
    foldl concat [] output

-- returns the standard active node setting
let getActiveNodeSetting = lam _.
    " fillcolor=darkgreen fontcolor = white"

-- returns the standard node setting
let getStdNodeSettings = lam _.
    " style=filled fillcolor=white shape=circle"


-- returns a btree in dot.
let btreeGetDot = lam tree. lam node2str. lam direction. lam id. lam vSettings.
    let dotEdges = map (lam e. initDotEdge (node2str e.0) (node2str e.1) "" "->" "") (treeEdges tree ()) in
    let dotVertices = map (lam v. 
        let extra = find (lam x. tree.eqv x.0 v) vSettings in
        let settings = concat (match extra with Some e then e.1 else "") (getStdNodeSettings ()) in
        initDotVertex (node2str v) settings
    ) (treeVertices tree) in
    getDot "digraph" direction dotVertices dotEdges id ""

-- returns a graph in dot.
let graphGetDot = lam graph. lam v2str. lam l2str. lam direction. lam id. lam graphType. lam vSettings.
    let delimiter = if ((setEqual eqchar) graphType "graph") then "--" else "->" in
    let dotVertices = map (lam v. 
        let extra = find (lam x. graph.eqv x.0 v) vSettings in
        let settings = concat (match extra with Some e then e.1 else "") (getStdNodeSettings ()) in
        initDotVertex (v2str v) settings
    ) (graphVertices graph) in
    let dotEdges = map (lam e. initDotEdge (v2str e.0) (v2str e.1) (l2str e.2) delimiter "") (graphEdges graph) in
    getDot graphType direction dotVertices dotEdges id ""

-- Gets a NFA in dot simulated "steps" steps av the "input" input.
let nfaGetDotSimulate = lam nfa. lam v2str. lam l2str. lam direction. lam id. lam vSettings. lam input. lam steps.
    let eqv = nfaGetEqv nfa in
    let path = (if (lti (negi 0) steps) then slice (nfaMakeEdgeInputPath nfa.startState input nfa) 0 steps
        else []) in
    let currentState = if (eqi steps 0) then nfa.startState
        else if (lti steps 0) then None()
        else (last path).1 in 
    let finalEdge = if (lti steps 1) then None() 
        else last path in
    let startNodeSettings = concat "style=invis" getStdNodeSettings () in
    let dotVertices = join [[initDotVertex "start" startNodeSettings],
        map (lam v. 
            let dbl = if (any (lam x. eqv x v) nfa.acceptStates) then "shape=doublecircle" else "" in
            let settings = (if (lti (negi 1) steps) then 
                if (eqv v currentState) then getActiveNodeSetting () else getStdNodeSettings ()
            else "") in
            let extra = find (lam x. eqv x.0 v) vSettings in
            let extraSettings = strJoin " " [dbl,(match extra with Some e then e.1 else "")] in
            initDotVertex (v2str v) (strJoin " " [extraSettings,settings]))
        (nfaStates nfa)] in
    let startEdgeStyle = if (eqi 0 steps) then "color=darkgreen" else "" in
    let eqEdge = (lam a. lam b. and (eqv a.0 b.0) (eqv a.1 b.1)) in
    let dotEdges = join [[initDotEdge "start" nfa.startState "start" "->" startEdgeStyle],
        map (lam e. 
            let extra = if (lti 0 steps) then 
                if (eqEdge (e.0,e.1) finalEdge) then "color=darkgreen"
                else "" 
            else "" in
            initDotEdge (v2str e.0) (v2str e.1) (l2str e.2) "->" extra)
        (nfaTransitions nfa)] in
    getDot "digraph" direction dotVertices dotEdges id ""

-- Gets a NFA in dot.
let nfaGetDot = lam nfa. lam v2str. lam l2str. lam direction. lam id. lam vSettings.
    nfaGetDotSimulate nfa v2str l2str direction id vSettings "" (negi 1)

let makeTDElem = lam color. lam elem_width. lam elem_height. lam quote.
    foldl concat [] ["<td ",
        "bgcolor=",quote,color,quote,
        " width=",quote,(int2string elem_width),quote,
        " height=",quote,(int2string elem_height),quote,
        "></td>\n"]

-- returns the standard node setting
let getBatteryNodeSettings = lam quote.
    let side_width = 1 in
    let center_width = 10 in
    let side_height = 5 in
    let center_height = 10 in
    foldl concat [] ["shape=none, color=none height=0 width=0 margin=0 label=<
    <table BORDER=",quote,"0",quote," CELLBORDER=",quote,"0",quote," CELLSPACING=",quote,"0",quote," CELLPADDING=",quote,"0",quote,"> 
        <tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                [("black",side_width,side_height),("none",center_width,side_height),("none",side_width,side_height)],
        "</tr> 
        <tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                [("black",side_width,side_height),("none",center_width,center_height),("black",side_width,side_height)],
        "</tr>
        <tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                [("black",side_width,side_height), ("none",center_width,side_height),("none",side_width,side_height)],
        "</tr>   
     </table>>"]

-- returns the standard node setting
let getGroundNodeSettings = lam quote.
    let width =5 in
    let height = 1 in
    foldl concat [] ["shape=none, color=none height=0 width=0 margin=0 label=<
    <table CELLBORDER=",quote,"0",quote," CELLSPACING=",quote,"0",quote," CELLPADDING=",quote,"0",quote," >\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["black","black","black","black","black"],
        " </tr>\n<tr>",
           makeTDElem "none" width (muli 2 height) quote,
       "</tr>\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["none","black","black","black","none"],
        "</tr>\n<tr>",
            makeTDElem "none" width (muli 2 height) quote,
        "</tr>\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["none","none","black","none","none"],
        "</tr>\n</table>> "]


let componentToDot = lam comp. lam quote.
    match comp with Component (comp_type,name,maybe_value) then
        -- round to two decimals
        let value = match maybe_value with None () then 0.0 else maybe_value in
        let value_str = int2string (roundfi value) in
        match comp_type with "resistor" then
            concatList [name,"[id=",quote,name,quote," ",
                        "xlabel=",quote,value_str," &Omega;",quote," ",
                        "style=filled color=black fillcolor=none shape=rect height=0.1 width=0.3 ",
                        "label=",quote,quote,"];"]
        else match comp_type with "battery" then
            concatList [name,"[id=",quote,name,quote," ",
                        "xlabel=",quote,value_str," V",quote," ",
                        getBatteryNodeSettings quote,"];"]
        else match comp_type with "ground" then
            let figName = concat name "fig" in
            concatList [figName,"[id=",quote,figName,quote," ",getGroundNodeSettings quote,"];",
                        name,"[id=",quote,name,quote," shape=point style=filled color=black height=0.05 width=0.05];",
                        figName,"--",name,";"]
        else ""
    else []


-- returns a graph in dot.
let circGetDot = lam circ. lam id. lam vSettings.
    let quote = getQuote id in
    let delimiter = "--" in
    let components = circGetAllComponents circ in
    let dotComponents = concatList (map (lam c. componentToDot c quote) components) in
    -- let edges = concat (circGetAllEdges circ) [] in
    -- let dotEdges = concat (map (lam e.
    --         let from = circGetComponentName e.0 in
    --         let to = circGetComponentName e.1 in
    --         initDotEdge from to "" delimiter ""
    --         ) edges) (map (lam e.
    --             initDotEdge (e.0).name (e.1).name "" delimiter ""
    --         ) groundEdges
    --         ) in

    concatList ["graph { concentrate=true; splines=ortho; ranksep=0.6; nodesep=0.6; rankdir=BT;",
                dotComponents,
                "node[ shape = point height = 0 width = 0 margin = 0 padding=0];",
                "}"]

-- converts the given model in dot. vSettings is a seqence of 
-- two element tuples, the first element refers to the name of the vertex, 
-- the second should be a string with custom graphviz settings.
let modelGetDot = lam model. lam id. lam vSettings.
    match model with Graph(graph,v2str,l2str,direction) then
        graphGetDot graph v2str l2str direction id "graph" vSettings
    else match model with Digraph(digraph,v2str,l2str,direction) then
        graphGetDot digraph v2str l2str direction id "digraph" vSettings
    else match model with NFA(nfa,input,state2str,label2str,direction) then
        nfaGetDot nfa state2str label2str direction id vSettings
    else match model with DFA(dfa,input,state2str,label2str,direction) then
        nfaGetDot dfa state2str label2str direction id vSettings
    else match model with BTree(tree, node2str,direction) then
        btreeGetDot tree node2str direction id vSettings
    else match model with Circuit(circuit) then
        circGetDot circuit id vSettings
    else ""

let modelPrintDotSimulateTo = lam model. lam steps. lam vSettings.
    match model with NFA(nfa,input,state2str,label2str,direction) then
        nfaGetDotSimulate nfa state2str label2str direction () vSettings input steps
    else match model with DFA(dfa,input,state2str,label2str,direction) then
        nfaGetDotSimulate dfa state2str label2str direction () vSettings input steps
    else ""

-- converts and prints the given model in dot.
let modelPrintDot = lam model. lam vSettings.
    print (modelGetDot model () vSettings)
