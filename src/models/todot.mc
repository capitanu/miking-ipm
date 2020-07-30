type dotEdge = {
    from: String,
    to: String,
    label: String,
    delimiter: String,
    extra: String
}

type dotVertex = {
    name: String,
    extra: String
}

-- constructor for dotEdge
let initDotEdge = lam from. lam to. lam label. lam delimiter. lam extra.
    {from = from, to = to, label = label, delimiter = delimiter, extra = extra}

-- constructor for dotVertex
let initDotVertex = lam name. lam extra.
    {name = name, extra = extra}

-- concatenates a list of strings
let concatList = lam list.
    foldl concat [] list

-- formats a dotEdge to dot
let edgeToDot = lam e.
    concatList [e.from," ",e.delimiter," ",e.to," [label=\"",e.label,"\" ",e.extra,"];"]

-- formats a dotVertex to dot
let vertexToDot = lam v.
    concatList [v.name,"[",v.extra,"];"]

-- prints a given model in dot syntax
let printDot = lam graphType. lam direction. lam stdVerticesSetting. lam vertices. lam edges.
    let printList = map (lam x. print x) in
    let _ = printList [graphType," {","rankdir=",direction,";"] in
    let _ = printList ["node [",stdVerticesSetting,"];"] in
    let _ = printList (map (lam v. vertexToDot v) vertices) in
    let _ = printList (map (lam e. edgeToDot e) edges) in
    let _ = print "}" in ()

-- converts a given model to dot and returns it as a string.
let model2dot = lam graphType. lam direction. lam stdVerticesSetting. lam vertices. lam edges.
    concatList [
        graphType," {","rankdir=",direction,";",
        "node [",stdVerticesSetting,"];",
        foldl (lam output. lam v. concat output (vertexToDot v)) vertices,
        foldl (lam output. lam e. concat output (edgeToDot e)) edges,"}"
    ]

-- returns the standard active node setting
let getActiveNodeSetting = lam _.
    " fillcolor=darkgreen fontcolor = white"

-- returns the standard node setting
let getStdNodeSettings = lam _.
    "style=filled fillcolor=white shape=circle"