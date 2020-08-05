include "string.mc"
include "../todot.mc"

type BTreeModel
    con Node  : (a,BTree,BTree) -> BTree 
    con Leaf  : (a) -> BTree
    con Nil   : () -> BTree
type BTree = {
    tree: BTreeModel,
    eqv: a -> a -> Bool
}

-- constructor for the BTree
let btreeConstr = lam tree. lam eqv.
    {tree = tree, eqv = eqv}

recursive
let treeEdgesModel = lam tree. lam from.
    match tree with Node n then
        join [match from with () then [] else [(from,n.0,"")], 
            treeEdgesModel n.1 n.0,
            treeEdgesModel n.2 n.0]
    else match tree with Leaf v then [(from,v,"")]
    else []
end

-- get all edges of the BTree in correct render order.
let treeEdges = lam tree. treeEdgesModel tree.tree

recursive
let treeVerticesModel = lam tree.
    match tree with Node t then
        join [[t.0], 
        treeVerticesModel t.1,
        treeVerticesModel t.2]
    else match tree with Leaf v then [v]
    else []
end

-- get all vertices of the BTree in correct render order.
let treeVertices = lam tree. treeVerticesModel tree.tree

-- prints a btree in dot.
let btreePrintDot = lam tree. lam node2str. lam direction. lam vSettings.
    let dotEdges = map (lam e. initDotEdge (node2str e.0) (node2str e.1) "" "->" "") (treeEdges tree ()) in
    let dotVertices = map (lam v. 
        let extra = find (lam x. tree.eqv x.0 v) vSettings in
        initDotVertex (node2str v) (match extra with Some e then e.1 else "")
    ) (treeVertices tree) in
    printDot "digraph" direction (getStdNodeSettings ()) dotVertices dotEdges

mexpr
let treeModel = Node(2, Nil (), Leaf 3) in
let tree = btreeConstr treeModel eqi in
utest match treeModel with Node t then t.0 else (negi 100) with 2 in
utest match treeModel with Node t then t.1 else (negi 100) with Nil () in
utest match treeModel with Node t then t.2 else (negi 100) with Leaf 3 in
utest treeEdgesModel treeModel () with [(2,3,"")] in
utest treeEdges tree () with [(2,3,"")] in
utest treeVerticesModel treeModel with [2,3] in
utest treeVertices tree with [2,3] in ()
