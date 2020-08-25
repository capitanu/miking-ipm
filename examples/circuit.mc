
-- This is an example file for generating and visualizing
-- a circuit model

-- You need to source this file in order to visualize models.
include "../src/models/modelVisualizer.mc"


mexpr


-- create your circuit
let circuit = Parallel [
    Series[
    Component ("ammeter","Afff",1.0,true),
    Component ("ground","g",0.0,false)
    ],
    Series [
        Component ("battery","V",11.0,true),
        Component ("resistor","R",4.0,true) 
    ]
] in

-- call function 'visualize' to get visualization code for the circuit
visualize [
	Circuit(circuit,[("ammeter","shape=circle style=filled fillcolor=lightgreen label=\\\"A\\\"","&Omega; ")])
]