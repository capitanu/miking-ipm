
-- This is an example file for generating and visualizing
-- a circuit model

-- You need to source this file in order to visualize models.
include "../src/models/modelVisualizer.mc"


mexpr


-- create your circuit
let circuit = Parallel [
    Component ("r","V1",11.0,false),
    Component ("ground","g",0.0,true)
] in

-- call function 'visualize' to get visualization code for the circuit
visualize [
	Circuit(circuit,[("r","shape=rect","k")])
]