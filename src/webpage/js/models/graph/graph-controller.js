class GraphController {
    /**
     * Controller class for the Graph view.
     * 
     * @param {object} model An object representing a model.
     * @param {div} modelRoot The root element of the view.
     * @param {int} index A unique model number.
     */
    constructor(model, modelRoot, index){
        let graphModel = new GraphModel(new Graph(model.model, model.type, index))
        // Defining the callback function, which is called when the graph is rendered.
        const callbackFunction = nodes =>
            /*      TEMPORARY -->   */
            // Adds on click event listeners to each of the nodes.
            nodes.on("click", function () {
                let name = d3.select(this).attr("id")
                console.log(graphModel.visualizationModel.getNodeByName(name))
            })
            /* < -- TEMPORARY       */

        this.graphView = new GraphView(graphModel, modelRoot, callbackFunction)
    }
}