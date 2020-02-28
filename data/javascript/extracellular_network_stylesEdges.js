[ {

    "selector" : "node",
    "css" : {
      "color" : "rgb(0,0,0)",
      "width" : 75.0,
      "background-opacity" : 1.0,
      "shape" : "roundrectangle",
      "border-opacity" : 1.0,
      "border-color" : "rgb(204,204,204)",
      "height" : 35.0,
      "text-opacity" : 1.0,
      "font-family" : "Helvetica",
      "font-weight" : "normal",
      "text-valign" : "center",
      "text-halign" : "center",
      "font-size" : 12,
      "border-width" : 0.0,
      "background-color" : "rgb(137,208,245)",
      "content" : "data(FriendlyName)"
    }
  },{
    "selector" : "node[Type = 'Ligand']",
    "css" : {
      "background-color" : "rgb(255,255,204)"
    }
  }, {
    "selector" : "node[Type = 'Receptor']",
    "css" : {
      "background-color" : "rgb(255,204,204)"
    }
  }, {
    "selector" : "node[Type = 'Cell']",
    "css" : {
      "background-color" : "rgb(204,255,204)"
    }
  }, {
    "selector" : "node:selected",
    "css" : {
      "shape" : "ellipse",
      "background-color" : "rgb(68,153,227)"
    }
  }, {
    "selector" : "edge",
    "css" : {
        "line-color": "black",
        "target-arrow-shape": "triangle",
        "target-arrow-color": "black",
        "width": 1.1,
        "curve-style": "bezier"
    }
  }, {
    "selector" : "edge[interaction = 'C3']",
    "css" : {
      "line-color" : "rgb(38,201,36)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(38,201,36)"
    }
  }, {
    "selector" : "edge[interaction = 'C5']",
    "css" : {
      "line-color" : "rgb(0,66,249)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(0,66,249)"
    }
  }, {
    "selector" : "edge[interaction = 'C6']",
    "css" : {
      "line-color" : "rgb(250,60,251)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(250,60,251)"
    }
  }, {
    "selector" : "edge[interaction = 'C1']",
    "css" : {
      "line-color" : "rgb(251,12,28)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(251,12,28)"
    }
  }, {
    "selector" : "edge[interaction = 'C2']",
    "css" : {
      "line-color" : "rgb(245,239,55)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(255,249,56)"
    }
  }, {
    "selector" : "edge[interaction = 'C4']",
    "css" : {
      "line-color" : "rgb(35,203,202)",
      "target-arrow-color" : "black",
      "source-arrow-color" : "rgb(35,203,202)"
    }
  }, {
    "selector" : "edge:selected",
    "css" : {
      "line-color" : "rgb(255,128,0)"
    
  }
  
} ]