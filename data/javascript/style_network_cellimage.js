[ {
  "format_version" : "1.0",
  "generated_by" : "cytoscape-3.7.2",
  "target_cytoscapejs_version" : "~2.1",
  "title" : "default",
  "style" : [ {
    "selector" : "node",
    "css" : {
      "width" : 75.0,
      "text-opacity" : 1.0,
      "text-valign" : "center",
      "text-halign" : "center",
      "color" : "rgb(0,0,0)",
      "font-family" : "Helvetica",
      "font-weight" : "normal",
      "border-opacity" : 1.0,
      "border-color" : "rgb(204,204,204)",
      "background-color" : "rgb(255,5,5)",
      "border-width" : 0.0,
      "font-size" : 14,
      "background-opacity" : 1.0,
      "shape" : "roundrectangle",
      "height" : 35.0,
      "content" : "data(FriendlyName)"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.875][UpBinRatio <= 1]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.875,1,rgb(0,109,44),rgb(0,68,27))",
      "color" : "rgb(217,217,217)"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.75][UpBinRatio < 0.875]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.75,0.875,rgb(35,139,69),rgb(0,109,44))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.625][UpBinRatio < 0.75]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.625,0.75,rgb(65,171,93),rgb(35,139,69))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.5][UpBinRatio < 0.625]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.5,0.625,rgb(116,196,118),rgb(65,171,93))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.375][UpBinRatio < 0.5]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.375,0.5,rgb(161,217,155),rgb(116,196,118))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.25][UpBinRatio < 0.375]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.25,0.375,rgb(199,233,192),rgb(161,217,155))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0.125][UpBinRatio < 0.25]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.125,0.25,rgb(229,245,224),rgb(199,233,192))"
    }
  }, {
    "selector" : "node[UpBinRatio >= 0][UpBinRatio < 0.125]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0,0.125,rgb(247,252,245),rgb(229,245,224))"
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.875][UpBinRatio <= 1]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.875,1,rgb(153,52,4),rgb(102,37,6))",
      "color" : "rgb(217,217,217)",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.75][UpBinRatio < 0.875]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.75,0.875,rgb(204,76,2),rgb(153,52,4))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.625][UpBinRatio < 0.75]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.625,0.75,rgb(236,112,20),rgb(204,76,2))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.5][UpBinRatio < 0.625]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.5,0.625,rgb(254,153,41),rgb(236,112,20))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.375][UpBinRatio < 0.5]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.375,0.5,rgb(254,196,79),rgb(254,153,41))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.25][UpBinRatio < 0.375]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.25,0.375,rgb(254,227,145),rgb(254,196,79))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0.125][UpBinRatio < 0.25]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.125,0.25,rgb(255,247,188),rgb(254,227,145))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'T_cells_CD8'][UpBinRatio >= 0][UpBinRatio < 0.125]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0,0.125,rgb(255,255,229),rgb(255,247,188))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.875][UpBinRatio <= 1]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.875,1,rgb(8,81,156),rgb(8,48,107))",
      "color" : "rgb(217,217,217)",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.75][UpBinRatio < 0.875]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.75,0.875,rgb(33,113,181),rgb(8,81,156))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.625][UpBinRatio < 0.75]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.625,0.75,rgb(66,146,198),rgb(33,113,181))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.5][UpBinRatio < 0.625]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.5,0.625,rgb(107,174,214),rgb(66,146,198))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.375][UpBinRatio < 0.5]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.375,0.5,rgb(158,202,225),rgb(107,174,214))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.25][UpBinRatio < 0.375]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.25,0.375,rgb(198,219,239),rgb(158,202,225))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0.125][UpBinRatio < 0.25]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.125,0.25,rgb(222,235,247),rgb(198,219,239))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Macrophage'][UpBinRatio >= 0][UpBinRatio < 0.125]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0,0.125,rgb(247,251,255),rgb(222,235,247))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.875][UpBinRatio <= 1]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.875,1,rgb(84,39,143),rgb(63,0,125))",
      "color" : "rgb(217,217,217)",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.75][UpBinRatio < 0.875]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.75,0.875,rgb(106,81,163),rgb(84,39,143))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.625][UpBinRatio < 0.75]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.625,0.75,rgb(128,125,186),rgb(106,81,163))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.5][UpBinRatio < 0.625]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.5,0.625,rgb(158,154,200),rgb(128,125,186))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.375][UpBinRatio < 0.5]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.375,0.5,rgb(188,189,220),rgb(158,154,200))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.25][UpBinRatio < 0.375]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.25,0.375,rgb(218,218,235),rgb(188,189,220))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0.125][UpBinRatio < 0.25]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.125,0.25,rgb(239,237,245),rgb(218,218,235))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Dendritic_cells'][UpBinRatio >= 0][UpBinRatio < 0.125]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0,0.125,rgb(252,251,253),rgb(239,237,245))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.875][UpBinRatio <= 1]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.875,1,rgb(189,0,38),rgb(128,0,38))",
      "color" : "rgb(217,217,217)",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.75][UpBinRatio < 0.875]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.75,0.875,rgb(227,26,28),rgb(189,0,38))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.625][UpBinRatio < 0.75]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.625,0.75,rgb(252,78,42),rgb(227,26,28))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.5][UpBinRatio < 0.625]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.5,0.625,rgb(253,141,60),rgb(252,78,42))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.375][UpBinRatio < 0.5]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.375,0.5,rgb(254,178,76),rgb(253,141,60))",
      "shape" : "ellipse",
     "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.25][UpBinRatio < 0.375]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.25,0.375,rgb(254,217,118),rgb(254,178,76))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0.125][UpBinRatio < 0.25]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0.125,0.25,rgb(255,237,160),rgb(254,217,118))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node[id = 'Tumor_cell'][UpBinRatio >= 0][UpBinRatio < 0.125]",
    "css" : {
      "background-color" : "mapData(UpBinRatio,0,0.125,rgb(255,255,204),rgb(255,237,160))",
      "shape" : "ellipse",
      "text-max-width" : 90.0,
      "text-wrap" : "wrap",
      "width" : 100.0,
      "height" : 60.0
    }
  }, {
    "selector" : "node:selected",
    "css" : {
      "background-color" : "rgb(255,255,0)"
    }
  }, {
    "selector" : "edge",
    "css" : {
      "line-color": "black",
        "target-arrow-shape": "triangle",
        "target-arrow-color": "black",
        "width": 0.5,
        "curve-style": "bezier"
    }
  }, {
    "selector" : "edge[interaction = 'peptide']",
    "css" : {
      "line-color" : "rgb(0,0,0)",
      "line-style" : "dashed",
      "target-arrow-shape": "none"
    }
  }, {
    "selector" : "edge[interaction = 'interacts']",
    "css" : {
      "line-color" : "rgb(0,0,0)",
      "target-arrow-color" : "rgb(0,0,0)",
      "source-arrow-color" : "rgb(0,0,0)"
    }
  }, {
    "selector" : "edge:selected",
    "css" : {
      "line-color" : "rgb(255,0,0)"
    }
  } ]
} ]