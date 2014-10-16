haskell-nvd3
=============

This package provides a basic Haskell interface to [NVD3](http://nvd3.org/index.html), which is a framework built over [d3.js](http://d3js.org/). The aim is to be able to generate the Javascript code that plots the data structures that you already have in your Haskell program.

You can see the types of charts that NVD3 supports [here](http://nvd3.org/examples/index.html). A separate plotting function is provided for each chart type that NVD3 supports. These plotting functions share the same type signature; for example, here is the type signature of the simple line chart function:

```haskell
line :: [Series] -> ChartOptions -> Text
```

The `Series` object contains information about a single series (in the line chart, that would be an individual line) and the `ChartOptions` type contains the settings for the chart. The documentation for both of these types, along with the names of other plotting functions, is on Hackage. Note that since it is not possible to put all the possible d3.js options in the `ChartOptions` object, a field is provided to inject custom d3 code if you so desire. An important point here is that this package aims to generate the JavaScript code exactly as the user wants without checking if the generated code is actually valid d3.js as such validation would require a substantial body of code to emulate both JavaScript and d3.js/NVD3, the latter of which is especially subject to change with time. It is, therefore, recommended that you try out a chart configuration with dummy data first before swapping in real data during actual use.

Here is an example of how this package can be used in ghci.

```haskell
ghci> import Graphics.NVD3
ghci> :set -XOverloadedStrings
ghci> import qualified Data.Text.Lazy as T
ghci> import qualified Data.Vector as V
ghci> let s1 = defSeries -- defSeries is the default Series object with some dummy data
ghci> let v1 = V.enumFromN 1 20 -- x coordinate for the second series
ghci> let v2 = V.map (*3) v1 -- y coordinate for the second series
ghci> let s2 = defSeries {values = mkNumVals v1 v2, key = "Second Series"}
ghci> let testLineFocus = lineFocus [s1,s2] defChartOptions {cssSelector = "#lineFocusChart svg", xAxis = Just defAxis {axisLabel = Nothing}, colorCategory = Just Category10}
ghci> putStr $ T.unpack testLineFocus
function graphFunction () {
var chart = nv.models.lineWithFocusChart();
chart.transitionDuration(350);
chart.color(d3.scale.category10().range());
chart.yAxis.axisLabel("Default Axis");
nv.utils.windowResize(chart.update);
var myData = [{"values":[{"x":1,"y":2},{"x":2,"y":4},{"x":3,"y":6},{"x":4,"y":8},{"x":5,"y":10},{"x":6,"y":12},{"x":7,"y":14},{"x":8,"y":16},{"x":9,"y":18},{"x":10,"y":20},{"x":11,"y":22},{"x":12,"y":24},{"x":13,"y":26},{"x":14,"y":28},{"x":15,"y":30},{"x":16,"y":32},{"x":17,"y":34},{"x":18,"y":36},{"x":19,"y":38},{"x":20,"y":40}],"key":"Default Series"},{"values":[{"x":1,"y":3},{"x":2,"y":6},{"x":3,"y":9},{"x":4,"y":12},{"x":5,"y":15},{"x":6,"y":18},{"x":7,"y":21},{"x":8,"y":24},{"x":9,"y":27},{"x":10,"y":30},{"x":11,"y":33},{"x":12,"y":36},{"x":13,"y":39},{"x":14,"y":42},{"x":15,"y":45},{"x":16,"y":48},{"x":17,"y":51},{"x":18,"y":54},{"x":19,"y":57},{"x":20,"y":60}],"key":"Second Series"}];

d3.select('#lineFocusChart svg').datum(myData).call(chart);
return chart;
}
nv.addGraph(graphFunction);
ghci> let d1 = defSeries {values = mkDiscVals (V.fromList ["Group 1", "Group 2", "Group 3", "Group 4", "Group 5"]) (V.fromList [3,7,2,4,8])}
ghci> let d2 = defSeries {values = mkDiscVals (V.fromList ["Group 1", "Group 2", "Group 3", "Group 4", "Group 5"]) (V.fromList [8,3,6,3,4]), key = "Second Series"}
ghci> let testMultiBar = multiBar [d1, d2] defChartOptions {reduceXTicks = Just True, cssSelector = "#multiBarChart svg", groupSpacing = Just 0.1, colorCategory = Just Category10}
ghci> putStr $ T.unpack testMultiBar
function graphFunction () {
var chart = nv.models.multiBarChart();
chart.transitionDuration(350);
chart.groupSpacing(0.1);
chart.reduceXTicks(true);
chart.color(d3.scale.category10().range());
chart.xAxis.axisLabel("Default Axis");
chart.yAxis.axisLabel("Default Axis");
nv.utils.windowResize(chart.update);
var myData = [{"values":[{"x":"Group 1","y":3},{"x":"Group 2","y":7},{"x":"Group 3","y":2},{"x":"Group 4","y":4},{"x":"Group 5","y":8}],"key":"Default Series"},{"values":[{"x":"Group 1","y":8},{"x":"Group 2","y":3},{"x":"Group 3","y":6},{"x":"Group 4","y":3},{"x":"Group 5","y":4}],"key":"Second Series"}];

d3.select('#multiBarChart svg').datum(myData).call(chart);
return chart;
}
nv.addGraph(graphFunction);
```
As in the above code, an advisable way to build `Series` and `ChartOptions` objects is to modify their default values, `defSeries` and `defChartOptions`. The above code also shows the use of two convenience functions, `mkNumVals` and `mkDiscVals`, that take the x and y vectors for a `Series` and construct the `values` field. 

The generated JavaScript code is then embedded in HTML. Don't forget to match the `cssSelector` inside the HTML

```html
<div id="multiBarChart"><svg style="width:800px;height:450px;"></svg></div>
```

and the necessary links in the head section

```html
<link href="https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.15-beta/nv.d3.css" rel="stylesheet" />
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.4.12/d3.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.15-beta/nv.d3.min.js"></script>
```

Note that there currently seems to be something wrong with the pie chart functionality in NVD3 so use pie charts at your own risk!
