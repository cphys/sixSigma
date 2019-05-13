ClearAll["Global`*"]

(* Plot Parameters *)
iSize = 500;
scale = .125;
dataColor = Blue;
uclCLlclColors = {Red, Black, Green};
uclCLlclThickness = .005;
dataThickness = .01;

(* p value data *)
cVals = {5, 4, 14, 12, 5, 5, 12, 5, 8, 15, 9, 6};

(* data labels for output *)
labs = {"UCL(c) = ", "CL(c) = ", "LCL(c) = "};

(* calculate the uper control limit, center line,and lower control limit based on p value data and number of samples *)
uclCLlcl[meanCVals_] :=
 N[{
   meanCVals + 3*Sqrt[meanCVals],
   meanCVals,
   If[meanCVals - 3 * Sqrt[meanCVals] < 0, 0, meanCVals - 3 * Sqrt[meanCVals]]}]

(* add labels to calculated control limit data *)
uclCLlclLabels[meanCVals_, labs_] := 
 Table[labs[[j]] <> ToString[uclCLlcl[meanCVals][[j]]], {j, 1, Length[labs]}]

(* This function will go through all of the p values and return any values which are out of control *)
cValsOutOfControl[cVals_] :=
 Table[
  If[
   uclCLlcl[Mean[cVals]][[3]] <= cVals[[ix]] <= uclCLlcl[Mean[cVals]][[1]],
   Nothing,
   cVals[[ix]]],
  {ix, Length[cVals]}]

ucl = Evaluate[uclCLlcl[Mean[cVals]]][[1]];
cl = Evaluate[uclCLlcl[Mean[cVals]]][[2]];
lcl = Evaluate[uclCLlcl[Mean[cVals]]][[3]];

(* Create plots *)
plotCVals = ListLinePlot[cVals,
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> {dataColor, Thickness[dataThickness]},
   ImageSize -> iSize];

plotregions = 
  Plot[Evaluate[uclCLlcl[Mean[cVals]]], {x, 0, Length[cVals]},
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> Table[{uclCLlclColors[[i]], Thickness[uclCLlclThickness]}, {i, 1, Length[uclCLlclColors]}],
   PlotLegends -> {"ucl", "cl", "lcl"}];

Row[{Show[{plotCVals, plotregions}], Column[{"           ", "           "}], 
  Column[Append[uclCLlclLabels[Mean[cVals], labs], "Out of control pValues: " <> ToString[cValsOutOfControl[cVals]]]]}]
