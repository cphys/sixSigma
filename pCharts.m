ClearAll["Global`*"]
(* Plot Parameters *)
iSize = 500;
scale = .125;
dataColor = Blue;
uclCLlclColors = {Red, Black, Green};
uclCLlclThickness = .005;
dataThickness = .01;

(* p value data *)
pVals = {.025, .0125, 0, 0.03, .035, .0125, .0525, 0, 0.05, .0125};
sampNubs = 80;

(* data labels for output *)
labs = {"UCL(P) = ", "CL(P) = ", "LCL(P) = "};

(* calculate the uper control limit, center line, and lower control limit based on p value data and number of samples *)

uclCLlcl[meanPVals_, numbOfSamples_] :=
 N[{
   meanPVals + 3*Sqrt[(meanPVals (1 - meanPVals))/numbOfSamples],
   meanPVals,
   If[meanPVals - 3*Sqrt[(meanPVals (1 - meanPVals))/numbOfSamples] < 0,
    0,
    meanPVals - 3*Sqrt[(meanPVals (1 - meanPVals))/numbOfSamples]]}]

(* add labels to calculated control limit data *)
uclCLlclLabels[meanPVals_, numbOfSamples_, labs_] := 
 Table[labs[[j]] <> ToString[uclCLlcl[meanPVals, numbOfSamples][[j]]], {j, 1, Length[labs]}]

(* This function will go through all of the p values and return any values which are out of control *)
pValsOutOfControl[pVals_, numbOfSamples_] :=
 Table[
  If[
   uclCLlcl[Mean[pVals], numbOfSamples][[3]] <= pVals[[ix]] <= uclCLlcl[Mean[pVals], numbOfSamples][[1]],
   Nothing,
   pVals[[ix]]],
  {ix, Length[pVals]}]

(* What is the minimum number of samples such that UCL-LCL > someValue *)
minValDiff[meanPVals_, someValue_] := Min[Table[ If[(uclCLlcl[meanPVals, iN][[1, 1]] - uclCLlcl[meanPVals, iN][[1, 3]]) < someValue, iN, Nothing], {iN, 1, 500}]]

ucl = Evaluate[uclCLlcl[Mean[pVals], sampNubs]][[1]];
cl = Evaluate[uclCLlcl[Mean[pVals], sampNubs]][[2]];
lcl = Evaluate[uclCLlcl[Mean[pVals], sampNubs]][[3]];

(* Create plots *)
plotCVals = ListLinePlot[pVals,
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, 
      ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> {dataColor, Thickness[dataThickness]},
   ImageSize -> iSize];

plotregions = 
  Plot[Evaluate[uclCLlcl[Mean[pVals], sampNubs]], {x, 0, 
    Length[pVals]},
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, 
      ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> 
    Table[{uclCLlclColors[[i]], Thickness[uclCLlclThickness]}, {i, 1, 
      Length[uclCLlclColors]}],
   PlotLegends -> {"ucl", "cl", "lcl"}];

Row[{Show[{plotCVals, plotregions}], Column[{"           ", "           "}], 
  Column[Append[uclCLlclLabels[Mean[pVals], sampNubs, labs], "Out of control pValues: " <> ToString[pValsOutOfControl[pVals, sampNubs]]]]}]
