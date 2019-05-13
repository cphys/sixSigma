ClearAll["Global`*"]
(* Plot Parameters *)
iSize = 500;
scale = .125;
dataColor = Blue;
uclCLlclColors = {Red, Black, Green};
uclCLlclThickness = .005;
dataThickness = .01;
(* There are 6 steps to the procedure of calculating ANOVA:
    State null (H0) and alternative (H1) hypotheses.
    Define the level of significance \[Alpha].
    Construct the ANOVA table.
    Compute the test statistic.
    Determine critical values of the test statistic in the F-table.
    State the conclusion of the test (Can H0 be rejected?). *)

(* These are control parameters *)

A2Vals = {1.88, 1.023, 0.729, 0.577, 0.483, 0.419, 0.373, 0.337, 0.308, 0.285, 0.266, 0.249, 0.235, 0.223};
D3Vals = {0, 0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223, 0.256, 0.283, 0.307, 0.328, 0.347};
D4Vals = {3.267, 2.574, 2.282, 2.114, 2.004, 1.924, 1.864, 1.816, 1.777, 1.744, 1.717, 1.693, 1.672, 1.653};

(* Input data *)
xVals = {93, 92.8, 92, 92.6, 92, 91.9, 92.4, 94.125, 91.8, 91.3};
rVals = {2, 7.1, 4, 1.7, 2.5, 1.8, 3.1, 3.3, 2.4, 3.2};
sampNubs = 6;

(* data labels for output *)
labs = {{"UCL(\!\(\*OverscriptBox[\(x\), \(_\)]\)) = ", "CL(\!\(\*OverscriptBox[\(x\), \(_\)]\)) = ", "LCL(\!\(\*OverscriptBox[\(x\), \(_\)]\)) = "}, {"UCL(R) = ", "CL(R) = ", "LCL(R) = "}};

(* calculate the uper control limit, center line, and lower control limit based on p value data and number of samples *)
uclCLlcl[meanxVals_, meanrVals_, numbOfSamples_] :=
 N[{{
    meanxVals + A2Vals[[numbOfSamples - 1]]*meanrVals,
    meanxVals,
    meanxVals - A2Vals[[numbOfSamples - 1]]*meanrVals},
   {
    D4Vals[[numbOfSamples - 1]]*meanrVals,
    meanrVals,
    D3Vals[[numbOfSamples - 1]]}}]

(* add labels to calculated control limit data *)
uclCLlclLabels[meanxVals_, meanrVals_, numbOfSamples_, labs_] := 
 Table[labs[[j, i]] <> 
   ToString[uclCLlcl[meanxVals, meanrVals, numbOfSamples][[j, i]]], {j, 1,Length[labs]}, {i, Length[labs[[1]]]}]

(* This function will go through all of the x values and return any values which are out of control *)
xValsOutOfControl[xVals_, rVals_, numbOfSamples_] :=
 Table[
  If[
   uclCLlcl[Mean[xVals], Mean[rVals], numbOfSamples][[1, 3]] <= xVals[[ix]] <= uclCLlcl[Mean[xVals], Mean[rVals], numbOfSamples][[1, 1]],
   Nothing,
   xVals[[ix]]],
  {ix, Length[xVals]}]

(* This function will go through all of the r values and return any values which are out of control *)
rValsOutOfControl[xVals_, rVals_, numbOfSamples_] :=
 Table[
  If[
   uclCLlcl[Mean[xVals], Mean[rVals], numbOfSamples][[2, 3]] <= rVals[[ix]] <= uclCLlcl[Mean[xVals], Mean[rVals], numbOfSamples][[2, 1]],
   Nothing,
   rVals[[ix]]],
  {ix, Length[xVals]}]

(* What is the minimum number of samples such that UCL-LCL > someValue *)
minValDiff[meanxVals_, meanrVals_, someValue_] := 
 Min[Table[
   If[(uclCLlcl[meanxVals, meanrVals, iN][[1, 1]] - uclCLlcl[meanxVals, meanrVals, iN][[1, 3]]) < someValue, iN, Nothing], {iN, 2, Length[A2Vals]}]]

ucl = Evaluate[uclCLlcl[Mean[xVals], Mean[rVals], sampNubs]][[1, 1]];
cl = Evaluate[uclCLlcl[Mean[xVals], Mean[rVals], sampNubs]][[1, 2]];
lcl = Evaluate[uclCLlcl[Mean[xVals], Mean[rVals], sampNubs]][[1, 3]];

(* Create plots *)
plotCVals = ListLinePlot[xVals,
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> {dataColor, Thickness[dataThickness]},
   ImageSize -> iSize];

plotregions = 
  Plot[Evaluate[uclCLlcl[Mean[xVals], Mean[rVals], sampNubs]], {x, 0,Length[xVals]},
   PlotRange -> {Automatic, {lcl - (ucl - lcl)*scale, ucl + (ucl - lcl)*scale}},
   PlotTheme -> "Detailed",
   PlotStyle -> Table[{uclCLlclColors[[i]], Thickness[uclCLlclThickness]}, {i, 1, Length[uclCLlclColors]}],
   PlotLegends -> {"ucl", "cl", "lcl"}];

Row[{Show[{plotCVals, plotregions}], 
  Column[{"           ", "           "}], Column[Append[uclCLlclLabels[Mean[xVals], Mean[rVals], sampNubs, labs], "Out of control x Values: " <> ToString[xValsOutOfControl[xVals, rVals, sampNubs]]]]}]
