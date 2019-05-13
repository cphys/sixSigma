ClearAll["Global`*"]

(* calculate probabilities under normal distribution function *)
probNum[\[Mu]_, \[Sigma]_, from_, to_] := NIntegrate[PDF[NormalDistribution[\[Mu], \[Sigma]], x], {x, from, to}]

prob[\[Mu]_, \[Sigma]_, from_, to_] := Integrate[PDF[NormalDistribution[\[Mu], \[Sigma]], x], {x, from, to}]

(* If given a confidence level and need a zAlphaOverTwo term  *)
zAlphOverTwoCalc[conLevel_] := 
 Solve[1 - 2*prob[0, 1, z, \[Infinity]] == conLevel, z][[1, 1, 2]] //Quiet

oneSidedzAlphOverTwoCalc[conLevel_] := 
 Solve[1 - prob[0, 1, z, \[Infinity]] == conLevel, z][[1, 1, 2]] //  Quiet

(* If given a  zAlphaOverTwo term and need confidence interval  *)
conLevCalc[zAlphOverTwo_] := 1 - 2*probNum[0, 1, zAlphOverTwo, \[Infinity]] oneSidedConLevCalc[zAlph_] := 1 - probNum[0, 1, zAlph, \[Infinity]]

(* Calculate kValue if given a list a confidence interval and a stdev *)
kVal[listSamp_, zAlphOverTwo_, \[Sigma]_] := zAlphOverTwo*(\[Sigma]/Sqrt[Length[listSamp]])

(* calculate confidence interval based on % confidence for a list of parts *)
conInt[listSamp_, zAlphOverTwo_, \[Sigma]_] := {Mean[listSamp] - kVal[listSamp, zAlphOverTwo, \[Sigma]], Mean[listSamp] + kVal[listSamp, zAlphOverTwo, \[Sigma]]}

(* if given a stdDev, a list, and an interval what is the % confidence? *)
conValueTest[listSamp_, \[Sigma]_, interval_] := probNum[Mean[listSamp], \[Sigma]/Sqrt[Length[listSamp]], interval[[1]], interval[[2]]]

(* give confidence value if given a list a zAlphaOver two and a sigma *)
conValue[listSamp_, zAlphOverTwo_, \[Sigma]_] := probNum[Mean[listSamp], \[Sigma]/Sqrt[Length[listSamp]], conInt[listSamp, zAlphOverTwo, \[Sigma]][[1]], conInt[listSamp, zAlphOverTwo, \[Sigma]][[2]]]

(* if given a mean, stdev, interval, and confidence interval calc number of samples needed *)
numberOfSamples[\[Mu]_, \[Sigma]_, interval_, conLevel_] := Solve[prob[\[Mu], \[Sigma]/Sqrt[n], interval[[1]], interval[[2]]] == conLevel, n][[1, 1, 2]] // Quiet

(* if given a list a standard deviation and z * \alpha / 2  *)
listAsamp = {246, 253, 250, 254, 247}; (* list given  *)
testInterval = {244.54, 255.46};  (* answer test if chosen interval is correct *)
conInt[listAsamp, zAlphOverTwoCalc[.98], 5.25]
conValueTest[listAsamp, 5.25, testInterval]


numberOfSamples[250, 5.25, {249.61, 250.39}, conLevCalc[2.326]]


listAsamp2 = {23.5, 22.4, 23, 21.9, 22.6, 22.8}; (* list given  *)
conInt[listAsamp2, zAlphOverTwoCalc[.95], .55]


(* calculate the p-value *)
probOutsideH0[listSamp_, \[Sigma]_, H0_] := probNum[Mean[listSamp], \[Sigma]/Sqrt[Length[listSamp]], H0, \[Infinity]]

probOutsideH0noList[\[Mu]_, \[Sigma]_, n_, H0_] := probNum[\[Mu], \[Sigma]/Sqrt[n], H0, \[Infinity]]

(* are the results significant *)
isSignificant[listSamp_, \[Sigma]_, H0_, \[Alpha]_] := If[
   probOutsideH0[listSamp, \[Sigma], H0] < \[Alpha],
   "The results are significant",
   "The results are not significant"] // Quiet

isSignificantNoList[\[Mu]_, \[Sigma]_, n_, H0_, \[Alpha]_] := If[
   probOutsideH0noList[\[Mu], \[Sigma], n, H0] < \[Alpha],
   "The results are significant",
   "The results are not significant"] // Quiet

H0value = 1.8;
sampleMean = 2.1;
sampleSize = 10;
stdevValue = 0.8;
alphValue = .1;
"Sample Mean = " <> ToString[sampleMean]
"p-value = " <> ToString[probOutsideH0noList[sampleMean, stdevValue, sampleSize, H0value]]
"alpha value = " <> ToString[alphValue]
"zobs = " <> ToString[oneSidedzAlphOverTwoCalc[probOutsideH0noList[sampleMean, stdevValue, sampleSize, H0value]]]
isSignificantNoList[sampleMean, stdevValue, sampleSize, H0value, alphValue]


testList = {.7, .61, .65, .62};
H0value = .6;
stdevValue = .05;
alphValue = .025;
"Sample Mean = " <> ToString[N[Mean[testList]]]
"Sample StDev = " <> ToString[N[StandardDeviation[testList]]]
"p-value = " <> ToString[probOutsideH0[testList, stdevValue, H0value]]
"alpha value = " <> ToString[alphValue]
isSignificant[testList, stdevValue, H0value, alphValue]

