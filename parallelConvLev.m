ClearAll["Global`*"]

parallelInterval[list_, samStdev_, confVal_, numbCpu_] := (
   meanList = N[Mean[list]];
   
   (* give the confidence level for a given k value *)   
   conLev[k_] := conLev[k] = NIntegrate[PDF[NormalDistribution[meanList, samStdev/Sqrt[Length[list]]], x], {x, meanList - k, meanList + k}];
   
   returnKval[stepSize_, k_] := returnKval[k] = (
      confValTest = N[ParallelMap[# <= confVal &,
         ParallelTable[conLev[k + i*stepSize],
          {i, 0, numbCpu - 1}]]];
      N[ParallelMap[
        If[#,
          Nothing,
          k + (8 - $KernelID)*stepSize] &,
        confValTest]]);

   confInT[start_, stepSize_] := confInT[start, stepSize] = (
      N[Do[
        If[Length[returnKval[stepSize, k]] == 0,
         Nothing,
         (Return[{{meanList - returnKval[stepSize, k][[1]], meanList + returnKval[stepSize, k][[1]]}, conLev[k]}]; 
          Break[])],
        {k, start, meanList/2, stepSize}]]);

   intVal[idk_] := intVal[idk] = N[.1^idk];
   newStart = 0;

   Do[If[
     (Abs[confInT[newStart, intVal[idk]][[2]] - sampConLev] / sampConLev) > .01,
     newStart = ((confInT[newStart, intVal[idk]][[1, 2]] - confInT[newStart, intVal[idk]][[1, 1]]) / 2) - intVal[idk],
     Return[confInT[newStart, intVal[idk]]]; Break[]],
    {idk, 0., 10.}]);

(* If we are given a list such as that in listAsamp and we want the 98% confidence iterval for a distribution with a standard deviatio \[Sigma]=5.25 *)
(* sample given list of data *)
listAsamp = RandomInteger[{236, 256}, 10000];
sampStdev = 5.25;
sampConLev = .98;
cpuNumb = 8;
Timing[parallelInterval[listAsamp, sampStdev, sampConLev, cpuNumb]]
