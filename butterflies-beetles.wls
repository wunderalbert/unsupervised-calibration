#!/usr/bin/env wolframscript
folder = $ScriptCommandLine[[2]];
files = FileNames["*.jpg", folder];
samplesize = ToExpression[$ScriptCommandLine[[3]]];

Print[StringJoin["Will write to: ", FileNameJoin[{folder, StringJoin["out_", ToString[samplesize], ".csv"]}]]]

Print[StringJoin["Resizing images to ", ToString[samplesize]]]
images = ImageResize[Import[#], {samplesize}] & /@ files;
model = NetModel["Wolfram ImageIdentify Net V1"];

Print("Computing butterflies")
butterflies = 
 Map[With[{image = #}, 
    model[image, "Probabilities"][
      Entity["Concept", "CommaButterfly::w98vn"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "RingletButterfly::28v7s"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "SulfurButterfly::b9263"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "TortoiseshellButterfly::hb63c"]]] &, images];
Print("Computing beetles")
beetles = 
 Map[With[{image = #}, 
    model[image, "Probabilities"][
      Entity["Concept", "ClickBeetle::hz374"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "ColoradoBeetle::7b393"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "FireBeetle::t3348"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "FleaBeetle::3794s"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "JapaneseBeetle::9kvxt"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "RhinocerosBeetle::23y53"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "RoveBeetle::5xsv6"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "StagBeetle::49pys"]] + 
     model[image, "Probabilities"][
      Entity["Concept", "TigerBeetle::8362y"]]] &, images];
Print("Writing output")
output = Dataset[
  Map[Association[{"filename" -> #[[1]], "butterfiles" -> #[[2]], 
      "beetles" -> #[[3]]}] &, 
   Transpose[{files, butterflies, beetles}]]];
Export[FileNameJoin[{folder, StringJoin["out_", ToString[samplesize], ".csv"]}], output]
Print("All done!")
