(* ::Package:: *)

(* ::Title:: *)
(*Rheology master sheet*)


(* ::Text:: *)
(*This worksheet is for importing data from files, where data are stored in tabular format with each point in a new row*)


SetDirectory[NotebookDirectory[]];
RHEfolders = Select[FileNames["*", "*", Infinity], DirectoryQ];
RHEfolder = RHEfolders[[-1]];


(* ::Subsection:: *)
(*get data from file*)


(* ::Text:: *)
(*get data from the file, row 3 to the second to last row, for columns x and y*)


getData[file_, x_, y_]:=Module[{l},
	l = Select[Import[file][[3;;]], NumberQ[#[[x]]]&& NumberQ[#[[y]]]&];
	If[Length[l]>0,
		l[[;;, {x, y}]]
		,
		{{}}
	]]; 


(* ::Subsection::Closed:: *)
(*make plot styles for a set of samples*)


(* ::Text:: *)
(*get plot styles from a table, where rows are different data sets to plot, and columns are different parameters*)
(*limit 3 columns*)
(*no more than 12 unique values of the 2nd most varied parameter*)
(*no more than 4 unique values of the 3rd most varied parameter*)
(**)
(*if 1 varied parameter, *)


getStyles[f_]:=Module[{numBins, numStyles, colorIndex, colors, colorValues, colorAssoc, markerIndex, markers, markerValues, markerAssoc,
					dottedIndex, dots, dottedValues, dottedAssoc},
Catch[
	If[Length[f]>1, 
	numBins = CountDistinct/@Transpose[f];
	numStyles = Count[numBins, u_/;u>1];
	colorIndex = Ordering[numBins, -1][[1]];
	colors = (ColorData["DarkRainbow"][#/(numBins[[colorIndex]]-1)])&/@Range[0, numBins[[colorIndex]]-1];
	colorValues = DeleteDuplicates[f[[;;, colorIndex]]];
	colorAssoc = <|(colorValues[[#]]->colors[[#]])&/@Range[numBins[[colorIndex]]]|>;
	If[numStyles>1, 
		markerIndex = Ordering[numBins, -2][[1]];
		If[numBins[[markerIndex]]<12, markers = {"\[FilledCircle]","\[EmptyCircle]", "\[FilledSquare]", "\[EmptySquare]", "\[FilledUpTriangle]", "\[EmptyUpTriangle]", "\[FilledDiamond]","\[EmptyDiamond]", "\[FilledDownTriangle]", "\[EmptyDownTriangle]", "\[FivePointedStar]"}[[1;;numBins[[markerIndex]]]], Return[1]]; 
		markerValues = DeleteDuplicates[f[[;;, markerIndex]]];
		markerAssoc = <|(markerValues[[#]]->markers[[#]])&/@Range[numBins[[markerIndex]]]|>;	
		If[numStyles>2, 
			dottedIndex = Ordering[numBins, -3][[1]];
			If[numBins[[dottedIndex]]<4, dots = {Bold, Dashed, Dotted}[[1;;numBins[[dottedIndex]]]];, Return[1];];
			dottedValues = DeleteDuplicates[f[[;;, dottedIndex]]];
			dottedAssoc = <|(dottedValues[[#]]->dots[[#]])&/@Range[numBins[[dottedIndex]]]|>;
			(*3 varied parameters, return {1, list of {color, marker, line style}}*)
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]], dottedAssoc[#[[dottedIndex]]]}&/@f}];
			,
			(*2 varied parameters, return {1, list of {color, marker}}*)
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]]}&/@f}];			
		];
	,
	(*1 varied parameter: return {1, list of colors}*)
	Return[{numStyles, colorAssoc[#[[colorIndex]]]&/@f}];
	];
	,
	(*no files, return {-1, Black}*)
	Return[{-1, {Black}}]];
]]


(* ::Subsection:: *)
(*make one sample's label for the legend*)


(* ::Text:: *)
(*labels = a list of parameter names, list = a list of parameter values*)


makeLabel[labels_, list_]:= StringJoin[(ToString[list[[#]]]<>" "<>labels[[#]]<>If[#==Length[labels], "", ", "])&/@Range[Length[labels]]];


makeLabels[labels_, lists_]:=Module[{varied, lab, lis,l},
	If[Length[lists]<2
		,None
		,
		varied = Select[Position[DeleteDuplicates/@Transpose[lists], z_/;Length[z]>1], Length[#]>0&][[;;,1]];
		lab = labels[[varied]];
		lis = lists[[;;, varied]]; 
		makeLabel[lab, #]&/@lis
	]
]


(* ::Subsection::Closed:: *)
(*break down list of files into association between file names and varied parameters*)


parseFiles[RHEfolder_]:=Module[{files, parsedFiles, indepColumns, indepVariables, pFiles},
Catch[
files = FileNames["*.csv", RHEfolder, Infinity]; (*list of files in the folder*)
If[Length[files]>1,
	pFiles = StringSplit[FileBaseName[#], {" ", "_", "-"}]&/@files; (*split up files by parameters separated by space, underscore, or dash*)
	pFiles = PadRight[pFiles]; (*make all rows the same length*)
	indepColumns = Flatten[Position[DeleteDuplicates/@Transpose[pFiles], u_/;Length[u]>1], Infinity]; (*identify which columns are varied as a list of indices*)
	If[Length[indepColumns]<4, 
		parsedFiles = <|(ToExpression[pFiles[[#, indepColumns]]]->files[[#]])&/@Range[Length[files]]|>; (*make an association between the varied parameters and the file names*)
		parsedFiles = KeySortBy[parsedFiles, Greater]; (*sort by the varied parameters*)
		indepVariables = Sort[ToExpression[#]]&/@Select[DeleteDuplicates/@Transpose[pFiles], Length[#]>1&]; (*list of possible values for varied parameters*)
		{parsedFiles, indepVariables}
	,
		{files, 1}
	]
,
{files, 0}]
]]


(* ::Subsection::Closed:: *)
(*get list of files to plot*)


(* ::Text:: *)
(*indices of files to keep*)


positionsInList[index_, parsedFiles_, selectedVariables_]:=Flatten[Position[Keys[parsedFiles][[;;, index]], #]&/@selectedVariables[[index]]]


(* ::Text:: *)
(*list of files*)


filterFiles[parsedFiles_, selectedVariables_]:=parsedFiles[[Intersection@@(positionsInList[#, parsedFiles, selectedVariables]&/@Range[Length[selectedVariables]])]];


(* ::Text:: *)
(*set default plotting options*)


SetOptions[ListLogLogPlot, AspectRatio->1, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black]];
SetOptions[ListLogPlot, AspectRatio->1, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black]];
SetOptions[ListPlot, AspectRatio->1, Frame->True, FrameStyle->Black, LabelStyle->Directive[12, Black]];


(* ::Subsection:: *)
(*interface*)


(* ::Subsubsection:: *)
(*rheologyPlots*)


rheologyPlots[RHEfolders_]:=DynamicModule[{plotTitle, xvar, yvar, imageSize, fontSize, markerSize, plotFormat, defaultMarker
											, parsedFiles, indepVariables, RHEfolder, selectedVariables, labels
											, numParams, filePanel, selectedFiles,rows, depVariables, disp},

plotTitle = "";
	(*start with a blank plot title*)
xvar = 1;
yvar = 6;
imageSize = 400;
fontSize = 14;
markerSize = 16;
plotFormat = ListLogLogPlot;
defaultMarker = \[FilledCircle];
depVariables = {1->"Freq (Hz)",2->"G' (Pa)",3->"G\" (Pa)",4->"tan delta",5->"Strain (%)",6->"Viscosity (Pa-s)",7->"Torque (g-cm)",8->"Phase angle (\[Degree])",9->"Temperature (\[Degree]C)",10->"Stress (Pa)",11->"Force (g)"};
disp = "Select folder";
filePanel = "";
selectedFiles = {};
parsedFiles = {};
paramPlot = False;
Panel[
Row[{
	Column[{
		Grid[{
		{Style["Folders", "Section"],Style["Files", "Section"]}
		,{
			Column[{TogglerBar[Dynamic[RHEfolder], RHEfolders, Appearance->"Vertical"]
			, Button["Update file list",
					If[Length[RHEfolder]>0,
						selectedFiles = {};
						{parsedFiles, indepVariables} = parseFiles[RHEfolder]; 
						(*association connecting varied variables to file names*)
						(*list of possible values of variables*)
						If[Length[indepVariables]>0, 
							(*if more than one sample but fewer than 4 parameters*)
							selectedVariables = {#[[1]]}&/@indepVariables;
							(*selectedVariables will be the values that we want to plot*)							
							labels =  ("Var "<>ToString@#<>"  ")&/@Range[Length[indepVariables]]; (*if more than one sample*) 	(*labels is the list of varied parameters*)
							filePanel = Column[Row[{
												InputField[Dynamic[labels[[#]]], String, ImageSize->100]
												, 
												TogglerBar[Dynamic[selectedVariables[[#]]], indepVariables[[#]]]
											}]&/@Range[Length[indepVariables]]]; (*these are the parameters and values to include*)
							, 
							filePanel = TogglerBar[Dynamic[selectedFiles], (#->FileBaseName[#])&/@parsedFiles, Appearance->"Vertical"->{10, Automatic}];
					]];
				]
				}] 
				,Dynamic[filePanel]}
		}, Alignment->Left, ItemSize->{Automatic, {4, 10}}]
		,""
		, Dynamic[If[Length[indepVariables]>0,Column[FileBaseName/@selectedFiles], ""]]
		,Style["\nPlot variables", "Section"]
		,Row[{Button["Viscosity plot", xvar = 1; yvar = 6;],Button["G'/G\"", xvar = 10; yvar = 2;]}]
		,Grid[{
			{"Plot title ", InputField[Dynamic[plotTitle], String, ImageSize->100]} (*input box to name plot*)
			,{"x variable ", SetterBar[Dynamic[xvar], depVariables, Appearance->"Row"]} (*variable on x axis*)
			,{"y variable ", SetterBar[Dynamic[yvar], depVariables, Appearance->"Row"]}
			,{"Image size ", InputField[Dynamic[imageSize], Number, ImageSize->100]}
			,{"Font size ", InputField[Dynamic[fontSize], Number, ImageSize->100]}
			,{"Marker size ", InputField[Dynamic[markerSize], Number, ImageSize->100]}
			,{"Plot format ", SetterBar[Dynamic[plotFormat], {ListLogLogPlot, ListLogPlot, ListPlot}]} (*which axes should be log*)
			,{"Marker", SetterBar[Dynamic[defaultMarker], {"\[FilledCircle]","\[EmptyCircle]", "\[FilledSquare]", "\[EmptySquare]", "\[FilledUpTriangle]", "\[EmptyUpTriangle]", "\[FilledDiamond]","\[EmptyDiamond]", "\[FilledDownTriangle]", "\[EmptyDownTriangle]", "\[FivePointedStar]"}]}
		}, Alignment->{{Right, Left}, Automatic}, ItemSize->{{6, 40}, Automatic}]

	}]
	,"\t"
	,Column[{	
	Button["Update plot ",
			If[Length[parsedFiles]>0,
				{selectedFiles, disp} = getPlotRhe[indepVariables
											, If[Length[indepVariables]>0, parsedFiles, selectedFiles]
											, selectedVariables
											, imageSize
											, defaultMarker
											, xvar
											, yvar
											, fontSize
											, markerSize
											, plotFormat
											, plotTitle
											, depVariables
											, labels];
				,
				disp = "Select folder"
			], ImageSize->imageSize]
	,Dynamic[Panel[Column[{
			disp
			, If[Length[selectedFiles]>0, Button["Export", Export[SystemDialogInput["FileSave"], disp, "PDF"], Method->"Queued"],""]
	}]]]
	}] (*save button*)
}]]
]


(* ::Subsubsection:: *)
(*getPlotRhe*)


getPlotRhe[indepVariables_, parsedFiles_, selectedVariables_, imageSize_, defaultMarker_, xvar_, yvar_, fontSize_, markerSize_, plotFormat_, plotTitle_, depVariables_, labels_]:=
Module[{sf, numParams, style, markers, legend, disp, (*labels, *)files2plot, model, modelPlot, data, tp, legends},
Catch[

If[Length[indepVariables]>0
	, (*plot styles according to parameters*)
		files2plot = filterFiles[parsedFiles, selectedVariables]; (*association that includes only the files we want to plot*)
		sf = Values[files2plot];	
	   legend = makeLabels[labels, Keys[files2plot]];
	   {numParams, style} = getStyles[Keys[files2plot]]; (*plot styles for the files we want to plot*)
	   Switch[numParams
	   ,-1,
			markers = defaultMarker;
		,1,
			markers = ConstantArray[defaultMarker, Length[style]];
		,2,
			markers = style[[;;,2]]
		,3,
			markers = style[[;;,2]];
			style = style[[;;, {1,3}]];
		];
	, (*default plot styles*) 
	  sf = parsedFiles;
	  If[indepVariables==1 
		, (*too many variables to plot*) 
		  legend = FileBaseName/@sf;
		  style= Automatic;		
		  markers = Automatic;
		, (*just one file*) 
		  legend = None;
		  style = Black; 
		  markers = ConstantArray[defaultMarker, Length[style]];
	]
];

If[xvar==10 && (yvar==2 || yvar==3)
	, If[Length[sf]==1, legend = {""}];
	  disp = getGGPlot[sf, fontSize, imageSize, markerSize, legend, plotTitle];
	, data = getData[#, xvar, yvar]&/@sf;
		disp = plotFormat[data, 
				FrameLabel->{Association[depVariables][xvar], Association[depVariables][yvar]}, 
				Joined->True, 
				ImageSize->{Automatic, imageSize}, 
				PlotStyle->style, 
				PlotMarkers->({#, markerSize}&/@markers), 
				LabelStyle->Directive[fontSize, Black],
				PlotLabel->plotTitle,
				Ticks->{4,4},
				PlotLegends->legend]; 
		If[xvar==1 && yvar==10,
			model = models[Import[#], False]&/@sf;
			tp = model[[;;,1]];
			legends = model[[;;,2]];
			modelPlot = Switch[plotFormat, ListLogLogPlot, LogLogPlot, ListLogPlot, LogPlot, ListPlot, Plot][
					tp, {\[Gamma], Min[data[[;;,;;,1]]], Max[data[[;;,;;,1]]]}, PlotLegends->legends, PlotStyle->Darker/@style];
			disp = Show[disp, modelPlot]
		];
];

Return[{sf, disp}]
]]


(* ::Subsection:: *)
(*G', G"*)


getGG[file_]:=Module[{l,c, ggp, g1, g2, xinit, diffs},
	l = Select[Import[file], Length[#]>1 && NumberQ[#[[1]]]&];
	l = Prepend[l[[Select[Range[2, Length[l]], l[[#, 10]]>l[[#-1, 10]]&]]], l[[1]]];
	l = Select[l, #[[2]]>0 && #[[3]]>0&];
	ggp = {l[[3;;, {10,2}]], l[[3;;, {10,3}]]};
(*	g1 = Interpolation[ggp[[1]]];
	g2 = Interpolation[ggp[[2]]];
	xinit = Mean[ggp[[1,{1,-1},1]]];
	c = Quiet[Check[x/.FindRoot[g1[x]==g2[x], {x, xinit, Min[ggp[[1,;;,1]]], Max[ggp[[1,;;,1]]]}], 0]];*)
	c = Select[Range[2, Length[ggp[[1]]]-1], ggp[[1, #-1,2]] > ggp[[2, #-1,2]] && ggp[[1, #+1,2]] < ggp[[2, #+1,2]]&];
	If[Length[c]>0 && Max[c]-Min[c]<=2, c = Mean[ggp[[1, c, 1]]], c = 0];
	{ggp, N[c]}]


gg[RHEfolder_]:=Manipulate[DynamicModule[{filenames, fileList, plot},
filenames = FileNames["*csv",RHEfolder];
fileList = {};
plot = Graphics[];
Row[{
	Column[{
		TogglerBar[Dynamic[fileList], filenames, Appearance->"Vertical"]
		,Button["Update", 
			plot = getGGPlot[fileList, fontSize, imageSize, pointSize, FileBaseName/@fileList, ""]
		]
	}]
	,"\t"
	, Dynamic[plot]
}]]
,
{{fontSize, 14, "Font size"}, 10, 30},
{{imageSize, 600, "Image size"}, 200, 1000},
{{pointSize, 14, "Point size"}, 10, 30},
ContinuousAction->False]


(*getGGPlot[fileList_, fontSize_, imageSize_, pointSize_, legend_, plotTitle_]:=Module[{toPlot, colors, data, maxG, minG, plot},
Print[legend];
		If[Length[fileList]>0,
					If[Length[fileList]>1,
						toPlot = getGG/@fileList;
						data = Flatten[toPlot[[;;,1]],1];
						maxG = Max[data[[;;,;;,2]]];
						minG = Min[data[[;;,;;,2]]];
						data = Flatten[Append[#[[1]], If[#[[2]]>0,{{#[[2]], minG}, {#[[2]], maxG}}, {{}}]]&/@toPlot,1];
					, 
						toPlot = getGG[fileList[[1]]];
						data = toPlot[[1]];
						maxG = Max[data[[;;,;;,2]]];
						minG = Min[data[[;;,;;,2]]];
						data = Append[data, If[toPlot[[2]]>0,{{toPlot[[2]], minG}, {toPlot[[2]], maxG}},{{}}]];
					];
					colors = getColors[Length[fileList]];
					plot = ListLogLogPlot[
							data
							, Joined->True 
							, PlotLegends->If[Length[legend]>0,Flatten[{"G'   "<>#, "G\"", ""}&/@legend,1], legend]
							, FrameLabel->{"Stress (Pa)", "Modulus (Pa)"}
							, PlotStyle->Flatten[{#, {#, Dashed}, {#, Dotted}}&/@colors,1]
							, PlotMarkers->{{\[FilledCircle],pointSize}, {\[EmptyCircle],pointSize},{Subscript["\[Sigma]","y"], fontSize}}
							, LabelStyle->Directive[fontSize, Black]
							, ImageSize->imageSize
							, PlotLabel->plotTitle
					];
		,
		plot = Graphics[];
		];
		plot
]*)


getGGPlot[fileList_, fontSize_, imageSize_, pointSize_, legend_, plotTitle_]:=Module[{toPlot, colors, data, maxG, minG, plot, yields, plot2},

		If[Length[fileList]>0,
					If[Length[fileList]>1,
						toPlot = getGG/@fileList;
						data = Flatten[toPlot[[;;,1]],1];
						maxG = Max[data[[;;,;;,2]]];
						minG = Min[data[[;;,;;,2]]];
						yields = If[#[[2]]>0,{{#[[2]], minG}, {#[[2]], maxG}}, {{-1, minG}, {-1, maxG}}]&/@toPlot;
					, 
						toPlot = getGG[fileList[[1]]];
						data = toPlot[[1]];
						maxG = Max[data[[;;,;;,2]]];
						minG = Min[data[[;;,;;,2]]];
						yields = If[toPlot[[2]]>0,{{toPlot[[2]], minG}, {toPlot[[2]], maxG}},0];
					];
					colors = getColors[Length[fileList]];
					plot = ListLogLogPlot[
							data
							, Joined->True 
							, PlotLegends->If[Length[legend]>0,Flatten[{"G'   "<>#, "G\""}&/@legend,1], legend]
							, FrameLabel->{"Stress (Pa)", "Modulus (Pa)"}
							, PlotStyle->Flatten[{#, {#, Dashed}}&/@colors,1]
							, PlotMarkers->{{\[FilledCircle],pointSize}, {\[EmptyCircle],pointSize}}
							, LabelStyle->Directive[fontSize, Black]
							, ImageSize->imageSize
							, PlotLabel->plotTitle
					];
					If[Length[yields]>0,
						plot2 = ListLogLogPlot[yields
							, Joined->True
							, PlotStyle->({#, Dotted}&/@colors)
							, PlotMarkers->{Subscript["\[Sigma]","y"], fontSize}		
						];
						,
						plot2 = Graphics[];
					];
					plot = Show[plot, plot2];
		,
		plot = Graphics[];
		];
		plot
]


(* ::Text:: *)
(*get a list of n colors, evenly distributed across the DarkRainbow color spectrum*)


getColors[n_Integer]:=If[n>1,ColorData["DarkRainbow"]/@(Range[0,n-1]/(n-1)), {Black}];


fixFile[txtFile_]:=Module[{grid},
grid = Select[Import[txtFile, "tsv"], Length[#]>1&];
If[Length[grid[[1]]]>7,
grid[[1]] = Flatten[StringSplit/@grid[[1]]];
If[Length[grid[[1]]]<11,
	grid = Transpose[Append[Insert[Transpose[grid], ConstantArray["", Length[grid[[;;,1]]]], {{8}}],  ConstantArray["", Length[grid[[;;,1]]]]]];
];
grid[[1]] = {"Freq", "G'", "G\"", "tan delta", "Strain", "Viscosity", "Torque", "Phase angle", "Temperature", "Stress", "Force"};
Export[StringReplace[txtFile, {".txt"->".csv", "dynamic frequency sweep"->"dfs"}],grid];
];
]


models[t_, print_]:=Module[{Bingham, HerschelBulkley, Casson, Sisko, RobertsonStiff, HeinzCasson, MizhariBerk, functions, function},
Quiet[Bingham = NonlinearModelFit[t[[3;;, {1, 10}]], t0+ninf*\[Gamma], {t0, ninf}, \[Gamma]];
HerschelBulkley = NonlinearModelFit[t[[3;;, {1, 10}]], t0+k*\[Gamma]^n, {t0, k, n}, \[Gamma]];
Casson = NonlinearModelFit[t[[3;;, {1, 10}]], (kOC + kC*\[Gamma]^0.5)^2, {kOC, kC}, \[Gamma]];
Sisko = NonlinearModelFit[t[[3;;, {1, 10}]], a*\[Gamma] + b*\[Gamma]^n, {a, b, n}, \[Gamma]];
RobertsonStiff =  NonlinearModelFit[t[[3;;, {1, 10}]], k*(\[Gamma]0 + \[Gamma])^n, {k, \[Gamma]0, n}, \[Gamma]];
HeinzCasson =  NonlinearModelFit[t[[3;;, {1, 10}]], (\[Gamma]0^n + k*(\[Gamma])^n)^(1/n), {k, \[Gamma]0, n}, \[Gamma]];
MizhariBerk =  NonlinearModelFit[t[[3;;, {1, 10}]], (kOM + kM*(\[Gamma])^nM)^(2), {kOM, kM, nM}, \[Gamma]];

functions = {{Bingham, "Bingham"}
			, {HerschelBulkley, "Herschel Bulkley"}
			, {Casson, "Casson"}
			, {Sisko, "Sisko"}
			, {RobertsonStiff, "Robertson Stiff"}
			, {HeinzCasson, "Heinz Casson"}
			, {MizhariBerk, "Mizhari Berk"}};
Quiet[functions = Select[functions, Im[#[[1]][1]]==0 && Im[#[[1]]["RSquared"]]==0&]];
If[print, Print[Grid[Table[{f[[2]], Normal[f[[1]]], f[[1]]["RSquared"]}, {f, functions}]]]]; 
function = MaximalBy[functions, #[[1]]["RSquared"]&][[1]];];
{function[[1]][\[Gamma]], Row[{function[[2]],"  ", Normal[function[[1]]]}]}
]
