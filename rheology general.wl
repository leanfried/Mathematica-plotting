(* ::Package:: *)

(* ::Title:: *)
(*Rheology master sheet*)


depVariables = {"Frequency (Hz)", "G' (Pa)", "G\" (Pa)","tan\[Delta]","Strain (%)","\[Eta]* (Pa s)","Torque (g cm)","Phase Angle (\[Degree])",
			"Temperature (\[Degree]C)","Stress (Pa)","Force (g)"};


SetDirectory[NotebookDirectory[]];
folders = Select[FileNames["*", "*", Infinity], DirectoryQ];
folder = folders[[-1]];


SetterBar[Dynamic[folder], folders]


rheologyPlots[folder]


(* ::Subsection:: *)
(*get data from file*)


(* ::Text:: *)
(*get data from the file, row 3 to the second to last row, for columns x and y*)


getData[file_, x_, y_]:=Import[file, "tsv"][[3;;-2, {x, y}]]; 


(* ::Subsection:: *)
(*make plot styles for a set of samples*)


(* ::Text:: *)
(*get plot styles from a table, where rows are different data sets to plot, and columns are different parameters*)
(*limit 3 columns*)
(*no more than 12 unique values of the 2nd most varied parameter*)
(*no more than 4 unique values of the 3rd most varied parameter*)


getStyles[f_]:=Module[{numBins, numStyles, colorIndex, colors, colorValues, colorAssoc, markerIndex, markers, markerValues, markerAssoc,
					dottedIndex, dots, dottedValues, dottedAssoc},
Catch[
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
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]], dottedAssoc[#[[dottedIndex]]]}&/@f}];
			,
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]]}&/@f}];			
		];
	,
	Return[{numStyles, colorAssoc[#[[colorIndex]]]&/@f}];
	];
]]


(* ::Subsection:: *)
(*make one sample's label for the legend*)


(* ::Text:: *)
(*labels = a list of parameter names, list = a list of parameter values*)


makeLabel[labels_, list_]:= StringJoin[(ToString[list[[#]]]<>" "<>labels[[#]]<>If[#==Length[labels], "", ", "])&/@Range[Length[labels]]];


(* ::Subsection:: *)
(*break down list of files into association between file names and varied parameters*)


parseFiles[folder_]:=Module[{files, parsedFiles, indepColumns, indepVariables},
	files = FileNames["*.txt", folder, Infinity]; (*list of files in the folder*)
	If[Length[files]>1,
	parsedFiles = StringSplit[FileBaseName[#], {" ", "_", "-"}]&/@files;
	indepColumns = Flatten[Position[DeleteDuplicates/@Transpose[parsedFiles], u_/;Length[u]>1], Infinity];
	parsedFiles = <|(ToExpression[parsedFiles[[#, indepColumns]]]->files[[#]])&/@Range[Length[files]]|>;
	parsedFiles = KeySortBy[parsedFiles, Greater];
	indepVariables = Sort[ToExpression[#]]&/@Select[DeleteDuplicates/@Transpose[StringSplit[FileBaseName[#], {" ", "_", "-"}]&/@files], Length[#]>1&];
	{parsedFiles, indepVariables}
	,
	{<|0->files[[1]]|>, 0}]
	]


(* ::Subsection:: *)
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


rheologyPlots[folder_]:=DynamicModule[{parsedFiles, indepVariables, selectedVariables, labels, disp, style, files2plot, plotTitle, numParams, markers},
{parsedFiles, indepVariables} = parseFiles[folder]; 
	(*association connecting varied variables to file names*)
	(*list of possible values of variables*)
selectedVariables = indepVariables;
	(*selectedVariables will be the values that we want to plot*)
If[Length[indepVariables]>0, 
	labels =  ("Var "<>ToString@#<>"  ")&/@Range[Length[indepVariables]]; (*if more than one sample*)
	,
	labels = {};
]; 
	(*labels is the list of varied parameters*)
plotTitle = "";
	(*start with a blank plot title*)
Manipulate[
	If[Length[indepVariables]>0, (*if more than one sample*)
		files2plot = filterFiles[parsedFiles, selectedVariables];
			(*association that includes only the files we want to plot*)
		{numParams, style} = getStyles[Keys[files2plot]];
			(*plot styles for the files we want to plot*)
		, 
		files2plot = parsedFiles; style = Black; numParams = 0;
	];
	If[!NumberQ[style], 
		(*if style throws an error, display an error, otherwise go ahead and plot*)
		If[numParams>1, (*if there are more than 1 parameters*)
				markers = style[[;;,2]]; (*set markers to designated markers*)
				If[Length[style[[1]]]>2, (*if there are 3 parameters*)
					style = style[[;;, {1,3}]] (*set color and line style*)
				]
			, 
			markers = ConstantArray[\[FilledCircle], Length[style]](*if there is only one parameter, set markers to circle*)
		];
		disp = plotFormat[getData[#, xvar, yvar]&/@Values[files2plot], 
				FrameLabel->{depVariables[[xvar]], depVariables[[yvar]]}, 
				Joined->True, 
				ImageSize->{Automatic, imageSize}, 
				PlotStyle->style, 
				PlotMarkers->({#, markerSize}&/@markers), 
				LabelStyle->Directive[fontSize, Black],
				PlotLabel->plotTitle,
				PlotLegends->If[numParams>0, (Style[makeLabel[labels, #], fontSize, Black]&/@Keys[files2plot]), None]]; (*this is the plot*)
		Row[{
			Column[
				Prepend[
					Join[
						Row[{InputField[Dynamic[labels[[#]]], String, ImageSize->100], TogglerBar[Dynamic[selectedVariables[[#]]], indepVariables[[#]]]}]&/@Range[Length[indepVariables]] (*these are the parameters and values to include*)
						, 
						FileBaseName/@Values[files2plot] (*this a list of files that we're plotting*)
					]
					, Row[{"Plot title ", InputField[Dynamic[plotTitle], String, ImageSize->100]}] (*input box to name plot*)
				]
			]
			,"\t"
			, Column[{ disp, Button["Export", Export[SystemDialogInput["FileSave"], disp, "PDF"], Method->"Queued"]}] (*save button*)
		}]
	,
	"Too many variables"]
	, {{xvar, 1, "x variable"}, (#->depVariables[[#]])&/@Range[Length[depVariables]]} (*variable on x axis*)
	, {{yvar, 6, "y variable"}, (#->depVariables[[#]])&/@Range[Length[depVariables]]} (*variable on y axis*)
	, {{imageSize, 300, "Image size"}, 150, 1000,10} (*height of image*)
	, {{fontSize, 20, "Font size"}, 8, 30,1} (*font size*)
	, {{markerSize, 12, "Marker size"}, 8, 30,1} (*marker size*)
	, {{plotFormat, ListLogLogPlot, "Plot format"}, {ListLogLogPlot, ListLogPlot, ListPlot}} (*which axes should be log*)
	, ContinuousAction->False (*don't update the plot until you release the drag bars*)
	]]
