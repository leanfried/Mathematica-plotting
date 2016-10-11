(* ::Package:: *)

(* ::Title:: *)
(*Rheology master sheet*)


(* ::Text:: *)
(*This worksheet is for importing data from files, where data are stored in tabular format with each point in a new row*)


depVariables = {"Frequency (Hz)", "G' (Pa)", "G\" (Pa)","tan\[Delta]","Strain (%)","\[Eta]* (Pa s)","Torque (g cm)","Phase Angle (\[Degree])",
			"Temperature (\[Degree]C)","Stress (Pa)","Force (g)"};


SetDirectory[NotebookDirectory[]];
folders = Select[FileNames["*", "*", Infinity], DirectoryQ];
folder = folders[[-1]];


TogglerBar[Dynamic[folder], folders]


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
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]], dottedAssoc[#[[dottedIndex]]]}&/@f}];
			,
			Return[{numStyles, {colorAssoc[#[[colorIndex]]], markerAssoc[#[[markerIndex]]]}&/@f}];			
		];
	,
	Return[{numStyles, colorAssoc[#[[colorIndex]]]&/@f}];
	];
	,
	Return[{-1, {Black}}]];
]]


(* ::Subsection:: *)
(*make one sample's label for the legend*)


(* ::Text:: *)
(*labels = a list of parameter names, list = a list of parameter values*)


makeLabel[labels_, list_]:= StringJoin[(ToString[list[[#]]]<>" "<>labels[[#]]<>If[#==Length[labels], "", ", "])&/@Range[Length[labels]]];


(* ::Subsection:: *)
(*break down list of files into association between file names and varied parameters*)


parseFiles[folder_]:=Module[{files, parsedFiles, indepColumns, indepVariables, pFiles},
Catch[
files = FileNames["*.txt", folder, Infinity]; (*list of files in the folder*)
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


rheologyPlots[folder_]:=DynamicModule[{parsedFiles, indepVariables, selectedVariables, labels, disp, style, files2plot, plotTitle, numParams, markers, selectedFiles, paramPlot, error, goPlot},
{parsedFiles, indepVariables} = parseFiles[folder]; 
	(*association connecting varied variables to file names*)
	(*list of possible values of variables*)
If[Length[indepVariables]>0, 
	(*if more than one sample but fewer than 4 parameters*)
	paramPlot = True;
	selectedVariables = indepVariables;
		(*selectedVariables will be the values that we want to plot*)
	labels =  ("Var "<>ToString@#<>"  ")&/@Range[Length[indepVariables]]; (*if more than one sample*) 	(*labels is the list of varied parameters*)
	numParams = Length[indepVariables];
	, 
	paramPlot = False;
	selectedFiles = {};
	labels = {};
	numParams = 0;
];
plotTitle = "";
	(*start with a blank plot title*)
	goPlot = True;
Manipulate[
	If[paramPlot, (*if there are few enough parameters in the large set*)
		files2plot = filterFiles[parsedFiles, selectedVariables]; (*association that includes only the files we want to plot*)
		If[Length[files2plot]>0, 
				selectedFiles = Values[files2plot]; (*file names*)
				{numParams, style} = getStyles[Keys[files2plot]]; (*plot styles for the files we want to plot*)
				If[NumberQ[style], goPlot==False; error="Too many variables";, goPlot = True];
				, 
				(*if there are no files selected*)
				selectedFiles = {};
		];
		, 
		If[indepVariables==0 , 
			(*only one plot*)style = Black; ,
			(*too many parameters*) style= Automatic
		];
		numParams = 0;
	];
	If[goPlot, 
		(*if style throws an error, display an error, otherwise go ahead and plot*)
		If[numParams>1, (*if there are more than 1 parameters*)
				markers = style[[;;,2]]; (*set markers to designated markers*)
				If[Length[style[[1]]]>2, (*if there are 3 parameters*)
					style = style[[;;, {1,3}]] (*set color and line style*)
				]
			, 
			If[paramPlot, markers = ConstantArray[\[FilledCircle], Length[style]];, markers = Automatic;](*if there is only one parameter, set markers to circle*)
		];
		If[Length[selectedFiles]>0, 
			disp = plotFormat[getData[#, xvar, yvar]&/@selectedFiles, 
				FrameLabel->{depVariables[[xvar]], depVariables[[yvar]]}, 
				Joined->True, 
				ImageSize->{Automatic, imageSize}, 
				PlotStyle->style, 
				PlotMarkers->({#, markerSize}&/@markers), 
				LabelStyle->Directive[fontSize, Black],
				PlotLabel->plotTitle,
				Ticks->{4,4},
				PlotLegends->If[numParams>0, 
									(Style[makeLabel[labels, #], fontSize, Black]&/@Keys[files2plot])
									, 
									If[numParams==0 && indepVariables==1,
											FileBaseName/@selectedFiles (*if list of files*)
											, None (*if only one file*)]]]; (*this is the plot*)
			,
			disp = Panel["No files selected", ImageSize->{imageSize, imageSize}]];
		Row[{
			Column[
				Prepend[
					If[paramPlot,
						Join[ 
							Row[{
								InputField[Dynamic[labels[[#]]], String, ImageSize->100]
								, 
								TogglerBar[Dynamic[selectedVariables[[#]]], indepVariables[[#]]]
							}]&/@Range[Length[indepVariables]] (*these are the parameters and values to include*)
						, 
						FileBaseName/@selectedFiles (*this a list of files that we're plotting*)
						]
						,
						{TogglerBar[Dynamic[selectedFiles], (#->FileBaseName[#])&/@parsedFiles, Appearance->"Vertical"->{25, Automatic}]}
						]
						, Row[{"Plot title ", InputField[Dynamic[plotTitle], String, ImageSize->100]}] (*input box to name plot*)
				]
			]
			,"\t"
			, Column[{ disp, If[Length[selectedFiles]>0, Button["Export", Export[SystemDialogInput["FileSave"], disp, "PDF"], Method->"Queued"],""]}] (*save button*)
		}]
	,
	Panel[error, ImageSize->{imageSize, imageSize}]]
	, {{xvar, 1, "x variable"}, (#->depVariables[[#]])&/@Range[Length[depVariables]]} (*variable on x axis*)
	, {{yvar, 6, "y variable"}, (#->depVariables[[#]])&/@Range[Length[depVariables]]} (*variable on y axis*)
	, {{imageSize, 300, "Image size"}, 150, 1000,10} (*height of image*)
	, {{fontSize, 20, "Font size"}, 8, 30,1} (*font size*)
	, {{markerSize, 12, "Marker size"}, 8, 30,1} (*marker size*)
	, {{plotFormat, ListLogLogPlot, "Plot format"}, {ListLogLogPlot, ListLogPlot, ListPlot}} (*which axes should be log*)
	, ContinuousAction->False (*don't update the plot until you release the drag bars*)
	]]


(*rheologyPlots[folder]*)
