(* ::Package:: *)

BeginPackage["Anim`"];


setProjectDirectory::usage = "setProjectDirectory[dir] sets the project directory to dir.";


setProjectResolution::usage = "setProjectResolution[{w , h}] sets the project resolution to w x h.";


setProjectBackground::usage = "setProjectBackground[color] sets the project background to color.";


setProjectFrameRate::usage = "setProjectFrameRate[f] sets the project framerate to f frames per second.";


animFunction::usage = "animFunction[f] returns an function for animating, f is a function that takes
a real argument 0 < t < 1, f[t] returns Graphics or Graphics3d";


after::usage = "after[len_][f1 , f2] returns a new function for animating with 
animating function f1 after animating functiton f2. The length of animation for f1 is
assumed to be len - by default 1.";


map::usage = "TODO";


mapTimed::usage = "TODO";


exportScene::usage = "TODO";


joinScenes::usage = "TODO";


aF::usage = "TODO";


getFunction::usage = "TODO";


status::usage = "TODO";


show::usage = "TODO";


Begin["`Private`"];


parameters = 
	Association[
		"projectResolution"->{1920 , 1080},
		"projectBackground" -> White ,
		"projectFrameRate" -> 24
	];


status = "";


animFunction[f_]:=aF[f , 1.0];


after[len_:1][aF[f2_ , d2_] , aF[f1_ , d1_]]:=aF[
	Function[t,
		If[
			t < d1/(d1 + d2 len) , 
			f1[t/(d1/(d1 + d2 len))] , 
			f2[(t-(d1/(d1 + d2 len)))/(1 - (d1/(d1 + d2 len)))]]
		] , d1 + d2 len
	];


map[fun_][aF[f_ , d_]]:= aF[Function[t , fun[f[t]]] ,  d];


mapTimed[fun_][aF[f_ , d_]]:= aF[Function[t , fun[t,f[t]]] ,  d];


setProjectDirectory[dir_String]:=
	Module[{},
		If[Not[DirectoryQ[dir]] , CreateDirectory[dir]];
		AssociateTo[parameters , "projectDirectory"->dir];
		parameters
	];


setProjectResolution[{w_Integer , h_Integer}]:=
	Module[{},
		AssociateTo[parameters , "projectResolution"->{w , h}];
		parameters
	];


setProjectBackground[color_]:=
	Module[{},
		AssociateTo[parameters , "projectBackground"->color];
		parameters
	];


setProjectFrameRate[f_Integer]:=
	Module[{},
		AssociateTo[parameters , "projectFrameRate"->f];
		parameters
	];


show[type_ , aF[f_ , d_] , t_]:=showGraphics[type[f[t]]];


showGraphics[g_]:= 
	Switch[
		Head[g],
		Graphics3D,
		Show[
			g , 
				PlotRange->All,
				Boxed->False,
				Lighting->{{"Directional",White,{2,-2,0}}} , 
				ViewMatrix->
					{
						TransformationMatrix[
							RescalingTransform[
								{
									{-1 , 1} , {-1 , 1} , {-1 , 1}
								},
								{
									{0 , 1} , {0 , 1} , {0 , 1}
								}
							]
						], 
						IdentityMatrix[4]
					},
				ImagePadding->Scaled[0.01]
		],
		Graphics,
		Show[g , 
			PlotRange->{{-1.7777777777777777` , 1.7777777777777777`} , {-1 , 1}} , 
			Axes->False , 
			Frame->False , 
			ImagePadding->Scaled[0.0001],
			Background -> parameters["projectBackground"]
		],
		_,
		None
	];
	(*Show[g , PlotRange->{{-1 , 1} , {-1 , 1}} , Axes->False , Frame->False , ImagePadding->Scaled[0.01]]*)


exportGraphics[sceneDir_String][g_ , {frameNumber_Integer}]:=
	Module[{im , dir},
		im = showGraphics[g];
		dir = FileNameJoin[{parameters["projectDirectory"] , sceneDir}];
		If[Not[DirectoryQ[dir]] , CreateDirectory[dir]];
		Export[
			FileNameJoin[{dir , IntegerString[frameNumber , 10 , 10]<>".png"}] , 
			im , 
			RasterSize-> parameters["projectResolution"] ,
			Background -> parameters["projectBackground"]
		]
	];


exportScene[type : Graphics|Graphics3D , name_ , duration_, aF[f_ , d_]]:=
	Module[{frames , dir , nme , sceneDir , i},
		sceneDir = FileNameJoin[{parameters["projectDirectory"] , name}];
		dir = FileNameJoin[{parameters["projectDirectory"] , name , "%10d.png"}];
		nme = FileNameJoin[{parameters["projectDirectory"] , name<>".mp4"}];
		If[FileExistsQ[nme] , DeleteFile[nme]];
		If[DirectoryQ[sceneDir] , RunProcess[{"rm" , "-r" , sceneDir}]];
		i = 1;
		frames = 
			Do[
					status = "time : "<>ToString[t]<>" total time : "<>ToString[duration];
					exportGraphics[name][type[f[t / duration]] , {i}];
					i = i + 1; 
					, {t , 0 , duration , 1.0 / parameters["projectFrameRate"]}
				];
		RunProcess[{"ffmpeg" , "-r" , ToString[parameters["projectFrameRate"]] , "-i" ,dir , "-pix_fmt" , "yuv420p" , nme}];
		name
	];
	(*MapIndexed[exportGraphics[name] , frames];*)


joinScenes[name_ , {scenes__String}]:=
	Module[{nme},
		nme = FileNameJoin[{parameters["projectDirectory"] , name<>".mp4"}];
		If[FileExistsQ[nme] , DeleteFile[nme]];
		RunProcess[
			Join[
				{"melt"} , 
				Function[s , FileNameJoin[{parameters["projectDirectory"] , s<>".mp4"}]]/@{scenes} , 
				{
					"-consumer" , 
					"avformat:"<>FileNameJoin[{parameters["projectDirectory"] , name<>".mp4"}] , 
					"acodec=libmp3lame" ,  
					"vcodec=libx264"
				}
				]
			];
		name
	]


getFunction[aF[f_ , d_]]:= f;


End[];


EndPackage[];
