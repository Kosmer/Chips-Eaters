(* ::Package:: *)

(* :Title: main *)
(* :Context: main` *)
(* :Author: GS *)
(* :Summary: an example of good programming style *)
(* :Copyright: GS 2023 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 13 *)
(* :History: last modified 27/3/2023 *)
(* :Keywords: programming style, local variables *)
(* :Sources: biblio *)
(* :Limitations: this is for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: package Context is not defined *)

BeginPackage["main`"];

main::usage = "Funzione principale che ritorna l'intera interfaccia utente interattiva."

Begin["`Private`"]

main[]:=
	Module[{}, 
	SetDirectory[NotebookDirectory[]];
	Get["draw.m"];
	Get["checkAnswer.m"];

	Return[CellPrint@ExpressionCell[draw`drawAll[1],"Output", Background->RGBColor[0.3,0.7,0.4],CellFrame->{{1,1},{1,1}},CellFrameColor->Red]]
];

End[]
EndPackage[]
