(* ::Package:: *)

(* :Title: checkAnswer *)
(* :Context: checkAnswer` *)
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

BeginPackage["checkAnswer`"];

checkAnswer::usage = "checkAnswer[userAnswer,correctAnswer,mode], dove userAnswer indica la risporta fornita dall'utente,
correctAnswer indica la risposta corretta e mode indica la modalit\[AGrave] di gioco in corso (che ne definisce anche i criteri di valutazione). 
La funzione ritorna la stringa \"Giusto!\" o \"Sbagliato!\" se la risposta fornita dall'utente \[EGrave] uguale alla risposta corretta."

Begin["`Private`"]

	checkAnswer[userAnswer_,correctAnswer_,mode_]:=
		Module[{uAns,cAns,guessed},
		
			(*se la modalit\[AGrave] precede le frazioni,le semplifico a priori*)
			If[mode==1,
				uAns=Simplify[userAnswer];
				cAns=Simplify[correctAnswer];
			];
			
			guessed="Sbagliato!";
			
			Switch[mode,
				1,If[userAnswer==correctAnswer,guessed="Giusto!"],
				2,If[userAnswer==correctAnswer,guessed="Giusto!"],
				3,If[userAnswer<=correctAnswer+4&&userAnswer>=correctAnswer-4,guessed="Giusto!"],
				4,If[userAnswer<=correctAnswer+3&&userAnswer>=correctAnswer-3,guessed="Giusto!"],
				5,If[userAnswer<=correctAnswer+2&&userAnswer>=correctAnswer-2,guessed="Giusto!"],
				6,If[userAnswer<=correctAnswer+1&&userAnswer>=correctAnswer-1,guessed="Giusto!"],
				_,"Errore, modalit\[AGrave] non esistente"];
				
			Return[guessed];
		];

End[]
EndPackage[]
