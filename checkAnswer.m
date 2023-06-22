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

	checkAnswer[userAnswear_,correctAnswear_,mode_]:=
		Module[{uAns,cAns,guessed},(*creazione del modulo*)
		
			(*se la modalit\[AGrave] precede le frazioni,le semplifico a priori*)
			If[mode==1,
				uAns=Simplify[userAnswear]; (*semplifica l'espressione indicata nella risposta dell'utente*)
				cAns=Simplify[correctAnswear]; (*semplifica l'espressione indicata nella risposta corretta. Verranno successivamente confrontate*)
			];
			
			guessed="Sbagliato!";
			
			(*qui vengono fatti i controlli per capire se la risposta fornita dall'utente \[EGrave] esatta*)
			Switch[mode,
				1,If[userAnswear==correctAnswear,guessed="Giusto!"],
				2,If[userAnswear==correctAnswear,guessed="Giusto!"],
				3,If[userAnswear<=correctAnswear+4&&userAnswear>=correctAnswear-4,guessed="Giusto!"],
				4,If[userAnswear<=correctAnswear+3&&userAnswear>=correctAnswear-3,guessed="Giusto!"],
				5,If[userAnswear<=correctAnswear+2&&userAnswear>=correctAnswear-2,guessed="Giusto!"],
				6,If[userAnswear<=correctAnswear+1&&userAnswear>=correctAnswear-1,guessed="Giusto!"],
				_,"Errore, modalit\[AGrave] non esistente"];
				
			Return[guessed]; (*Ritorniamo guessed che \[EGrave] una stringa che indica se giusto o sbagliato*)
		];

End[]
EndPackage[]
