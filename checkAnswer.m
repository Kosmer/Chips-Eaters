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
			
			(* Controllo se la risposta fornita dall'utente \[EGrave] esatta*)
			If[userAnswear==correctAnswear,guessed="Giusto!"];
				
			Return[guessed]; (*Ritorniamo guessed che \[EGrave] una stringa che indica se giusto o sbagliato*)
		];

End[]
EndPackage[]
