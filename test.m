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

test::usage = "checkAnswer[x, y, z] ritorna la stringa Giusto! o Sbagliato! se la risposta x fornita dall'utente \[EGrave] uguale alla risposta y corretta, z \[EGrave] la modalit\[AGrave] che cambia il criterio di valutazione della risposta"
BeginPackage["test`"];
Begin["Private`"]

	test[]:=
		Module[{},
		
			
			Print["CCC"];
			Return["ciao"];
		];

End[]
EndPackage[]
