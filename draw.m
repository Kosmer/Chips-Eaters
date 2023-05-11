(* ::Package:: *)

(* :Title: calcolaProb *)
(* :Context: calcolaProb` *)
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

BeginPackage["draw`"];

drawAll::usage = "Disegnamento disegnante"

Begin["`Private`"]

drawAll[modalita_]:=Module[{},
SetDirectory[NotebookDirectory[]];
Get["randCards.m"];
Get["calcolaProb.m"];

Switch[modalita,
1,player=1;carteScoperte=RandomInteger[{2,4}],
2,player=1;carteScoperte=3,
3,player=1;carteScoperte=3,
4,player=2;carteScoperte=3,
5,player = 3; carteScoperte = 4,
_, "Errore"];

(*CREO LE CARTE DEL BANCO E DEL GIOCATORE*)
{carteBanco,carteGiocatore}=randCards`randomCards[carteScoperte,player];

(*CREO IL DISEGNO DELLE CARTE AVVERSARIO*)
Switch[player,
2,
outputavversari=Grid[{{Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[3]],carteGiocatore[[4]]},"CardSpreadAngle"->0.4]}},Spacings->{Scaled[0.1],Scaled[0.1]},Alignment->Center],
3,
outputavversari=Grid[{{Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[3]],carteGiocatore[[4]]},"CardSpreadAngle"->0.4],Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[5]],carteGiocatore[[6]]},"CardSpreadAngle"->0.4]}},Spacings->{Scaled[0.1],Scaled[0.1]},Alignment->Center]
,
_,
"Errore"];

(*CREO IL DISEGNO DELLE CARTE BANCO*)
outputbanco=Row[{ResourceFunction["PlayingCardGraphic"][carteBanco[[1]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[2]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[3]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[4]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[5]]]},Spacer[10]];
outputbanco;

(*CREO IL DISEGNO DELLE CARTE GIOCATORE*)
outputgiocatore=Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[1]],carteGiocatore[[2]]}];
outputgiocatore;

(*RICAVO LA PROBABILITA' CORRETTA E LA CARTA RICHIESTA*)
{correctprob, requestcard} = calcolaProb`calcolaProb[carteBanco, carteGiocatore,player, modalita];

effectivecard = ToString[Mod[requestcard,13]];
Switch[effectivecard,
"0", effectivecard = "K",
"1", effectivecard = "A",
"11", effectivecard = "J",
"12", effectivecard = "Q",
_, "Errore"];

Return[{outputbanco,outputgiocatore, outputavversari, effectivecard, correctprob}];
]

End[]
EndPackage[]
