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

createAll::usage = "Creazione creante"
drawAll::usage = "Disegno disegnante"


Begin["`Private`"]

createAll[modalita_Integer, seed_Integer]:=Module[{},
SetDirectory[NotebookDirectory[]];
Get["randCards.m"];
Get["calcolaProb.m"];

(*
Switch[modalita,
1,player=1;carteScoperte=RandomInteger[{2,4}],
2,player=1;carteScoperte=3,
3,player=1;carteScoperte=RandomInteger[{2,3}],
4,player=2;carteScoperte=3,
5,player = 3; carteScoperte = 4,
_, "Errore"];
*)

Switch[modalita,
1,player=1,
2,player=1,
3,player=1,
4,player=2,
5,player = 3,
_, "Errore"];


(*CREO LE CARTE DEL BANCO E DEL GIOCATORE*)
{carteBanco,carteGiocatore, nSeed}=randCards`randomCards[modalita, player, seed];

(*CREO IL DISEGNO DELLE CARTE AVVERSARIO*)
Switch[player,
2,
outputavversari=Grid[{{Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[3]],carteGiocatore[[4]]},"CardSpreadAngle"->0.4, Background->RGBColor[0.3,0.7,0.4]]}},Spacings->{Scaled[0.1],Scaled[0.1]},Alignment->Center],
3,
outputavversari=Grid[{{Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[3]],carteGiocatore[[4]]},"CardSpreadAngle"->0.4, Background->RGBColor[0.3,0.7,0.4]],Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[5]],carteGiocatore[[6]]},"CardSpreadAngle"->0.4, Background->RGBColor[0.3,0.7,0.4]]}},Spacings->{Scaled[0.1],Scaled[0.1]},Alignment->Center]
,
_,
"Errore"];

(*CREO IL DISEGNO DELLE CARTE BANCO*)
outputbanco=Row[{ResourceFunction["PlayingCardGraphic"][carteBanco[[1]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[2]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[3]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[4]]],ResourceFunction["PlayingCardGraphic"][carteBanco[[5]]]},Spacer[10]];
outputbanco;

(*CREO IL DISEGNO DELLE CARTE GIOCATORE*)
outputgiocatore=Rasterize@ResourceFunction["PlayingCardGraphic"][{carteGiocatore[[1]],carteGiocatore[[2]]}, Background->RGBColor[0.3,0.7,0.4]];
outputgiocatore;

(*RICAVO LA PROBABILITA' CORRETTA E LA CARTA RICHIESTA*)
{correctprob, requestcard, spiegazione} = calcolaProb`calcolaProb[carteBanco, carteGiocatore,player, modalita];

effectivecard = ToString[Mod[requestcard,13]];
Switch[effectivecard,
"0", effectivecard = "K",
"1", effectivecard = "A",
"11", effectivecard = "J",
"12", effectivecard = "Q",
_, "Errore"];

Return[{outputbanco,outputgiocatore, outputavversari, effectivecard, correctprob, spiegazione, nSeed}];
];


drawAll[modalita_Integer]:= 
	
	DynamicModule[{banco = {}, giocatore={}, avversari={}, cardreq="",rightp= 0,text="",result="", answer=0, richiesta="", esercizio, spiegazione = "", spiegazione2="", scelta="Modalit\[AGrave] 1: Semplice", selezione="Hai selezionato la modalit\[AGrave] 1.", modalita2 = modalita, seed = 0, nSeed2, seedtext=""},
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	_, "Errore"];
	esercizio = Column[{
	
	"Seleziona la modalit\[AGrave]:",PopupMenu[Dynamic[scelta],{"Modalit\[AGrave] 1: Semplice","Modalit\[AGrave] 2: Intermedio","Modalit\[AGrave] 3: Difficile", "Modalit\[AGrave] 4: Wow mbare sei tutto pazzo" }],
	Dynamic[Switch[scelta,
	"Modalit\[AGrave] 1: Semplice",selezione="Hai selezionato la modalit\[AGrave] 1."; modalita2 =1 ; ,
	"Modalit\[AGrave] 2: Intermedio",selezione="Hai selezionato la modalit\[AGrave] 2.";modalita2 =2; ,
	"Modalit\[AGrave] 3: Difficile",selezione="Hai selezionato la modalit\[AGrave] 3.";modalita2=3 ; ,
	"Modalit\[AGrave] 4: Wow mbare sei tutto pazzo" ,selezione="Hai selezionato la modalit\[AGrave] 4."; modalita2 =4; ]
	],
	
	Row[{
	"Seed Esercizio: ", Dynamic[nSeed2], "   ",
	InputField[Dynamic[seedtext],String, ImageSize -> {100, 20}],
	Button["Carica Seed",
	seed = ToExpression[seedtext];
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	result="";
	text = "";
	spiegazione2="";
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	_, "Errore"];
	]
	}],
	If[modalita>3,
	Dynamic[avversari]],
	Dynamic[banco], Dynamic[giocatore]," ", Dynamic[richiesta]," ",
	Row[{
	InputField[Dynamic[text],String, ImageSize -> {100, 20}]," ", Button["Verifica Risultato",answer=ToExpression[text];
	result=checkAnswer`checkAnswer[answer,rightp,modalita];]," "(*,Dynamic@Row[{result}]*)}],
	Dynamic@Row[{result}],
	Button["Pulisci esercizio", text = ""; result="",ImageSize -> {200, 25}],
	Button["Nuovo esercizio",
	seed = 0;
	seedtext = "";
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	result="";
	text = "";
	spiegazione2="";
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	_, "Errore"];,
	ImageSize -> {200, 25}
	],
	Button["Spiegazione",spiegazione2=spiegazione, ImageSize -> {200, 25}],Dynamic[spiegazione2]
	}, ItemSize -> {60, Automatic}, Alignment -> Center];
	Dynamic[esercizio]
	];

End[]
EndPackage[]
