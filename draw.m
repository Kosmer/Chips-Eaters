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

createAll::usage = "createAll[modalita, seed], dove modalita \[EGrave] la modalit\[AGrave] del gioco in corso, seed \[EGrave] il seme del generatore di numeri casuali. La funzione ritorna una lista 
{outputbanco,outputgiocatore, outputavversari, effectivecard, correctprob, spiegazione, nSeed}, dove outputbanco \[EGrave] la lista delle immagini delle carte del banco, outputgiocatore \[EGrave] 
la lista delle immagini delle carte del giocatore (utente), outputavversari \[EGrave] la lista delle immagini delle carte degli avversari, effectivecard \[EGrave] la carta che viene richiesta nella 
domanda, correctprob \[EGrave] la probaili\[AGrave] corretta nonch\[EGrave] soluzione dell'esercizio in corso, spiegazione \[EGrave] una stringa che contiene la soluzione all'esercizio in corso con una spiegazione per 
l'utente, nSeed \[EGrave] il nuovo seme del generatore casuale di numeri."


drawAll::usage = "drawAll[modalita], dove modalita \[EGrave] la modalit\[AGrave] del gioco in corso. La funzione ritorna una struttura che contiene tutti i valori di ritorno creati dalla funzione 
createAll, ovvero ritorna tutta l'interfaccia utente."


Begin["`Private`"]

createAll[modalita_Integer, seed_Integer]:=Module[{player, carteBanco, carteGiocatore, nSeed, outputavversari, outputbanco, outputgiocatore, correctprob, requestcard,spiegazione, effectivecard},
SetDirectory[NotebookDirectory[]];
Get["randCards.m"];
Get["calcolaProb.m"];

(* assegno il numero di player in base alla modalit\[AGrave] indicata *)
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
1, outputavversari=Grid[{}],
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

(*Trasformo il valore della carta in K, A, J o Q nel caso in cui sia capitata una carta dal valore corrispondente*)
Switch[effectivecard,
"0", effectivecard = "K",
"1", effectivecard = "A",
"11", effectivecard = "J",
"12", effectivecard = "Q",
_, "Errore"];

Return[{outputbanco,outputgiocatore, outputavversari, effectivecard, correctprob, spiegazione, nSeed}];
];


drawAll[modalita_Integer]:= 
	
	DynamicModule[{banco = {}, giocatore={}, avversari={}, cardreq="",rightp= 0,text="",result="", answer=0, richiesta="", esercizio, spiegazione = "", spiegazione2="", scelta="Modalit\[AGrave] 1: Semplice", selezione="Hai selezionato la modalit\[AGrave] 1.", modalita2 = modalita, seed = 0, nSeed2, seedtext="", erroretipo=""},
	(* Genero gli elementi utili per l'esercizio, ovvero le varie carte e le corrette probabilit\[AGrave] da ottenere con la relativa spiegazione *)
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	
	(*Genero una domanda iniziale in base alla modalit\[AGrave] scelta *)
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le carte rimanenti dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le carte rimanenti dal mazzo?",
	4, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo considerando anche le carte degli avversari?",
	_, "Errore"];
	
	(*Creo la mia variabile "esercizio" con tutti gli elementi utili associati all'esercizio*)
	esercizio = Column[{
	(*Tendina per la scelta della modalit\[AGrave]*)
	"Seleziona la modalit\[AGrave]:",PopupMenu[Dynamic[scelta],{"Modalit\[AGrave] 1: Semplice","Modalit\[AGrave] 2: Intermedio","Modalit\[AGrave] 3: Difficile", "Modalit\[AGrave] 4: Molto Difficile" }],
	Dynamic[Pane[Switch[scelta,
	"Modalit\[AGrave] 1: Semplice",selezione="Hai selezionato la modalit\[AGrave] 1."; modalita2 =1 ; ,
	"Modalit\[AGrave] 2: Intermedio",selezione="Hai selezionato la modalit\[AGrave] 2.";modalita2 =2; ,
	"Modalit\[AGrave] 3: Difficile",selezione="Hai selezionato la modalit\[AGrave] 3.";modalita2=3 ; ,
	"Modalit\[AGrave] 4: Molto Difficile" ,selezione="Hai selezionato la modalit\[AGrave] 4."; modalita2 =4; ]
	]],
	
	Row[{
	(* Creazione del form per caricare un seed noto, per poter far rifare all'utente esercizi passati*)
	"Seed Esercizio: ", Dynamic[nSeed2], "   ",
	InputField[Dynamic[seedtext],String, ImageSize -> {100, 33}]," ",
	Button["Carica Seed",
	If[IntegerQ[ToExpression[seedtext]]==True,
	seed = ToExpression[seedtext];
	
	(* Inizializzo gli elementi utili per l'esercizio, ovvero le varie carte e le corrette probabilit\[AGrave] da ottenere con la relativa spiegazione*)
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	result="";
	text = "";
	erroretipo="";
	spiegazione2="";
	seedtext="";
	
	(*Creo la domanda in base alla modalit\[AGrave] selezionata *)
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	4, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo considerando anche le carte degli avversari?",
	_, "Errore"];
	, erroretipo="Valore inserito non valido!"]; (*gestione di modalit\[AGrave] con valore non valido*)
	]
	}],
	Dynamic[erroretipo],
	If[modalita<4,
	avversari=""];
	
	(* Visualizzo le carte degli avversari (se ci sono), del banco e del nostro giocatore *)
	Dynamic[avversari],
	Dynamic[banco], Dynamic[giocatore]," ", Dynamic[richiesta]," ",
	
	Row[{
	(* Creo il form per inserire il risultato e il bottone per verificarlo *)
	InputField[Dynamic[text],String, ImageSize -> {100, 33}]," ", Button["Verifica Risultato",answer=ToExpression[text];
	
	(* Richiamo checkAnswer per verificare la correttezza del risultato inserito, e successivamente lo stampo*)
	result=checkAnswer`checkAnswer[answer,rightp,modalita];]," "}],
	Dynamic@Row[{result}],
	
	(* Creo il bottone per pulire l'esercizio *)
	Button["Pulisci esercizio", text = ""; result="",ImageSize -> {200, 25}], 
	
	(* Creo il bottone per generare un nuovo esercizio. Al suo interno setto di nuovo i vari valori utili per la generazione dell'esercizio *) 
	Button["Nuovo esercizio",
	seed = 0;
	seedtext = "";
	{banco,giocatore,avversari, cardreq,rightp, spiegazione, nSeed2}=draw`createAll[modalita2, seed];
	result="";
	text = "";
	spiegazione2="";
	erroretipo="";
	seedtext="";
	Switch[modalita2,
	1,richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo la prossima carta dal mazzo?",
	2, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare un tris di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	3, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo?",
	4, richiesta ="Qual \[EGrave] la probabilit\[AGrave] di fare una coppia di " <>cardreq<> " estraendo le prossime 2 carte dal mazzo considerando anche le carte degli avversari?",
	_, "Errore"];,
	ImageSize -> {200, 25}
	], 
	
	(* Creo il bottone per la spiegazione *)
	Button["Spiegazione",spiegazione2=spiegazione, ImageSize -> {200, 25}],Dynamic[spiegazione2] 
	}, ItemSize -> {60, Automatic}, Alignment -> Center];
	
	(* Infine, stampo l'esercizio *)
	Dynamic[esercizio]
	];

End[]
EndPackage[]
