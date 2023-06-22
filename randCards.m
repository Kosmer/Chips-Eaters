(* ::Package:: *)

(* :Title: randomCards *)
(* :Context: randomCards` *)
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

BeginPackage["randCards`"];

randCards::usage = "randomCards[modalita, nPlayers, nSeed], dove modalita indica la modalit\[AGrave] del gioco in corso, nPlayers indica il numero di giocatori che giocano, nSeed indica il seme 
del generatore casuale delle carte del gioco. La funzione ritorna due liste di carte ed il nuovo seme del generatore, la prima lista contiene le carte del banco con un certo numero di
 carte scoperte, mentre la seconda contiene le carte di ciascuna mano di ciascun giocatore."
 
Begin["`Private`"]
	
	randomCards[modalita_Integer, nPlayers_Integer, nSeed_Integer]:=
		Module[
			{banco,mani,p,b,ArrayCheck,nPlayer,nDiscovered, n, seed, nSeed2, nDiscoveredCards},ArrayCheck={};
			
			(*assegno il seed alla nostra variabile locale. Se abbiamo indicato un seed, prender\[AGrave] quello, altrimenti sar\[AGrave] 0*)
			nSeed2 = nSeed;
			
			(*Se il seed \[EGrave] zero, crea un nuovo seed random per la generazione del nuovo esercizio, altrimenti prende il seed da noi specificato*)
			If[nSeed2==0, nSeed2 = RandomInteger[{1,500}]; seed = SeedRandom[nSeed2], seed = SeedRandom[nSeed2]]; 
			
			(*in base alla modalit\[AGrave] sceglie in modo random quante carte scoperte ci sono sul banco*)
			Switch[modalita,  
			1,nDiscoveredCards=RandomInteger[{2,4}],
			2,nDiscoveredCards=3,
			3,nDiscoveredCards=RandomInteger[{2,3}],
			4,nDiscoveredCards=3,
			5,nDiscoveredCards = 4,
			_, "Errore"];
			
			banco=Table[0,{5}];
			mani=Table[0,{nPlayers*2}];
			nDiscovered=1;
			nPlayer=1;
			
			(*Crea le carte scoperte a seconda di nDiscoveredCards (ovvero il numero di carte scoperte) *)
			While[nDiscovered-1<nDiscoveredCards,banco[[nDiscovered]]=RandomInteger[{1,52}];
			While[MemberQ[ArrayCheck,banco[[nDiscovered]]],banco[[nDiscovered]]=RandomInteger[{1,52}]];
				ArrayCheck=Append[ArrayCheck,banco[[nDiscovered]]];
				nDiscovered+=1;
			];
			
			(*Crea le mani dei giocatori a seconda di nPlayers (ovvero il numero di giocatori) *)
			While[nPlayer-1<nPlayers*2,mani[[nPlayer]]=RandomInteger[{1,52}];
			While[MemberQ[ArrayCheck,mani[[nPlayer]]],mani[[nPlayer]]=RandomInteger[{1,52}]];
				ArrayCheck=Append[ArrayCheck,mani[[nPlayer]]];nPlayer+=1;
				mani[[nPlayer]]=RandomInteger[{1,52}
			];
			While[MemberQ[ArrayCheck,mani[[nPlayer]]],mani[[nPlayer]]=RandomInteger[{1,52}]];
				ArrayCheck=Append[ArrayCheck,mani[[nPlayer]]];nPlayer+=1;
			];
			{banco,mani, nSeed2}
		]

End[]
EndPackage[]
