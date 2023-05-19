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

randCards::usage = "randomCards[x, y] ritorna due liste di carte, la prima contiene le carte del banco con x carte scoperte, mentre la seconda contiene 2*y carte che rappresentano le mani da due carte in base al numero di giocatori."

Begin["`Private`"]

	randomCards[modalita_Integer, nPlayers_Integer, nSeed_Integer]:=
		Module[
			{banco,mani,p,b,ArrayCheck,nPlayer,nDiscovered, n, seed, nSeed2, nDiscoveredCards},ArrayCheck={};
			
			nSeed2 = nSeed;
			If[nSeed2==0, nSeed2 = RandomInteger[{1,500}]; seed = SeedRandom[nSeed2], seed = SeedRandom[nSeed2]];
			
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
			(*Crea le carte scoperte a seconda di nDiscoveredCards*)While[nDiscovered-1<nDiscoveredCards,banco[[nDiscovered]]=RandomInteger[{1,52}];
			While[MemberQ[ArrayCheck,banco[[nDiscovered]]],banco[[nDiscovered]]=RandomInteger[{1,52}]];
				ArrayCheck=Append[ArrayCheck,banco[[nDiscovered]]];
				nDiscovered+=1;
			];
			(*Crea le mani dei giocatori a seconda di nPlayers*)While[nPlayer-1<nPlayers*2,mani[[nPlayer]]=RandomInteger[{1,52}];
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
