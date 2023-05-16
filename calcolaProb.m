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

BeginPackage["calcolaProb`"];
calcolaProb::usage = "calcolaProb[x, y, w, z]"

Begin["`Private`"]

	calcolaProb[carteBanco_,carteGiocatore_,player_Integer,modalita_Integer]:=
		Module[
			{probabilita, probabilitastr, carteBancoscoperte, carteScoperteTotali, cartascelta, cartaeffettiva, carteRimanenti, contacoppie, carta1player,   carta2player, indicecartascelta,carteBancocoperte,contacoppietotali,carteScoperteplayer,  i, spiegazione},
		
			probabilita=0;
			probabilitastr = "0";
			contacoppie=0;
			contacoppietotali =0;
			carteBancoscoperte=Select[carteBanco,#!=0&] ;
			(*Print[carteBancoscoperte]; giusto*)
			carteScoperteTotali=Join[carteBancoscoperte,carteGiocatore];
			carteBancocoperte = 5 - Length[carteBancoscoperte];
			(*Print[carteScoperteTotali]; giusto*)
			carta1player=carteGiocatore[1];
			carta2player=carteGiocatore[2];
			
			Switch[modalita,
			(*1 calcola probabilit\[AGrave] di coppia di una carta random scoperta con la prossima carta,gli altri giocatori hanno carte coperte*)
			1,
			indicecartascelta=Random[Integer,{1,Length[carteScoperteTotali]}];
			(*Print[indicecartascelta]; giusto *)
			(*da qui in poi cartascelta non ha piu il valore da 1 a cartescoperte ma ha il valore della carta da 1 a 52*)
			cartascelta=carteScoperteTotali[[indicecartascelta]];
			cartaeffettiva = Mod[cartascelta,13];
			cartaeffettiva = ToString[cartaeffettiva];
			Switch[cartaeffettiva,
			"0", cartaeffettiva = "K",
			"1", cartaeffettiva = "A",
			"11", cartaeffettiva = "J",
			"12", cartaeffettiva = "Q",
			_, "Errore"];
						
												
			(*Print["Inserisci la probabilit\[AGrave] che si crei una coppia con: ",cartascelta, " estraendo la prossima carta dal mazzo" ]; *)
			(*verificare se ci sono gia coppie tra banco e giocatore*)
			For[i=1,i<=Length[carteScoperteTotali],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];
			
			carteRimanenti=52-Length[carteScoperteTotali];
			
			
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			If[contacoppie>1,probabilita = 1; probabilitastr = "1";
			spiegazione = "
In questo caso, la probabilit\[AGrave] \[EGrave] uguale a "<>ToString[probabilitastr]<>". Tra mano e banco c'\[EGrave] gi\[AGrave] una coppia di carte con il numero "<>ToString[cartaeffettiva]<> ".
Quindi la probabilit\[AGrave] \[EGrave] semplicemente 1.", probabilita = 3/carteRimanenti; probabilitastr="3/"<>ToString[carteRimanenti];
			spiegazione = "
In questo caso, la probabilit\[AGrave] \[EGrave] uguale a "<>ToString[probabilitastr]<>". Tra mano e banco c'\[EGrave] solo una carta con il numero "<>ToString[cartaeffettiva]<> ".
Quindi la probabilit\[AGrave] che ne esca un'altra \[EGrave] calcolata come i casi favorevoli su i casi totali.";
			];
			,
			
			(*2 calcola probabilit\[AGrave] di tris di una carta random scoperta con le rimanenti da estrarre,gli altri giocatori hanno carte coperte*)
			2,
			indicecartascelta=Random[Integer,{1,Length[carteScoperteTotali]}];
			(*da qui in poi cartascelta non ha piu il valore da 1 a cartescoperte ma ha il valore della carta da 1 a 52*)
			cartascelta=carteScoperteTotali[[indicecartascelta]];
			(*Print["Inserisci la probabilit\[AGrave] che si crei un tris con: ",cartascelta, " estraendo le carte rimanenti, per arrivare a 5, dal mazzo" ];*)
			cartaeffettiva = Mod[cartascelta,13];
			cartaeffettiva = ToString[cartaeffettiva];
			Switch[cartaeffettiva,
			"0", cartaeffettiva = "K",
			"1", cartaeffettiva = "A",
			"11", cartaeffettiva = "J",
			"12", cartaeffettiva = "Q",
			_, "Errore"];
			For[i=1,i<=Length[carteScoperteTotali],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];
			
			carteRimanenti=52-Length[carteScoperteTotali];
			Switch[carteBancocoperte,
			1,
			If[contacoppie == 2, probabilita = 2/carteRimanenti;
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>ToString[probabilita]<>", cio\[EGrave] 2 sul numero di carte rimanenti.";
			];
			,
			2,
			If[contacoppie == 2, probabilita = (2/carteRimanenti) +  (((carteRimanenti-2) /carteRimanenti) * (2/(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>ToString[probabilita]<>", cio\[EGrave] \[EGrave] la probabilit\[AGrave] che una carta con quel numero esca in una delle due carte ancora coperte.
Per calcolarla sommiamo la probabilit\[AGrave] che una carta con lo stesso numero della carta scelta esca con la prima carta coperta (P1) alla probabilit\[AGrave] che esca alla seconda (P2).
P1 \[EGrave] uguale a 2 (casi favorevoli) / "<>ToString[carteRimanenti]<>" carte rimanenti. P2 \[EGrave] uguale a "<>ToString[carteRimanenti]<>"(carte rimanenti) - 2 / "<>ToString[carteRimanenti]<>" * 2 /";
			];
			If[contacoppie == 1, probabilita = ((3/carteRimanenti) *  (2/(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco non c'\[EGrave] nessuna coppia con la carta scelta, quindi per fare tris Devono uscire carte del numero della carta scelta entrambe le volte.
La probabilit\[AGrave] \[EGrave] quindi 3/"<>ToString[carteRimanenti]<>"* 2/"<>ToString[carteRimanenti-1]<>". 3 e 2 sono il numero di casi favorevoli.
2 dato che dopo che esce il primo il numero di casi favorevoli diminuisce di uno. ";
			];
			,
			3,(*3 carte coperte sul banco ];*)
			If[contacoppie == 2, probabilita = ((2/carteRimanenti) +  (((carteRimanenti-2) /carteRimanenti) * (2/(carteRimanenti-1))) + (((carteRimanenti-2)/carteRimanenti)*(carteRimanenti-2)/carteRimanenti-1)*(2/(carteRimanenti-2)));
			spiegazione = "Spiegazione della risposta.
In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e \[EGrave] gia presente sul banco una coppia con la carta scelta.
Il numero di combinazioni possibili di casi favorevoli sono numerosi. Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che un "<>ToString[cartaeffettiva]<>" esca in una tra le tre carte coperte del banco.
Si tratta di sommare le 3 possibilit\[AGrave]: nel caso in cui esca nella prima, nella seconda e nella terza. La prima proabbilit\[AGrave] \[EGrave] 2/"<>ToString[carteRimanenti]<>".
La seconda probabilit\[AGrave] \[EGrave] "<>ToString[carteRimanenti-2]<>"/"<>ToString[carteRimanenti]<>"*2/"<>ToString[carteRimanenti-1]<>".
La terza probabilit\[AGrave] \[EGrave] "<>ToString[carteRimanenti-2]<>"/"<>ToString[carteRimanenti]<>"*"<>ToString[carteRimanenti-2]<>"/"<>ToString[carteRimanenti-1]<>"*2/"<>ToString[carteRimanenti-2]<>".";
			];
			If[contacoppie == 1, probabilita =  ((3/carteRimanenti)*(2/(carteRimanenti-1)*(1/(carteRimanenti-1))) + (3*((3)/carteRimanenti)* (2 /(carteRimanenti-1))* ((carteRimanenti-3) /(carteRimanenti-2))))
			spiegazione = "Spiegazione della risposta.
In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e non \[EGrave] presente alcuna coppia sul banco con la carta scelta.
Il numero di combinazioni possibili di casi favorevoli sono numerosi. Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che per due volta "<>ToString[cartaeffettiva]<>" tra le tre carte coperte del banco.
Per calcolare la probabilit\[AGrave] finale dobbiamo calcolare qual \[EGrave] la probabilit\[AGrave] che escano due volte la carta che vogliamo su tre e moltiplicare questa probabilit\[AGrave] per 3.
Questo viene fatto perch\[EGrave] dobbiamo permutare le possibilit\[AGrave]. A questo aggiungiamo la probabilit\[AGrave] che esca 3 volte la stessa carta.";
			];
			,
			_,
			Print["ci sono piu di 3 carte da estrarre"]; probabilita =0;
			];
			
			
			(*controllo finale nel caso non sia possibile fare un tris o sia gia sul banco*)
			If[contacoppie  >3,probabilita = 1;];
			If[contacoppie + carteBancocoperte<3,probabilita = 0; Print["coppie + carte min 2"]];
			,
			
			(*3 calcola probabilit\[AGrave] di coppia di una carta random scoperta con le rimanenti da estrarre,gli altri giocatori hanno carte coperte*)
			3,
			indicecartascelta=Random[Integer,{1,Length[carteScoperteTotali]}];
			(*da qui in poi cartascelta non ha piu il valore da 1 a cartescoperte ma ha il valore della carta da 1 a 52*)
			cartascelta=carteScoperteTotali[[indicecartascelta]];
			(*Print["Inserisci la probabilit\[AGrave] che si crei una coppia con: ",cartascelta, " estraendo le carte rimanenti, per arrivare a 5, dal mazzo" ];*)
			(*verificare se ci sono gia coppie tra banco e giocatore*)
			cartaeffettiva = Mod[cartascelta,13];
			cartaeffettiva = ToString[cartaeffettiva];
			Switch[cartaeffettiva,
			"0", cartaeffettiva = "K",
			"1", cartaeffettiva = "A",
			"11", cartaeffettiva = "J",
			"12", cartaeffettiva = "Q",
			_, "Errore"];
			For[i=1,i<=Length[carteScoperteTotali],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];
			carteRimanenti=52-Length[carteScoperteTotali];
			Switch[carteBancocoperte,
			1,
			probabilita=3/carteRimanenti;
			,
			2,
			probabilita=3/carteRimanenti+((carteRimanenti-3)*3)/(carteRimanenti*(carteRimanenti-1));
			,
			3,
			probabilita=3/carteRimanenti+((carteRimanenti-3)*3)/(carteRimanenti*(carteRimanenti-1))+(((carteRimanenti-3)^2)*3)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2));
			,
			(*troppo complicato da calcolare a mente, non si prevede un caso in cui ci sono 4 carte da estrarre*)
			_,Print["ci sono piu di 3 carte da estrarre"]; probabilita =0;];
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			 If[contacoppie>1,probabilita = 1;];
			,
			(*modalita con avversari con carte scoperte*)
			(*4 calcola probabilit\[AGrave] di coppia di una carta random scoperta con la prossima carta,gli altri giocatori hanno carte coperte *)
			4,
			carteScoperteplayer = Join[carteBancoscoperte, Take[carteGiocatore, 2]];
			indicecartascelta=Random[Integer,{1,Length[carteScoperteplayer]}];
			(*da qui in poi cartascelta non ha piu il valore da 1 a cartescoperte ma ha il valore della carta da 1 a 52*)
			cartascelta=carteScoperteplayer[[indicecartascelta]];
			(*Print["Inserisci la probabilit\[AGrave] che si crei una coppia con: ",cartascelta, " estraendo la prossima carta dal mazzo" ]; *)
			(*verificare se ci sono gia coppie tra banco e giocatore*)
			cartaeffettiva = Mod[cartascelta,13];
			cartaeffettiva = ToString[cartaeffettiva];
			Switch[cartaeffettiva,
			"0", cartaeffettiva = "K",
			"1", cartaeffettiva = "A",
			"11", cartaeffettiva = "J",
			"12", cartaeffettiva = "Q",
			_, "Errore"];
			For[i=1,i<=Length[carteScoperteplayer],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteplayer[[i]],13],contacoppie++;]];
			For[i=1,i<=Length[carteScoperteTotali],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppietotali++;]];
			diff = contacoppietotali - contacoppie;
			carteRimanenti=52-Length[carteScoperteTotali];
			probabilita=(3-diff)/carteRimanenti;
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			If[contacoppie>1,probabilita = 1;];
			,
			
			_,
			Print["modalita ancora da implementare"];probabilita =0;
			];
			Return[{probabilita, cartascelta, spiegazione}];
	]
End[]
EndPackage[]

