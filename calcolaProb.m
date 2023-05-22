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
			{probabilita, probabilitastr, carteBancoscoperte, carteScoperteTotali, cartascelta, cartaeffettiva, carteRimanenti, contacoppie, carta1player,   carta2player, indicecartascelta,carteBancocoperte,contacoppietotali,carteScoperteplayer,  i, spiegazione, contacoppiemie},
		
			probabilita=0;
			probabilitastr = "0";
			contacoppie=0;
			contacoppiemie=0;
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
			probabilita = 3/carteRimanenti;
			probabilitastr = ToString[3]<>"/"<>ToString[carteRimanenti];
			spiegazione = "
In questo caso, la probabilit\[AGrave] \[EGrave] uguale a "<> probabilitastr <>". Tra mano e banco c'\[EGrave] solo una carta con il numero "<>ToString[cartaeffettiva]<> ".
Quindi la probabilit\[AGrave] che ne esca un'altra \[EGrave] calcolata come i casi favorevoli su i casi totali.
La probabilit\[AGrave] finale \[EGrave] "<>probabilitastr<>".";
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			If[contacoppie>1,probabilita = 1; probabilitastr = "1";
			spiegazione = "
In questo caso, la probabilit\[AGrave] \[EGrave] uguale a "<>probabilitastr<>". Tra mano e banco c'\[EGrave] gi\[AGrave] una coppia di carte con il numero "<>ToString[cartaeffettiva]<> ".
Quindi la probabilit\[AGrave] \[EGrave] semplicemente 1.
La probabilit\[AGrave] finale \[EGrave] "<>probabilitastr<>".";
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
			If[contacoppie == 2, probabilita = 2/carteRimanenti; probabilitastr = ToString[2]<>"/"<>ToString[carteRimanenti];
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>probabilitastr<>", cio\[EGrave] 2 sul numero di carte rimanenti.
La probabilit\[AGrave] finale \[EGrave] "<>probabilitastr<>".";
			];
			,
			2,
			If[contacoppie == 2, probabilita =  2*((carteRimanenti-2) /carteRimanenti) * (2/(carteRimanenti-1)) + (2/(carteRimanenti*(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>ToString[probabilita]<>", cio\[EGrave] \[EGrave] la probabilit\[AGrave] che una carta con quel numero esca in una delle due carte ancora coperte.
Per calcolarla sommiamo la probabilit\[AGrave] che una carta con lo stesso numero della carta scelta esca con la prima carta coperta (P1) alla probabilit\[AGrave] che esca alla seconda (P2).
P1 e P2 sono uguali. P1 \[EGrave] uguale a "<>ToString[carteRimanenti]<>"(carte rimanenti) - 2 / "<>ToString[carteRimanenti]<>" * 2 /"<>ToString[carteRimanenti]<>" - 1
A questo dobbiamo sommare il caso in cui il giocatore faccia Poker, quindi 2 / "<>ToString[carteRimanenti]<>" * "<>ToString[carteRimanenti]<>" - 1";
			];
			If[contacoppie == 1, probabilita = ((3/carteRimanenti) *  (2/(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco non c'\[EGrave] nessuna coppia con la carta scelta, quindi per fare tris Devono uscire carte del numero della carta scelta entrambe le volte.
La probabilit\[AGrave] \[EGrave] quindi 3/"<>ToString[carteRimanenti]<>"* 2/"<>ToString[carteRimanenti-1]<>". 3 e 2 sono il numero di casi favorevoli.
2 dato che dopo che esce il primo il numero di casi favorevoli diminuisce di uno.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			];
			,
			3,(*3 carte coperte sul banco, 1 coppia gia sul banco;*)
			If[contacoppie == 2, probabilita = 3*(2*(carteRimanenti-3)/(carteRimanenti*(carteRimanenti-1)))+ 3*(2/(carteRimanenti*(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e \[EGrave] gia presente sul banco una coppia con la carta scelta.
Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che un "<>ToString[cartaeffettiva]<>" esca in una tra le tre carte coperte del banco.
Si tratta di sommare le 3 possibilit\[AGrave]: nel caso in cui esca nella prima, nella seconda e nella terza. Le tre probabilit\[AGrave] sono uguali
La probabilit\[AGrave] \[EGrave] "<>ToString[carteRimanenti-3]<>"/"<>ToString[carteRimanenti]<>"*"<>ToString[carteRimanenti-2]<>"/"<>ToString[carteRimanenti-1]<>"*2/"<>ToString[carteRimanenti-2]<>".
La formula si puo' semplificare togliendo carte rimanenti -2.
A questo va sommata la probabilit\[AGrave] di fare Poker che \[EGrave] la probabilit\[AGrave] che escano due carte su tre con il valore della carta scelta.
Questa probabilit\[AGrave] \[EGrave] 3* (le combinazioni) 2 / (carte rimanenti * carte rimanenti -1).
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			];
			If[contacoppie == 1, probabilita =  3*(3* 2* (carteRimanenti -3)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+ (3*2/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)));
			spiegazione = "Spiegazione della risposta.
In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e non \[EGrave] presente alcuna coppia sul banco con la carta scelta.
Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che per due volte "<>ToString[cartaeffettiva]<>" venga estratta tra le tre carte coperte del banco.
Per calcolare la probabilit\[AGrave] finale dobbiamo calcolare qual \[EGrave] la probabilit\[AGrave] che escano due volte la carta che vogliamo su tre e moltiplicare questa probabilit\[AGrave] per 3.
Questo viene fatto perch\[EGrave] dobbiamo permutare le possibilit\[AGrave] (P1). A questo aggiungiamo la probabilit\[AGrave] che esca 3 volte la stessa carta (P2).
P1 \[EGrave] 3 * 2 * carte rimanenti -3 / i casi totali, che sono sempre carte rimanenti  e poi cr -1 e -2. 
P2 \[EGrave] 3 * 2 * 1 / i casi totali.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			];
			,
			_,
			(*Print["ci sono piu di 3 carte da estrarre"]; *)probabilita =0;
			];
			
			
			(*controllo finale nel caso non sia possibile fare un tris o sia gia sul banco*)
			If[contacoppie  >3,probabilita = 1;
			spiegazione = "Spiegazione della risposta.
La probabilit\[AGrave] \[EGrave] 1. C'\[EGrave] gia un tris con la carta scelta sul banco.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
			If[contacoppie + carteBancocoperte<3,probabilita = 0; (*Print["coppie + carte min 2"];*)
			spiegazione = "Spiegazione della risposta.
La probabilit\[AGrave] \[EGrave] 0. Non \[EGrave] possibilie fare un tris dato che non abbiamo abbastanza carte coperte.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
			,
			
			(*4 calcola probabilit\[AGrave] di coppia di una carta random scoperta con le rimanenti da estrarre,gli altri giocatori hanno carte scoperte*)
			4,
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
			For[i=1,i<=Length[carteBancoscoperte + 2],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppiemie++;]];
			For[i=1,i<=Length[carteScoperteTotali],i++,
			If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];
			carteRimanenti=52-Length[carteScoperteTotali];
			Switch[carteBancocoperte,
			1,
			probabilita=(3-(contacoppie - contacoppiemie))/carteRimanenti;
			spiegazione = "Spiegazione della risposta.
In questo caso, dato che c'\[EGrave] solo una carta coperta sul banco il calcolo della probabilit\[AGrave] non \[EGrave] complesso.
Si tratta di contare il numero di casi favorevoli su casi totali, l'unica accortezza \[EGrave] nel fatto che devo controllare che i miei avversari non abbiano carte con lo stesso numero della carta scelta.
In quel caso devo sottrarle a 3 che sarebbe il valore di casi favorevoli iniziale. In pi\[UGrave] devo stare attento al numero di casi totali che sono 52 - tutte le carte scoperte sul campo di gioco.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			,
			2,
			probabilita=2*((carteRimanenti-(3-(contacoppie - contacoppiemie)))*(3-(contacoppie - contacoppiemie)))/(carteRimanenti*(carteRimanenti-1))+ (((3-(contacoppie - contacoppiemie))*(2-(contacoppie - contacoppiemie)))/(carteRimanenti*(carteRimanenti-1)));
			spiegazione = "Spiegazione della risposta.
In questo caso il numero di carte coperte sul banco \[EGrave] 2, pi\[UGrave] il numero di carte coperte aumenta pi\[UGrave] il calcolo diventa complesso.
Si tratta di calcolare la probabilit\[AGrave] che la carta scelta esca in una delle due carte coperte rimanenti.
Possiamo dividere il calcolo della probabilit\[AGrave] in due parte. Che esca o nella prima corta o nella seconda e il caso in cui esca entrambe le volte.
Per i casi favorevoli bisogna controllare che gli altri giocatori non abbiano una carta dello stesso numero della carta scelta, in quel caso il numero di casi favorevoli scende.
La probabilit\[AGrave] \[EGrave] quindi binomiale di 2 (dato che le carte coperte sono 2) * casi favorevoli per casi non favorevoli diviso carte rimanenti per carte rimanenti -1. 
A questo va sommata la probabilit\[AGrave] che esca la carta scelta entrambe le volte che \[EGrave] carte favorevoli per carte favorevoli -1 diviso diviso carte totali * carte cotali -1.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			,
			(*troppo complicato da calcolare a mente, non si prevede un caso in cui ci sono 4 carte da estrarre*)
			_,Print["ci sono piu di 2 carte da estrarre"]; probabilita =0;
			spiegazione = "Spiegazione della risposta.Controllare errore.";];
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			 If[contacoppiemie>1,probabilita = 1;
			 spiegazione = "Spiegazione della risposta.La probabilit\[AGrave] \[EGrave] 1, c'\[EGrave] gia presente una coppia sul banco.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
			 If[contacoppiemie - contacoppie >2,probabilita = 0;
			 spiegazione = "Spiegazione della risposta.La probabilit\[AGrave] \[EGrave] 0, non \[EGrave] possibile fare una coppia con la carta scelta.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
			,
			(*modalita con avversari con carte scoperte*)
			(*3 calcola probabilit\[AGrave] di coppia di una carta random scoperta con la prossima carta,gli altri giocatori hanno carte coperte *)
			3,
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
			carteRimanenti=52-Length[carteScoperteTotali];
			Switch[carteBancocoperte,
			1,
			probabilita=3/carteRimanenti;
			spiegazione = "Spiegazione della risposta.
Dato che il numero di carte coperte \[EGrave] 1 questo esercizio \[EGrave] uguale agli esercizi della modalit\[AGrave] 1.
Basta contare il numero di casi favorevoli fratto il numero di casi totali.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			,
			2,
			probabilita=2*(3*(carteRimanenti-3)/(carteRimanenti * (carteRimanenti -1)))+ (3*2/(carteRimanenti * (carteRimanenti -1)));
			spiegazione = "Spiegazione della risposta.
Il numero di carte coperte \[EGrave] 2. Quindi il calcolo della probabilit\[AGrave] consiste nel calcolare la possibilit\[AGrave] che la carta scelta esca o nella prima o nella seconda carta coperta che rimane.
La probabilit\[AGrave] che esca la carta scelta nella prima carta \[EGrave] 3 / carte rimanenti * la probabilit\[AGrave] che non esca nella seconda, carte rimanenti - 3 / carte rimanenti - 1.
La probabilit\[AGrave] che esca la carta scelta nella seconda carta \[EGrave] carte rimanenti - 3 / carte rimanenti * 3 / carterimanenti -1.
Come si puo' notare le due probabilit\[AGrave] sono uguali.
A questo va sommata la probabilit\[AGrave] che il numero della carta scelta esca sia nella prima carta coperta che nella seconda; che \[EGrave] 3 * 2 / carte rimanenti * carte rimanenti -1.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			,
			3,
			probabilita= 3*(((carteRimanenti-3)*(carteRimanenti-4)*3)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+3*(((carteRimanenti-3)*3*2)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+((3*2)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)));
			spiegazione = "Spiegazione della risposta.
Il numero di carte coperte \[EGrave] 2. Quindi il calcolo della probabilit\[AGrave] consiste nel calcolare la possibilit\[AGrave] che la carta scelta esca o nella prima o nella seconda o nella terza carta coperta che rimane.
La probabilit\[AGrave] che esca la carta scelta una sola volta \[EGrave] 3* la probabilit\[AGrave] che esca in ciascuno spot dei 3 coperti, cio\[EGrave]: 3 / carte rimanenti *  carte rimanenti - 3 / carte rimanenti - 1 *  carte rimanenti - 4 / carte rimanenti - 2.
A questo va sommata la probabilit\[AGrave] che escano due carte con lo stesso valore della carta scelta, la probabilit\[AGrave] \[EGrave] 3 * carterimanenti -3 * 3 * 2 / (carte rimanenti * carte rimanenti -1 * carte rimanenti -2). 
Per finire va sommata la probabilit\[AGrave] che il numero della carta scelta esca 3 volte; che \[EGrave] 3 * 2 / carte rimanenti * carte rimanenti -1 * carte rimanenti -2.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";
			,
			
			_,Print["ci sono piu di 2 carte da estrarre"]; probabilita =0;
			spiegazione = "Spiegazione della risposta.Controllare errore.";];
			(*ora calcoliamo la probabilita che esca una coppia,nel caso ci sia gia l'utente se ne deve accorgere e deve dire 1*)
			If[contacoppie>1,probabilita = 1;
			spiegazione = "Spiegazione della risposta.C'\[EGrave] gia una coppia presente sul banco con la carta scelta.
La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
			,
			_,
			Print["modalita ancora da implementare"];probabilita =0;
			];
			Return[{probabilita, cartascelta, spiegazione}];
	]
End[]
EndPackage[]

