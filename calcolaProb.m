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
calcolaProb::usage = "calcolaProb[carteBanco,carteGiocatore,player,modalita], dove carteBanco \[EGrave] una lista che contiene le carte presenti nel banco, carteGiocatore \[EGrave] una lista che contiene 
le carte di tutti i giocatori, player indica il numero di giocatori, modalita \[EGrave] la modalit\[AGrave] del gioco in corso. La funzione ritorna una lista {probabilita, cartascelta, spiegazione}, dove 
probabilita indica la soluzione corretta dell'esercizio in corso, cartascelta indica la carta oggetto della domanda rivolta all'utente, spiegazione contiene sia la soluzione che una breve 
spiegazione del calcolo corretto per giungere alla soluzione dell'esercizio in corso."

Begin["`Private`"]

	calcolaProb[carteBanco_,carteGiocatore_,player_Integer,modalita_Integer]:=
		Module[
			{probabilita, carteBancoscoperte, carteScoperteTotali, cartascelta, cartaeffettiva, carteRimanenti, contacoppie, indicecartascelta,carteBancocoperte,carteScoperteplayer,  i, spiegazione, contacoppietotali, carteScoperteBG, numeratore, denominatore},
		
			probabilita=0;            (*probabilit\[AGrave] finale*)
			contacoppie=0;            (*contatore coppie tra la carta scelta e le altre carte scoperte (banco e giocatore) *)
			contacoppietotali=0;      (*contatore coppie tra la carta scelta e tutte le altre carte scoperte, incluse quelle degli altri giocatori*)
			
			(* Creo un array che contiene solo le carte scoperte del banco, toglie dall'array carteBanco le carte con valore 0*)
			carteBancoscoperte=Select[carteBanco,#!=0&];  
			
			(* Unisco due array, per creare un array che contiene tutte le carte scoperte*)                 
			carteScoperteBG=Join[carteBancoscoperte,Take[carteBancoscoperte, 2]];      
			carteScoperteTotali=Join[carteBancoscoperte,carteGiocatore];
			
			carteBancocoperte = 5 - Length[carteBancoscoperte];         (* Variabile che contiene il numero di carte coperte*)
			
			(*Switch per dividere le 4 modalit\[AGrave]*)
			Switch[modalita,
			
			(*MODALITA 1: calcola probabilit\[AGrave] di coppia di una carta random scoperta con la prossima carta,gli altri giocatori hanno carte coperte*)
			1,
				indicecartascelta=Random[Integer,{1,Length[carteScoperteTotali]}];   (*scelgo randomicamente la carte su cui verr\[AGrave] fatta la domanda e memorizzo l'indice *)
				cartascelta=carteScoperteTotali[[indicecartascelta]];   (*Assegno la carta scelta in base all'indice*)
				
				(* Calcolo la carta effettiva per poter sostituire la lettera al numero nel caso in cui sia una figura *)
				cartaeffettiva = Mod[cartascelta,13]; 
				cartaeffettiva = ToString[cartaeffettiva]; 
				Switch[cartaeffettiva, 
				"0", cartaeffettiva = "K",
				"1", cartaeffettiva = "A",
				"11", cartaeffettiva = "J",
				"12", cartaeffettiva = "Q",
				_, "Errore"];
																
				(* Verifico se ci sono gia coppie tra banco e giocatore*)
				For[i=1,i<=Length[carteScoperteTotali],i++,
				If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]]; (*se ci sono incremento il contatore contacoppie*)
				
				carteRimanenti=52-Length[carteScoperteTotali];
				probabilita = 3/carteRimanenti;                      (*qui si calcola la probabilit\[AGrave] dell'esercizio della prima modalit\[AGrave]*)
				numeratore = Numerator[probabilita];
				denominatore = Denominator[probabilita];
				
				(*Spiegazione del calcolo della probabilit\[AGrave] dell'esercizio *)
				spiegazione = "  
	Tra mano e banco c'\[EGrave] solo una carta con il numero "<>ToString[cartaeffettiva]<> ".
	Quindi la probabilit\[AGrave] che ne esca un'altra \[EGrave] calcolata come i casi favorevoli su i casi totali.
	La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>"."; 
	
				(*Controllo se c'\[EGrave] gi\[AGrave] una coppia sul tavolo e aggiorno la probabilit\[AGrave] corretta e la spiegazione di conseguenza*)
				If[contacoppie>1,probabilita = 1;
				spiegazione = "
	Tra mano e banco c'\[EGrave] gi\[AGrave] una coppia di carte con il numero "<>ToString[cartaeffettiva]<> ".
	Quindi la probabilit\[AGrave] \[EGrave] semplicemente 1.";
				];
				,
			
			
			(*MODALITA 2: calcola probabilit\[AGrave] di tris di una carta random scoperta con le rimanenti da estrarre,gli altri giocatori hanno carte coperte*)
			2,
				indicecartascelta=Random[Integer,{1,Length[carteScoperteTotali]}];  (*scelgo randomicamente la carte su cui verr\[AGrave] fatta la domanda e memorizzo l'indice *)
				cartascelta=carteScoperteTotali[[indicecartascelta]];                  (*Assegno la carta scelta in base all'indice*)
				
				
				(* Calcolo la carta effettiva per poter sostituire la lettera al numero nel caso in cui sia una figura *)
				cartaeffettiva = Mod[cartascelta,13];
				cartaeffettiva = ToString[cartaeffettiva];
				Switch[cartaeffettiva,
				"0", cartaeffettiva = "K",
				"1", cartaeffettiva = "A",
				"11", cartaeffettiva = "J",
				"12", cartaeffettiva = "Q",
				_, "Errore"];
				
				(* Verifico se ci sono gia coppie tra banco e giocatore*)
				For[i=1,i<=Length[carteScoperteTotali],i++,
				If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];    (*se ci sono incremento il contatore contacoppie*)
				
				carteRimanenti=52-Length[carteScoperteTotali]; 
				
				(* Switch per analizzare le varie casistiche della modalit\[AGrave], in base al numero di carte coperte del banco*)
				Switch[carteBancocoperte,
				1,
					If[contacoppie == 2, probabilita = 2/carteRimanenti;
					numeratore = Numerator[probabilita];
					numeratore = Denominator[probabilita];
					spiegazione = "
		In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]", cio\[EGrave] 2 sul numero di carte rimanenti.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					];
					,
				2,
					If[contacoppie == 2, probabilita =  2*((carteRimanenti-2) /carteRimanenti) * (2/(carteRimanenti-1)) + (2/(carteRimanenti*(carteRimanenti-1)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco c'\[EGrave] gia una coppia con la carta scelta, quindi la probabilit\[AGrave] \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>", cio\[EGrave] \[EGrave] la probabilit\[AGrave] che una carta con quel numero esca in una delle due carte ancora coperte.
		Per calcolarla sommiamo la probabilit\[AGrave] che una carta con lo stesso numero della carta scelta esca con la prima carta coperta (P1) alla probabilit\[AGrave] che esca alla seconda (P2).
		P1 e P2 sono uguali. P1 \[EGrave] uguale a "<>ToString[carteRimanenti]<>"(carte rimanenti) - 2 / "<>ToString[carteRimanenti]<>" * 2 /"<>ToString[carteRimanenti]<>" - 1
		A questo dobbiamo sommare il caso in cui il giocatore faccia Poker, quindi 2 / "<>ToString[carteRimanenti]<>" * "<>ToString[carteRimanenti]<>" - 1";
					];
					If[contacoppie == 1, probabilita = ((3/carteRimanenti) *  (2/(carteRimanenti-1)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso le carte coperte sul banco sono "<>ToString[carteBancocoperte]<>". Sul banco non c'\[EGrave] nessuna coppia con la carta scelta, quindi per fare tris Devono uscire carte del numero della carta scelta entrambe le volte.
		La probabilit\[AGrave] \[EGrave] quindi 3/"<>ToString[carteRimanenti]<>"* 2/"<>ToString[carteRimanenti-1]<>". 3 e 2 sono il numero di casi favorevoli.
		2 dato che dopo che esce il primo il numero di casi favorevoli diminuisce di uno.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					];
					,
				3,
					If[contacoppie == 2, probabilita = 3*(2*(carteRimanenti-3)/(carteRimanenti*(carteRimanenti-1)))+ 3*(2/(carteRimanenti*(carteRimanenti-1)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e \[EGrave] gia presente sul banco una coppia con la carta scelta.
		Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che un "<>ToString[cartaeffettiva]<>" esca in una tra le tre carte coperte del banco.
		Si tratta di sommare le 3 possibilit\[AGrave]: nel caso in cui esca nella prima, nella seconda e nella terza. Le tre probabilit\[AGrave] sono uguali
		La probabilit\[AGrave] \[EGrave] "<>ToString[carteRimanenti-3]<>"/"<>ToString[carteRimanenti]<>"*"<>ToString[carteRimanenti-2]<>"/"<>ToString[carteRimanenti-1]<>"*2/"<>ToString[carteRimanenti-2]<>".
		La formula si puo' semplificare togliendo carte rimanenti -2.
		A questo va sommata la probabilit\[AGrave] di fare Poker che \[EGrave] la probabilit\[AGrave] che escano due carte su tre con il valore della carta scelta.
		Questa probabilit\[AGrave] \[EGrave] 3* (le combinazioni) 2 / (carte rimanenti * carte rimanenti -1).
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					];
					If[contacoppie == 1, probabilita =  3*(3* 2* (carteRimanenti -3)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+ (3*2/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso il calcolo della probabilit\[AGrave] diventa pi\[UGrave] complicato. Il numero di carte scoperte \[EGrave] 3 e non \[EGrave] presente alcuna coppia sul banco con la carta scelta.
		Lo scopo \[EGrave] calcolare la possibilit\[AGrave] che per due volte "<>ToString[cartaeffettiva]<>" venga estratta tra le tre carte coperte del banco.
		Per calcolare la probabilit\[AGrave] finale dobbiamo calcolare qual \[EGrave] la probabilit\[AGrave] che escano due volte la carta che vogliamo su tre e moltiplicare questa probabilit\[AGrave] per 3.
		Questo viene fatto perch\[EGrave] dobbiamo permutare le possibilit\[AGrave] (P1). A questo aggiungiamo la probabilit\[AGrave] che esca 3 volte la stessa carta (P2).
		P1 \[EGrave] 3 * 2 * carte rimanenti -3 / i casi totali, che sono sempre carte rimanenti  e poi cr -1 e -2. 
		P2 \[EGrave] 3 * 2 * 1 / i casi totali.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					];
					,
				_,
				probabilita =0;
				];
				
				
				(* Controllo se c'\[EGrave] gi\[AGrave] un tris*)
				If[contacoppie  >3,probabilita = 1;
				spiegazione = "
	La probabilit\[AGrave] \[EGrave] 1. C'\[EGrave] gia un tris con la carta scelta sul banco.
	La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
	
				(* Controllo se non \[EGrave] possibile fare un tris*)
				If[contacoppie + carteBancocoperte<3,probabilita = 0; 
				spiegazione = "
	La probabilit\[AGrave] \[EGrave] 0. Non \[EGrave] possibilie fare un tris dato che non abbiamo abbastanza carte coperte.
	La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
				,
			
			(*MODALITA 4: calcola probabilit\[AGrave] di coppia di una carta random scoperta con le rimanenti da estrarre,gli altri giocatori hanno carte scoperte*)
			4,
				indicecartascelta=Random[Integer,{1,Length[carteBancoscoperte + 2]}];    (*scelgo randomicamente la carte su cui verr\[AGrave] fatta la domanda e memorizzo l'indice *)
				cartascelta=carteScoperteTotali[[indicecartascelta]];                      (*Assegno la carta scelta in base all'indice*)
				
				
				(* Calcolo la carta effettiva per poter sostituire la lettera al numero nel caso in cui sia una figura *)
				cartaeffettiva = Mod[cartascelta,13];
				cartaeffettiva = ToString[cartaeffettiva];
				Switch[cartaeffettiva,
				"0", cartaeffettiva = "K",
				"1", cartaeffettiva = "A",
				"11", cartaeffettiva = "J",
				"12", cartaeffettiva = "Q",
				_, "Errore"];
				
				(* Verifico se ci sono gia coppie tra banco e giocatore*)
				For[i=1,i<=Length[carteScoperteBG],i++,
				If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppie++;]];      (* se ci sono incremento il contatore contacoppie *)
				
				(* Verifico se ci sono gia coppie tra banco, giocatore e avversari*)
				For[i=1,i<=Length[carteScoperteTotali],i++,
				If[Mod[cartascelta,13]==Mod[carteScoperteTotali[[i]],13],contacoppietotali++;]];   (* se ci sono incremento il contatore contacoppietotali *)
				
				
				carteRimanenti=52-Length[carteScoperteTotali];
				
				(* Switch per analizzare le varie casistiche della modalit\[AGrave], in base al numero di carte coperte del banco*)
				Switch[carteBancocoperte,
				1,
					probabilita=(3-(contacoppietotali - contacoppie))/carteRimanenti;
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso, dato che c'\[EGrave] solo una carta coperta sul banco il calcolo della probabilit\[AGrave] non \[EGrave] complesso.
		Si tratta di contare il numero di casi favorevoli su casi totali, l'unica accortezza \[EGrave] nel fatto che devo controllare che i miei avversari non abbiano carte con lo stesso numero della carta scelta.
		In quel caso devo sottrarle a 3 che sarebbe il valore di casi favorevoli iniziale. In pi\[UGrave] devo stare attento al numero di casi totali che sono 52 - tutte le carte scoperte sul campo di gioco.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					,
				2,
					probabilita=2*((carteRimanenti-(3-(contacoppietotali - contacoppie)))*(3-(contacoppietotali - contacoppie)))/(carteRimanenti*(carteRimanenti-1))+ (((3-(contacoppietotali - contacoppie))*(2-(contacoppietotali - contacoppie)))/(carteRimanenti*(carteRimanenti-1)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		In questo caso il numero di carte coperte sul banco \[EGrave] 2, pi\[UGrave] il numero di carte coperte aumenta pi\[UGrave] il calcolo diventa complesso.
		Si tratta di calcolare la probabilit\[AGrave] che la carta scelta esca in una delle due carte coperte rimanenti.
		Possiamo dividere il calcolo della probabilit\[AGrave] in due parte. Che esca o nella prima corta o nella seconda e il caso in cui esca entrambe le volte.
		Per i casi favorevoli bisogna controllare che gli altri giocatori non abbiano una carta dello stesso numero della carta scelta, in quel caso il numero di casi favorevoli scende.
		La probabilit\[AGrave] \[EGrave] quindi binomiale di 2 (dato che le carte coperte sono 2) * casi favorevoli per casi non favorevoli diviso carte rimanenti per carte rimanenti -1. 
		A questo va sommata la probabilit\[AGrave] che esca la carta scelta entrambe le volte che \[EGrave] carte favorevoli per carte favorevoli -1 diviso diviso carte totali * carte cotali -1.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					,
					(*troppo complicato da calcolare a mente, non si prevede un caso in cui ci sono 4 carte da estrarre*)
					_,Print["ci sono piu di 2 carte da estrarre"]; probabilita =0;
					spiegazione = "Controllare errore.";];
					
					(*Controllo se c'\[EGrave] gi\[AGrave] una coppia tra le carte scoperte*)
					 If[contacoppie>1,probabilita = 1;
					 spiegazione = "La probabilit\[AGrave] \[EGrave] 1, c'\[EGrave] gia presente una coppia sul banco.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
		
					(*Controllo se non \[EGrave] possibile far uscire una coppia*)
					 If[contacoppietotali - contacoppie >2,probabilita = 0;
					 spiegazione = "La probabilit\[AGrave] \[EGrave] 0, non \[EGrave] possibile fare una coppia con la carta scelta.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
					,
				
				
			(*MODALITA 3: calcola probabilit\[AGrave] di coppia di una carta random scoperta con la prossima carta,gli altri giocatori hanno carte coperte *)
			3,
				carteScoperteplayer = Join[carteBancoscoperte, Take[carteGiocatore, 2]];
				
				indicecartascelta=Random[Integer,{1,Length[carteScoperteplayer]}];        (*scelgo randomicamente la carte su cui verr\[AGrave] fatta la domanda e memorizzo l'indice *)
				cartascelta=carteScoperteplayer[[indicecartascelta]];                     (*Assegno la carta scelta in base all'indice*)
				
				
				(* Calcolo la carta effettiva per poter sostituire la lettera al numero nel caso in cui sia una figura *)
				cartaeffettiva = Mod[cartascelta,13];
				cartaeffettiva = ToString[cartaeffettiva];
				Switch[cartaeffettiva,
				"0", cartaeffettiva = "K",
				"1", cartaeffettiva = "A",
				"11", cartaeffettiva = "J",
				"12", cartaeffettiva = "Q",
				_, "Errore"];
				
				(* Verifico se ci sono gia coppie tra banco e giocatore*)
				For[i=1,i<=Length[carteScoperteplayer],i++,
				If[Mod[cartascelta,13]==Mod[carteScoperteplayer[[i]],13],contacoppie++;]];   (* se ci sono, incremento il contatore contacoppie*)

				carteRimanenti=52-Length[carteScoperteTotali];
				
				(* Switch per analizzare le varie casistiche della modalit\[AGrave], in base al numero di carte coperte del banco*)
				Switch[carteBancocoperte,
				1,
					probabilita=3/carteRimanenti;
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		Dato che il numero di carte coperte \[EGrave] 1 questo esercizio \[EGrave] uguale agli esercizi della modalit\[AGrave] 1.
		Basta contare il numero di casi favorevoli fratto il numero di casi totali.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					,
				2,
					probabilita=2*(3*(carteRimanenti-3)/(carteRimanenti * (carteRimanenti -1)))+ (3*2/(carteRimanenti * (carteRimanenti -1)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		Il numero di carte coperte \[EGrave] 2. Quindi il calcolo della probabilit\[AGrave] consiste nel calcolare la possibilit\[AGrave] che la carta scelta esca o nella prima o nella seconda carta coperta che rimane.
		La probabilit\[AGrave] che esca la carta scelta nella prima carta \[EGrave] 3 / carte rimanenti * la probabilit\[AGrave] che non esca nella seconda, carte rimanenti - 3 / carte rimanenti - 1.
		La probabilit\[AGrave] che esca la carta scelta nella seconda carta \[EGrave] carte rimanenti - 3 / carte rimanenti * 3 / carterimanenti -1.
		Come si puo' notare le due probabilit\[AGrave] sono uguali.
		A questo va sommata la probabilit\[AGrave] che il numero della carta scelta esca sia nella prima carta coperta che nella seconda; che \[EGrave] 3 * 2 / carte rimanenti * carte rimanenti -1.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					,
				3,
					probabilita= 3*(((carteRimanenti-3)*(carteRimanenti-4)*3)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+3*(((carteRimanenti-3)*3*2)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)))+((3*2)/(carteRimanenti*(carteRimanenti-1)*(carteRimanenti-2)));
					numeratore = Numerator[probabilita];
					denominatore = Denominator[probabilita];
					spiegazione = "
		Il numero di carte coperte \[EGrave] 2. Quindi il calcolo della probabilit\[AGrave] consiste nel calcolare la possibilit\[AGrave] che la carta scelta esca o nella prima o nella seconda o nella terza carta coperta che rimane.
		La probabilit\[AGrave] che esca la carta scelta una sola volta \[EGrave] 3* la probabilit\[AGrave] che esca in ciascuno spot dei 3 coperti, cio\[EGrave]: 3 / carte rimanenti *  carte rimanenti - 3 / carte rimanenti - 1 *  carte rimanenti - 4 / carte rimanenti - 2.
		A questo va sommata la probabilit\[AGrave] che escano due carte con lo stesso valore della carta scelta, la probabilit\[AGrave] \[EGrave] 3 * carterimanenti -3 * 3 * 2 / (carte rimanenti * carte rimanenti -1 * carte rimanenti -2). 
		Per finire va sommata la probabilit\[AGrave] che il numero della carta scelta esca 3 volte; che \[EGrave] 3 * 2 / carte rimanenti * carte rimanenti -1 * carte rimanenti -2.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[numeratore]<>"/"<>ToString[denominatore]<>".";
					,
					
					_, Print["ci sono piu di 2 carte da estrarre"]; probabilita =0;
					spiegazione = "Controllare errore.";];
					
					(*Controllo se c'\[EGrave] gi\[AGrave] una coppia sul tavolo e aggiorno la probabilit\[AGrave] corretta e la spiegazione di conseguenza*)
					If[contacoppie>1,probabilita = 1;
					spiegazione = "C'\[EGrave] gia una coppia presente sul banco con la carta scelta.
		La probabilit\[AGrave] finale \[EGrave] "<>ToString[probabilita]<>".";];
					,
			_,
				Print["modalita ancora da implementare"];probabilita =0; (*non sono presenti altre modalit\[AGrave] oltre alla 4*)
			];
			
			(*Resituisco la probabilit\[AGrave] calcolata, la carta su cui \[EGrave] incentrata la domanda e la spiegazione della risposta*)
			Return[{probabilita, cartascelta, spiegazione}]; 
	]
End[]
EndPackage[]
