;*****************************************
; asmsyntax=tpasm (for vim)
;
; PICODOS  V1.1          03-07-85
;
; M.Broek  IJmuiden.  02550-32169.
;
;*****************************************
;
; Assembly gegevens voor PICODOS
; formaat.
;
;*****************************************
;
BLOK	equ	8	;aant. tracks/file
SIDES	equ	0	;SS=0 DS<>0
RANGE	equ	40	;max. aant. tracks
;
;*****************************************
;
; Deklaratie's.
;
; Page zero.
;
BASPNT	equ	$79
CHARGT	equ	$BC
CHARPT	equ	$C3
TRCNT 	equ	$E0
MESSPT	equ	$E3
INITCT	equ	$E5
REWRCT	equ	$F6
RDHULP	equ	$F7
RERDCT	equ	$F8
FNDHLP	equ	$F9
TTRACK	equ	$FA
SECHLP	equ	$FB
MSELEN	equ	$FD
MEMPNT	equ	$FE
;
; Page 2.
;
LOADFL	equ	$0203
SAVEFL	equ	$0205
INVEC 	equ	$0218
LOADVC	equ	$021E
SAVVEC	equ	$0220
POINT 	equ	$02FB
;
; Basic in ROM adressen.
;
FATERR	equ	$A262
CLEAR 	equ	$A47A
FCERR 	equ	$AE88
GET8  	equ	$B3AE
BASIC 	equ	$BD11
;
; Floppy interface adressen.
;
PIA   	equ	$C000
DORA  	equ	PIA
CRA   	equ	PIA+1
DORB  	equ	PIA+2
CRB   	equ	PIA+3
ACICR 	equ	PIA+16
ACIDA 	equ	PIA+17
;
; Deklaratie in EXT. DOS eprom.
;
ERRSTR	equ	$CE85    ;Foutteksten
;
; Adressen in monitor
;
MONLD 	equ	$FF8B
MSAVE 	equ	$FF96
OUTPUT	equ	$FFEE
;
H1	equ	START/256*256
H2	equ	LOADR/256*256
H3	equ	SAVER/256*256
H4	equ	PICIN/256*256
H5	equ	POINT/256*256
;
;*******************************************
;
; Bereken parameters voor filelengte.
;
;*******************************************
;
TDZT1	equ	BLOK*2048+763
TDZT2	equ	TDZT1/10000
TDZT3	equ	TDZT2*10000
;
DZT1	equ	TDZT1-TDZT3
DZT2	equ	DZT1/1000
DZT3	equ	DZT2*1000
;
HDT1	equ	DZT1-DZT3
HDT2	equ	HDT1/100
HDT3	equ	HDT2*100
;
TIEN1	equ	HDT1-HDT3
TIEN2	equ	TIEN1/10
TIEN3	equ	TIEN2*10
;
EENH	equ	TIEN1-TIEN3
;
FILES	equ	(RANGE-2)/BLOK
;
;*******************************************
;
; PICODOS operating system. Alleen track 0.
;
;*******************************************
;
	ORG	BLOK*2048+768
;
START   JSR HEADUN      ;unload head
	LDY #$00        ;reset Y
	LDA #$04        ;masker DDR/DOR
	STY CRA	        ;select DDR.
	LDY #$40        ;maak PA6 uitgang
	STY DORA        ;doe dat
	STA CRA	        ;select DORA
	LDA #$40        ;masker A
	STA DORA        ;leg vast
	LDA #$FF        ;masker A
	STA DORB        ;leg ook vast
	LDA INVEC       ;zet de inputvector
	STA PICIN+11    ;in picodos
	LDA INVEC+1
	STA PICIN+16
	LDA LOADVC      ;de loadvector
	STA LOADR+3     ;in picodos
	LDA LOADVC+1
	STA LOADR+4
	LDA SAVVEC      ;de savevector
	STA SAVER+3     ;in picodos
	LDA SAVVEC+1
	STA SAVER+4
	LDA #PICIN-H4   ;zet picodos in
	STA INVEC       ;de inputvector
	LDA #PICIN/256
	STA INVEC+1
	LDA #LOADR-H2   ;picodos load in
	STA LOADVC      ;de loadvector
	LDA #LOADR/256
	STA LOADVC+1
	LDA #SAVER-H3   ;picodos save in
	STA SAVVEC      ;de savevector
	LDA #SAVER/256
	STA SAVVEC+1
	JMP BASIC       ;naar koude basicstart
;
; De picodos loadroutine
;
LOADR   BNE LOADR1      ;LOAD + iets, weg
	JMP MONLD       ;anders gewoon LOAD
LOADR1  JSR BERTR       ;bereken track
	JSR LDPGM       ;laad file
	JSR COPPNT      ;pointers overnemen
	JMP CLEAR       ;via CLEAR naar basic
;
; De picodos saveroutine.
;
SAVER   BNE SAVER1      ;SAVE + iets, weg
	JMP MSAVE       ;anders gewoon SAVE
SAVER1  JSR BERTR       ;bereken track
	JSR SAVPGM      ;save file
	JMP COPPNT      ;herstel pointers
;
; Picodos input tijdens koude bascistart.
;
PICIN   LDA TEKST       ;Haal tekstkarakter
	INC PICIN+1     ;verhoog pointer
	CMP #$00        ;karakter nul?
	BNE PICIN1      ;nee, dan weg
	LDA #$00        ;herstel de input-
	STA INVEC       ;vector
	LDA #$00
	STA INVEC+1
	LDA #$0D        ;terug met CR.
PICIN1  RTS             ;terug naar input
;
COPPNT  LDX #$FC        ;stacpointer
	TXS             ;naar stackregister
	LDX #$04        ;teller in X
COPPN1  LDA POINT-1,X   ;filepointer
	STA BASPNT-1,X  ;naar basicpointer
	DEX             ;aftellen
	BNE COPPN1      ;tot klaar
	RTS
;
BERTR   JSR GET8        ;haal prgrammanummer
	TXA             ;naar accu
	BEQ BERTR1      ;progr 0, dan weg
	CMP #FILES+1    ;hoger dan toegestaan?
	BCC BERTR2      ;nee, dan verder
	JMP FCERR       ;geef FC error
BERTR1  LDX #$01        ;1 track
	TXA             ;op track 01
	BNE BERTR5      ;spring altijd
BERTR2  LDA #$00        ;reset accu
BERTR3  ADC #BLOK       ;+ filegrootte
	DEX             ;tel progrnr. af
	BNE BERTR3      ;totdat klaar
	SBC #$02        ;min de offset
	TAX             ;juiste tracknr in HEX
	LDA #$00        ;reset A, bereken BCD
	SED             ;decimaal mode
BERTR4  ADC #$01        ;plus 1
	DEX             ;tel af
	BNE BERTR4      ;niet klaar, terug
	CLD             ;weer hex, tracknr in A
	LDX #BLOK       ;filegrootte in X
BERTR5  STX TRACKS      ;leg dat vast
	STA HLPFLG      ;leg tracknr vast
	TSX             ;stackpointer redden
	INX             ;plus twee
	INX
	STX COPPNT+1    ;zet dat weg
	LDX #$04        ;teller in X
BERTR6  LDA BASPNT-1,X  ;basicpointer
	STA POINT-1,X   ;naar filepointer
	DEX             ;aftellen
	BNE BERTR6      ;tot klaar
	DEX             ;X wordt $FF
	STX INITCT      ;in teller
	db	 $A2       ;instr LDX
TRACKS  db	 $00       ;aantal tracks
	STX POINT+4     ;in filepointer
	PLA             ;pop returnadres
	CLC             ;geen carry
	ADC #$01        ;verhoog met 1
	STA BERTR7+1    ;zet weg
	PLA             ;pop ook HI-byte
	ADC #$00        ;plus carry
	STA BERTR7+2    ;zet weg
	LDA HLPFLG      ;haal tracknummer
BERTR7  JMP $0000       ;en RTS door JMP
HLPFLG  db	 $00       ;hulpvlag tracknr.
;
	NOP             ;dummy's, de teksten
	NOP             ;moeten in 1 pagina.
;
; Teksten voor koude start, eerst HIMEM,
; dan CR voor TERMINAL WIDTH, dan LOAD0
; en RUN.
;
TEKST   db	 TDZT2+$30,DZT2+$30
	db	 HDT2+$30,TIEN2+$30
	db	 EENH+$30,$0D
	db	 $0D
	db	 "LOAD0",$0D
	db	 "RUN",$00
;
;*******************************************
;
; Diskette drivers V3.3, alleen de nodige
; routine's voor picodos zijn nog aanwezig.
;
;*******************************************
;
DRIVES  db	 $01       ;huidige drive.
TRNUMB  db	 $00       ;huidig tracknr.
SENUMB  db	 $00       ;huidig sectornr.
SELEN   db	 $00       ;sector lengte
;
;  HOME.
;
HOME    JSR NEXTTR      ;verhoog eerst track
	JSR HOME2       ;vertraag
	STY TRNUMB      ;tracknr op 0
HOME1   LDA #$02        ;masker voor tr.0
	BIT DORA        ;test dat
	BEQ HOME2       ;bereikt, dan weg
	JSR DECTR       ;verlaag track
	BNE HOME1       ;en terug
HOME2   LDX #$0C        ;tijdsindex HOME.
;
; TIMER, dit is de algemene klok, in X moet
; de vertagingstijd staan.
;
TIMER   LDY #$31        ;referentiebyte
	JSR TIMEH       ;vertragingslus
	DEX             ;tel af
	BNE TIMER       ;niet klaar, terug
TIMER2  RTS             ;time out.
;
; Ga naar vorig track.
;
PREVTR  LDA DORB        ;B register
	ORA #$04        ;masker DIR hoog
	BNE NEXTT1      ;spring altijd
;
; Ga naar volgend track.
;
NEXTTR  LDA #$FB        ;masker DIR laag
	AND DORB        ;met poort B
NEXTT1  STA DORB        ;zet DIR goed
	JSR TIMER2      ;dummy vertraging
	AND #$F7        ;masker steppuls
	JSR DRHUL1      ;geef die
	JSR TIMEH1      ;dummy vertraging
	ORA #$08        ;masker steppuls
	JSR DRHUL1      ;geef einde
	LDX #$08        ;tydsindex steptime
	BNE TIMER       ;naar timer.
;
; Stel in op track in accu.
;
STRACK  STA TTRACK      ;bewaar tracknummer
	JSR SETTR       ;omleiding SS/DS
	PHA             ;tracknr op stack
	BEQ STRAC1      ;spring bij 0
	AND #$06        ;alleen bit 1 en 2
	BNE STRAC2      ;een geset, dan weg
STRAC1  PLA             ;tracknr van stack
	CMP #$40        ;40 ?
	BCC STRAC3      ;kleiner, dan goed
STRAC2  LDA #$08        ;fout #8 [bad tracknumber]
	BNE STRAC4      ;naar foutroutine
STRAC3  LDA DRIVES      ;haal selected drive
	AND #$01        ;drive 0 of 1
	TAY             ;naar Y
	JSR TSTRDY      ;test gewenste drive
	BCC STRAC5      ;weg bij ready
	LDA #$06        ;fout #6 [drive not ready]
STRAC4  JMP ERROR       ;naar KERNEL foutroutine
STRAC5  LDA TTRACK      ;haal gewenste track
	CMP TRNUMB      ;vergelijk met huidige
	BEQ ALGHLP      ;gelijk, dan weg
	BCS STRAC6      ;hoger, dan weg
	JSR PREVTR      ;track terug
	LDA #$99        ;optellen met min 1
	BCC STRAC7      ;spring altijd
STRAC6  JSR NEXTTR      ;verhoog een track
	TXA             ;A op 0, carry geset
STRAC7  SED             ;decimaal mode
	ADC TRNUMB      ;tel op/trek af
	STA TRNUMB      ;nieuw tracknr
	CLD             ;weer hexadecimaal
	JMP STRAC5      ;en terug.
;
; Hulprekenroutine voor timer.
;
HULPT   LDA TTRACK      ;opgegeven waarde
	ASL             ;maal 8
	ASL
	ASL
	TAX             ;resultaat naar X
	RTS             ;en klaar.
;
; Hulploops voor timer.
;
TIMEH   JSR TIHULP      ;vertraaginstruktie
	DEY             ;tel Y af
	BNE TIMEH       ;niet 0, terug
TIMEH1  NOP             ;dummy
	RTS             ;en klaar.
;
; Algemene hulproutine.
;
ALGHLP  LDY #$00        ;reset Y.
	RTS             ;en klaar.
;
; Vertraging voor interface.
;
DRHULP  BCS DRHUL1      ;carry geset, dan weg
	LDA #$40        ;low current (8' drive)
DRHUL0  ORA DORB        ;zet bit
DRHUL1  STA DORB        ;zet in register B
	RTS             ;klaar.
;
; Wacht op indeximpuls.
;
WAITIN  LDA DORA        ;haal register A
	BMI WAITIN      ;bit 7 hoog, terug
WAIT1   LDA DORA        ;index, haal A weer
	BPL WAIT1       ;bit 7 laag, terug
	RTS             ;index is geweest.
;
; Geef head-load, wacht op index, initialiseer
; de ACIA.
;
SREADY  JSR HEADLD      ;geef headload
SREAD1  JSR WAITIN      ;wacht op indexpuls
	LDA #$03        ;reset de ACIA en
	STA ACICR       ;initialiseer de ACIA:
	LDA #$58        ;RTS laag, 8 bits, even
	STA ACICR       ;pariteit, 1 stopbit.
	RTS             ;klaar.
;
; Geef head-load.
;
HEADLD  LDA #$7F        ;masker voor head-load
	AND DORB        ;testen met B register
HEADL1  STA DORB        ;zet dat byte
	LDX #$28        ;tijdsindex voor head
	JMP TIMER       ;naar timer.
;
; Geef head-unload.
;
HEADUN  LDA #$80        ;masker voor head-unload
HEADU1  ORA DORB        ;test met B register
	BNE HEADL1      ;en spring weg.
;
; Schrijf karakter in X-register naar disk.
;
WRDISK  LDA ACICR       ;lees kontroleregister
	LSR             ;test zendregister
	LSR
	BCC WRDISK      ;niet leeg, terug
	STX ACIDA       ;zet byte in zendregister
	RTS
;
; Lees een karakter van disk.
;
RDDISK  LDA ACICR       ;lees kontroleregister
	LSR             ;test ontvangstregister
	BCC RDDISK      ;niets binnen, terug
	LDA ACIDA       ;haal karakter
RDDIS1  RTS
;
; Schrijf een sector naar disk.
;
WRSECT  LDA SELEN       ;haal sectorlengte
	BEQ WRSEC1      ;nul, dan weg
	BPL WRSEC3      ;wel een waarde, weg
WRSEC1  LDA #$0B        ;fout #B [bad sector length]
WRSEC2  JMP ERROR       ;naar foutroutine
WRSEC3  CMP #$09        ;lengte 9 pagina's
	BPL WRSEC1      ;of groter, fout
	LDA #$02        ;masker track 0
	BIT DORA        ;testen
	BEQ RDDIS1      ;ja, dan naar een RTS
	LSR             ;sectorlengte /2
	STA TTRACK      ;zet weg voor rekenhulp
	LDA #$20        ;masker voor write protect
	BIT DORA        ;test dit
	BNE WRSEC4      ;niet, dan weg
	LDA #$04        ;fout #4 [write protected]
	BNE WRSEC2      ;naar foutroutine
WRSEC4  LDA #$01        ;1 maal rewrite
	STA REWRCT      ;toestaan
WRSEC5  LDA #$03        ;en 2 maal reread
	STA RERDCT      ;toestaan
	JSR FNDHDR      ;zoek track en sectorheader
	JSR DELAY2      ;naar vertraging
	LDA #$FE        ;masker voor write enable
	AND DORB        ;test met register B
	STA DORB        ;zet write
	LDX #$02        ;tijdsindex
WRSEC6  JSR DELAY3      ;vertraag
	LDA #$FF        ;dummy opdracht (voor 8' ?)
	AND DORB
	STA DORB
	JSR DELAY2      ;vertraag
	LDX #$76        ;sectorheader
	JSR WRDISK      ;naar disk
	LDX SENUMB      ;sectornummer
	JSR WRDISK      ;naar disk
	LDX SELEN       ;sectorlengte
	STX MSELEN      ;wegzetten in teller
	JSR WRDISK      ;en naar disk
	LDY #$00        ;reset Y
WRSEC7  LDA (MEMPNT),Y  ;byte uit geheugen
	TAX             ;naar X
	JSR WRDISK      ;en naar disk
	INY             ;volgend byte
	BNE WRSEC7      ;geen pag.einde, terug
	INC MEMPNT+1    ;pagina verhogen
	DEC MSELEN      ;teller verlagen
	BNE WRSEC7      ;niet klaar, terug
	LDX #$47        ;trailerbyte 1
	JSR WRDISK      ;naar disk
	LDX #$53        ;trailerbyte 2
	JSR WRDISK      ;naar disk
	LDA SELEN       ;sectorlengte
	ASL             ;maal 2
	STA MSELEN      ;zet weg
	ASL             ;maal 2
	ADC MSELEN      ;tel op
	TAX             ;naar X
	JSR DELAY3      ;naar vertraging
	LDA DORB        ;masker write enable
	ORA #$01        ;uitzetten
	STA DORB        ;doe dat
	LDX #$05        ;tijdsindex
WRSEC8  JSR DELAY3      ;naar vertraging
	LDA #$02        ;masker voor erase
	JSR DRHUL0      ;uitzetten
WRSEC9  CLC             ;geen carry
	TXA             ;X naar accu
	ADC MEMPNT+1    ;plus paginaadres
	SEC             ;set carry
	SBC SELEN       ;min sectorlengte
	STA MEMPNT+1    ;zet terug
	JSR REREAD      ;nalezen met geheugen
	BCS DELAY5      ;goed, dan weg
	DEC RERDCT      ;rereadteller -1
	BNE WRSEC9      ;geen nul, weg
	DEC REWRCT      ;rewriteteller -1
	BMI WRSE11      ;negatief, dan weg
	TXA             ;X naar accu
	ADC MEMPNT+1    ;plus paginaadres
	SEC             ;set carry
	SBC SELEN       ;min sectorlengte
	STA MEMPNT+1    ;nieuw paginaadres
	JMP WRSEC5      ;en opnieuw schrijven
WRSE11  LDA #$02        ;fout #2 [reread error]
	BNE RDCHA2      ;naar foutroutine
;
; Vertraag routine.
;
DELAY2  JSR HULPT       ;bereken X
DELAY3  LDA TIMER+1     ;haal referentiebyte
DELAY4  BIT $00	        ;test dat
	SEC             ;set carry
	SBC #$05        ;min 5
	BCS DELAY4      ;nog geset, terug
	DEX             ;X aftellen
	BNE DELAY3      ;niet klaar, terug
DELAY5  RTS             ;time out
;
; Lees een karakter van disk en kontroleer of dit
; gebeurt voordat het track voorbij is.
;
RDCHAR  LDA DORA        ;A register
	BPL RDCHA1      ;indexhole, dan weg
	LDA ACICR       ;ACIA kontroleregister
	LSR             ;test ontvangstregister
	BCC RDCHAR      ;niets binnen, terug
	LDA ACIDA       ;haal karakter
	RTS             ;en klaar
RDCHA1  LDA #$09        ;fout #9 [can't find trackheader]
RDCHA2  JMP ERROR       ;naar KERNEL foutroutine
;
; Zoek track- en sectorheader
;
FNDHDR  JSR SREAD1      ;wacht op index, init. ACIA
FNDHD1  JSR RDCHAR      ;lees karakter
FNDHD2  CMP #$43        ;trackheaderbyte 1 ?
	BNE FNDHD1      ;nee, terug
	JSR RDCHAR      ;lees karakter
	CMP #$57        ;trackheaderbyte 2 ?
	BNE FNDHD2      ;nee, terug
	JSR RDDISK      ;lees karakter
	CMP TRNUMB      ;juiste track ?
	BEQ FNDHD3      ;ja, dan verder
	LDA #$05        ;fout #5 [seek error]
	BNE RDCHA2      ;naar foutroutine
FNDHD3  JSR RDDISK      ;lees karakter
	DEC SENUMB      ;sectronummer min 1
	BEQ FNDHD5      ;nul, dan al goed
	LDA #$00        ;accu op 0
	STA FNDHLP      ;zet in teller
FNDHD4  JSR FNDSEC      ;lees een sector door
	BCC FNDHD6      ;fout, dan weg
	LDA SENUMB      ;haal sectornummer
	CMP FNDHLP      ;vergelijk met teller
	BNE FNDHD4      ;niet gelijk, opnieuw
	CMP SECHLP      ;test met gevonden sector
	BNE FNDHD6      ;niet, dan weg
FNDHD5  INC SENUMB      ;herstel sectornummer
	RTS             ;en klaar
FNDHD6  LDA #$0A        ;fout #A [can't find sector]
	BNE RDCHA2      ;naar foutroutine
;
; Reread. Als de accu nul is wordt de data
; in het geheugen gezet, anders wordt de disk
; met het geheugen vergeleken. Het resultaat
; van beide mogenlijkheden komt in de carry-
; vlag, geset is goed.
;
REREAD  PHA             ;bewaar opdracht
	JSR FNDHDR      ;zoek track + sector
REREA1  JSR RDCHAR      ;lees karakter
	CMP #$76        ;sectorheader ?
	BNE REREA1      ;nee, terug
	JSR RDDISK      ;lees karakter
	CMP SENUMB      ;goede sectornr ?
	BEQ REREA3      ;ja, dan weg
	PLA             ;herstel stack
REREA2  CLC             ;geen carry = fout
	RTS             ;en terug
REREA3  JSR RDDISK      ;lees karakter
	TAX             ;naar X
	STA SELEN       ;zet in sectorlengte
	LDY #$00        ;reset Y
	PLA             ;opdracht terug
	BEQ REREA5      ;weg voor inlezen
REREA4  LDA ACICR       ;vergelijken: ACIA
	LSR             ;test ontvangstregister
	BCC REREA4      ;nog leeg, terug
	LDA ACIDA       ;haal karakter
	BIT ACICR       ;test parityvlag
	BVS REREA2      ;geset, dan weg
	CMP (MEMPNT),Y  ;vergelijk met geheugen
	BNE REREA2      ;fout, dan weg
	INY             ;volgend byte
	BNE REREA4      ;geen pag.einde, terug
	INC MEMPNT+1    ;verhoog pagina
	DEX             ;tel af
	BNE REREA4      ;niet klaar, terug
	SEC             ;carry set = goed
	RTS             ;en klaar
REREA5  LDA ACICR       ;reread: ACIA test
	LSR             ;ontvangstregister
	BCC REREA5      ;niets binnen, terug
	LDA ACIDA       ;haal karakter
	BIT ACICR       ;test parity
	BVS REREA2      ;fout, dan weg
	STA (MEMPNT),Y  ;zet in geheugen
	INY             ;volgend byte
	BNE REREA5      ;geen pag.einde, terug
	INC MEMPNT+1    ;verhoog pagina
	DEX             ;verlaag teller
	BNE REREA5      ;niet klaar, terug
	SEC             ;set carry = goed
	RTS             ;en klaar.
;
; Lees een sector in het geheugen.
;
RDTRAC  LDA #$03        ;indirekte leespogingen
	STA RDHULP      ;in teller
RDTRA1  LDA #$07        ;direkte leespogingen
	STA RERDCT      ;in teller
RDTRA2  LDA #$00        ;opdracht lees in geheugen
	JSR REREAD      ;doe dat
	BCC RDTRA4      ;fout, dan weg
	RTS             ;anders klaar
RDTRA3  DEC MEMPNT+1    ;verlaag paginaadres
	INX             ;verhoog teller
RDTRA4  CPX SELEN       ;vergelijk met lengte
	BNE RDTRA3      ;niet gelijk, terug
	DEC RERDCT      ;verlaag direkte teller
	BNE RDTRA2      ;niet 0, opnieuw
	JSR PREVTR      ;rammel met head, eerst
	JSR HOME2       ;terug en vertragen
	JSR NEXTTR      ;dan weer naar originele
	JSR HOME2       ;track en vertragen
	DEC RDHULP      ;verlaag indirekte teller
	BPL RDTRA1      ;nog positief, weer proberenq
	LDA #$01        ;fout #1 [parity error]
	JMP ERROR       ;naar KERNEL foutroutine
;
; Zoek sector.
;
FNDSEC  LDA DORA        ;A register
	BPL FNDSE2      ;index bereikt, dan weg
	LDA ACICR       ;ACIA kontroleregister
	LSR             ;test ontvangstregister
	BCC FNDSEC      ;niets binnen, terug
	LDA ACIDA       ;haal karakter
	CMP #$76        ;sectorheader ?
	BNE FNDSEC      ;nee, terug
	JSR RDDISK      ;lees karakter
	STA SECHLP      ;zet dat weg
	JSR RDDISK      ;lees karakter
	STA TTRACK      ;zet ook weg
	INC FNDHLP      ;verhoog teller
	TAY             ;lengte naar Y
	LDX #$00        ;reset X
FNDSE1  JSR RDDISK      ;lees karakter
	DEX             ;tel X af
	BNE FNDSE1      ;geen 0, terug
	DEY             ;volgende pagina aftellen
	BNE FNDSE1      ;niet klaar, terug
	SEC             ;set carry = gevonden
	RTS             ;en klaar
FNDSE2  CLC             ;clear carry = niet gevonden
	RTS             ;en klaar
;
; Test of drive A of B ready is. Als Y nul is
; wordt drive B getest, anders drive A.
;
TSTRDY  LDA DORA        ;haal register A
	LSR             ;test bit 0
	PHP             ;resultaat op stack
	CPY #$00        ;test Y
	BNE TSTRD1      ;weg voor drive A
	PLP             ;herstel stack
	LSR             ;test bit 4
	LSR
	LSR
	LSR
	RTS             ;weg met resultaat
TSTRD1  PLP             ;herstel resultaat
	RTS             ;en weg
;
; Hulproutine voor timerhulp.
;
TIHULP  BNE TIHUL1
TIHUL1  RTS
;
; omleiding SS/DS.
;
SETTR   LDA SIDES       ;parameter ophalen
	BEQ SETTR2      ;weg bij SS-drive
	LDX #$00        ;reset X
	LDA TTRACK      ;haal gewenste track
	CMP #$40        ;40 of hoger?
	BCC SETTR1      ;nee, dan weg
	SED             ;decimaal
	SEC             ;set carry
	SBC #$39        ;min 39
	CLD             ;weer hex
	LDX #$01        ;X op 1, B-side
	STA TTRACK      ;omgerekend tracknr
SETTR1  LDY DRVTAB,X    ;bitpatroon driveselect
	STY DORA        ;selecteer A/B-side
SETTR2  BIT NEXTT1+15   ;test byte
	RTS             ;en terug.
;
; Tabel voor select side A/B
;
DRVTAB  db	 $40,$00
;
;********************************************
;
;  KERNEL voor picodos aangepast.
;
;********************************************
;
; Foutroutine voor DOS.
;
ERROR   JSR PRERR       ;druk fout af
	JMP FATERR      ;naar basic fatal error
;
; Print fout met behulp van EXTDOS rom.
;
PRERR   LDX #$00        ;reset de
	STX LOADFL      ;load en de
	STX SAVEFL      ;savevlaggen
	JSR HEADUN      ;unload de head
	JSR CRLF        ;nieuwe regel
	ASL             ;foutnr maal 2
	TAX             ;naar X
	LDA ERRSTR+1,X  ;haal adres tekst
	PHA             ;naar stack
	LDA ERRSTR,X    ;ook de HI-byte
	PHA             ;naar stack
	JMP MESSAG      ;geef de tekst
;
; Laad de file.
;
LDPGM   JSR INLDPN      ;zet pointers klaar
	STX TRCNT       ;teller op nul
	BEQ LDPGM2      ;spring altijd
LDPGM1  JSR INCTR       ;volgend track
LDPGM2  JSR RDTRAC      ;lees track
	INC TRCNT       ;verhoog teller
	DEC POINT+4     ;verlaag pointer
	BNE LDPGM1      ;niet klaar, terug
	LDA TRCNT       ;herstel de
	STA POINT+4     ;pointer en
	JMP HEADUN      ;unload
;
; Save de file.
;
SAVPGM  JSR INLDPN      ;zet de pointers klaar
	LDA POINT+4     ;aantal tracks
	STA TRCNT       ;naar teller
	LDA #$08        ;sectorlengte
	STA SELEN       ;vastzetten
SAVPG1  JSR WRSECT      ;schrijf track
	DEC TRCNT       ;tel af
	BEQ SAVPG2      ;klaar, dan weg
	JSR INCTR       ;verhoog track
	JMP SAVPG1      ;en terug
SAVPG2  JMP HEADUN      ;unload
;
; Pointers klaarzetten voor load en save.
;
INLDPN  JSR STRACK      ;zet track
	LDA #POINT/256  ;start file
	STA MEMPNT+1    ;overnemen
	LDA #POINT-H5   ;HI-byte
	STA MEMPNT      ;ook
	LDA #$01        ;sector 1
	STA SENUMB      ;vastzetten
	JMP HEADLD      ;load de head
;
; Verhoog tracknummer.
;
INCTR   SED             ;decimaal
	LDA TRNUMB      ;haal tracknummer
	BIT DORA        ;test A/B-side
	BVS INCTR1      ;weg voor A-side
	CLC             ;clear carry
	ADC #$39        ;correctie B-side
INCTR1  CLC             ;clear carry
	ADC #$01        ;plus 1
	CLD             ;weer HEX
	CMP INITCT      ;laatste?
	BEQ INCTR2      ;ja, dan weg
	BCS INCTR3      ;te ver, dan fout
INCTR2  JMP STRACK      ;stel track in
INCTR3  LDA #$0D        ;fout #D
	JMP ERROR       ;naar foutroutine
;
; Nieuwe regel.
;
CRLF    LDA #$0D        ;CR
	JSR OUTPUT      ;naar output
	LDA #$0A        ;linefeed
	JMP OUTPUT      ;naar output
;
; Messageprinter
;
MESSAG  PLA             ;pop returnadres
	STA MESSPT      ;en zet weg
	PLA
	STA MESSPT+1
	LDY #$01        ;teller tekst
MESSA1  LDA ($E3),Y     ;haal tekstbyte
	BEQ MESSA2      ;nul, dan klaar
	JSR OUTPUT      ;anders output
	INY             ;volgende
	BNE MESSA1      ;altijd terug
MESSA2  TYA             ;tekstlengte
	SEC             ;plus carry
	ADC $E3	        ;bij returnadres
	STA $E3	        ;wegzetten
	BCC MESSA3      ;geen carry, weg
	INC $E4	        ;verhoog pagina
MESSA3  JMP ($00E3)     ;naar returnadres
;
; Verlaag een track
;
DECTR   JSR PREVTR      ;naar vorig track
	INC MSELEN      ;verhoog teller
	BNE DECTR1      ;niet nul, RTS
	LDA #$06        ;fout #6 [Drive not ready]
	JMP ERROR       ;naar foutroutine
DECTR1  RTS
;
;****************************************
;
pdend
;
	NOLIST
	REPEAT	START+2048-pdend
	db	$ff
	ENDR
	LIST
;
;
