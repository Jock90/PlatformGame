

;if bottom of screen flickers at the start of each screen change blt to
;bne in vertblank

;a1200 version	- remember to section ALL of program into chip.
;				- make DMAWAITCTR larger e.g. $1000 instead of $300
;minimum of 6 bobs processed in every screen - pad out with P,0,0,PW,NXTSP if
;less - game speeds up noticably on a1200 if screen has < 6 bobs. 

;remember - no anim with TOGlocn

; ? able to replace these 2 lines .....

;		move.l	btabBptr,a0
;		move.w	#$ffff,(a0)

;with   move.l #$ffff,(btabBptr) ????

;when animating only need Lo-Hi-Hi-Lo do not need Hi-Lo-Lo-Hi since to
;get the second anim we only need set the start frame to Hi


;SFX channel 0-3 allocation :-
;~~~~~~~~~~~~~~~~~~~~~~~~~~
;plrsampch0 	- player sfx (footstep,land on rope)
;collectsampch1 - player collect object sfx (coins,buttons,etc)
;alien1sampch2	- lostlife,locked door,baddies sfx
;alien2sampch3	- more baddies!



;		      ##########################
;		      	Platform Designer V2.0 
;		      ##########################




;sort out routines that DO NOT have to be executed every frame
;e.g. read joystick/keyboard

;FIX :-
;	AD=3 animation
;	how to section code into public





;Screen Memory Format :-


;Each block is 16 bits wide by 16 bits high. The gamescreen Ymax=224
;As a result there are 20*14 = 280 blocks per gamescreen (scoreboard below)

;Screen Memory Format :-
;--------------------

;crunchblock,crunchblock,......(280 bytes/screen)
;blockstat,blockstat,..........(280*2 bytes/screen)

;crunchblock holds relevant block numbers

;blockstat (2 bytes) :-

;BYTE 1	bit 7 = solid status
;		bit 6 = is collect block crunchblock or mask block ?
;		bit 5 = animate conditionally 
;		bit 4 = re-defined block type
;		bit 3 = crunchblock/mask = Lo frame 
;		bit 2 = use default newscrn pos/user-defined 
;		bit 1 = 
;		bit 0 = collect while jumping/falling ? 
  

;BYTE 2	bit 7 = object taken/not taken status
;		bit 6 = use mask block? yes/no  
;		bit 5 = plr infront block/plr behind block 
;		bit 4 = animation on/off
;		bit 3 = collectable
;		bit 2 = ready to plot - used in the game only
;		bit 1 = mask type - own or computer-generated
;		bit 0 = ground



;Thus, each screen takes up 280*3 = 840 bytes
;In addition to this a table is built up when more info is required
;about each block. In this case 12 bytes (fixed) are added to the table
;for each entry/block, as follows :-


;scrnnum,bcomtype,scpos,Hi+st,anistat,Spd,Locn,newscrn,mask,score,repl,btype
;						     	   {newscrnpos}	
;(scpos,newscrnpos = Words)

;This series repeats from bcomtype to btype for each screen i.e. :-
;screennum,12 bytes*n,0,$fffe,screennum,12 bytes*n,0,$fffe......../$ffff

;a scrnpos entry of $fffe marks the end of the current screen
;a scrnpos entry of $ffff marks the end of all screens   

;The blocktype table at the end of the program holds 256 default values
;directly relating to each of the 256 available blocks. Each value represents
;a unique blocktype e.g. rope,ladder,ice,etc, plus default solid status
;Each blocktype byte in the table is structured as follows :-
;blocktype :-	bits 0-6 = blocktype (0-63),  bit 7 = solidstatus
;It is possible to alter the default blocktype in the game screen. This
;results in an entry in the binfotable.The new blocktype is stored in btype 
;It is also possible to combine up to 14 'commonly occuring' features with
;the current blocktype. These extra features are pre-set and are held in
;bcomtype. It's structure is as follows :-

;bcomtype	bit 7 = page1		bit 7 = page2
;			bit 6 =				bit 6 =
;			bit 5 =				bit 5 =
;			bit 4 =				bit 4 =
;			bit 3 = conveyor	bit 3 =
;			bit 2 = ladder		bit 2 =
;			bit 1 = rope		bit 1 =
;			bit 0 = ice  		bit 0 =

;Page1(bit7=0) or Page2(bit7=1) are thus selected via bit7


;Animated blocks are defined as bytes from Lo-frame (crunchbyte) to
;Hi-frame with a speed/frame delay held in Spd. In addition the starting
;frame can be set so that anim blocks of the same speed can be initialised
;to different starting ponts in the anim sequence. The start+Hi frames are
;held as offsets to the Lo frame in the same byte :-
;bits 0-3 = Hi-frame offset,  bits 4-7 = start-frame offset.   
;Thus, a maximum offset of 15 is allowed.  
;The anistat byte determines the following anim parameters :-
;anistat	bit 7 = base offset (0-7) / Xexit (auto door)
;			bit 6 =	" 	   / Yexit	"
;			bit 5 =	"
;			bit 4 = alter start frame	 
;			bit 3 = anim direction 4 is complete/ anim 3 & 4 dir status
;			bit 2 = anim direction (0-7)
;			bit 1 = 	"	"
;			bit 0 = 	"	" 

;Base is the start of an action jumptable.If base offset (0-7) is 0 then
;no action is taken here. A value of 1-7 gives rise to the following actions :-
;
;base offset	1 set Locn once Hi frame is reached
;				2 clear Locn once Lo frame is reached
;				3
;				4
;				5
;				6
;				7

;anim direction value:-	0
;			1	Lo-Hi, Lo-Hi ....
;			2	Hi-Lo, Hi-Lo ....
;			3	Lo-Hi-Lo-Hi  .... 
;			4	Lo-Hi once only then stop
;			5
;			6
;			7

;NOTE - since horizontal conveyors will always animate via dir 1 or 2
;we only need		btst.b #0,anidir
;					beq	convLeft	must=2 (left)  
;					....convRight



;Crumble blocks :-
;------- ------
; These are defined in a similar manner to an animation i.e :-
;
;	Lo		= Crunchblock or Mask
;	Hi		= EHi
;	Sp		= ESpeed (never changes)
;	TmpSp	= Elocn (each countdown to 0 increases a frame)
;	Curfame	=
;	
;This system thus allows us to add a static background block infront (mask)
;or behind (crunchblock) the actual gfx block being crumbled - in a similar
;manner to standard animation definitions. Note that the anim direction will
;always be Lo to Hi.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;General Labels :-
;--------------

Maxframe	=	20*16	;number of bob frames catered for in

centreXoffset	=	48
condXdelay	=	16		;No frames to wait before next condXY set
						;fnumconvert table
;Player right/left/up/down animation frame definitions
R1st		=	40	
RLst		=	45	
RT1			=	41		;right static target 1
RT2			=	41		;  "	"		"    2
L1st		=	46		
LLst		=	51	
LT1			=	47		;left static target 1
LT2			=	47		;  "	"		"   2
UD1st		=	20	
UDLst		=	25
CLIMB		=	1
WALK		=	2
DMAWAITCTR	=	$1000
PLSYNCSFXP1	=	RT1
PLSYNCSFXP2	=	LT1
PLSYNCSAMP	=	2
PLSYNCCHAN	=	3
SCOREDEL	=	20		;frame delay to print score


ALSPEED		=	1		;num pixels to add to x/y eACCh movement 
PLSPEED		=	1		; "        	"		"
PLANISP		=	2		;player anim speed (frames)
RSETSP		=	1		;anim reset speed (jstk at centre)	
COLBARSP	=	10		;speed (frames) of colour pulse
COLSTART	=	$002
COLEND		=	$005
COLADSUB	=	$001
STARTLIVES	=	5
MB			=	30		;max bob num (alter to suit max = 32)
MAXCOND		=	300		;condition table size
LASTSAMP	=	$ffff	;last sample definition reached
MAXFALL		=	33		;max fall height allowed (actual pixels)
MAXFALLjump	=	15		;maxfall if jumping right/left
MAXFALLWall	=	-15
YMIN		=	0		;min player y co-ord
YMAX		=	820		;max   "    "   "
XMAX		=	352
XMIN		=	48
PLANES		=	4		;num of bitplanes
BobscrnY	=	256		;height of bob anim screen
bwidth		=	6		;bob width
bwidth4		=	4	
scwidth		=	26*2	;gamescreen width (incl. boundary)
scheight	=	256		;gamescreen height
bscwidth	=	40		;blockscreen data width

maxscreen	=	50		;pace for 50 crunch screens (70000 bytes)
curmaxscreen	=	42
fontwidth	=	40		;40 bytes/characters wide
blockWd		=	20		;20 blocks wide
blockHt		=	14		;14 blocks high
blockWdp	=	2		;each block = 2 bytes wide
blockHtp	=	16		;  "    "   = 16 pixels high
NumBlocks	=	blockWd*blockHt
crunchscsize	=	blockWd*blockHt*3	;bytes per screen



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Blockinfo Entry labels :-
;----------------------

entrylength		=	12		;Num bytes per entry
newentrylength	=	11+5	;see entryscreen

BINFOSCRNEND	=	$fffe	;end of current blockinfo screen
BINFOTABEND	=	$ffff		;end of blockinfo table

Ecomtype	=	-1
Escpos		=	0
EHi			=	2
Eanistat	=	3
Espeed		=	4
ELocn		=	5
Enewscrn	=	6
Emask		=	7
Escore		=	8
Ereplace	=	9
Ebtype		=	10

;Block Status Information
;------------------------

;Byte 1
;------
SOLSTAT		=	7	;solid status (bit 7 of btype also !)
COLCRMSK	=	6	;0 (collect = crunchblock), 1 (collect= mask)
ANICOND		=	5	;0=anim always, 1=anim only if Locn=0
NEWBTYPE	=	4	;1=btype has been re-defined
ANILOBLK	=	3	;0(Lo=crunchblock) 1(Lo=Maskblock)
NEWSCPOSbit	=	2	;0 use default newscpos, 1 use user-defined 
;		=	1
COLLTYPE	=	0	;1=cannot collect while jumping/falling


;Byte 2
;------
TAKEN		=	7	;object taken/not taken
USEMASK		=	6	;1=use mask, 0=don't
BPRIORITY	=	5	;0=block behind bob, 1=block infront	
BANISTAT	=	4	;0=no anim, 1=anim on
COLLBIT		=	3	;1=collectable
READY		=	2
MASKTYPE	=	1	;0=use own, 1=use computer-generated mask
GRNDBIT		=	0	;1=can walk on block



;Block Feature Definition Table
;------------------------------

GRND		=	1
ROPE		=	2
LAD			=	3
CVR			=	4
PIPE		=	5
DOOR		=	6
ICE			=	7
DEAD		=	8
SETLOCn		=	9
CLRLOCn		=	10
TOGLOCn		=	11
COLLECT		=	12		;e.g. coin, fruit, etc
CRUMBLE		=	13
TELEPORT	=	14
XtraLife	=	15

JUMP		=	63		;enable both jump tables


;CombByte bit definitions :-
;------------------------
ICEbit	=	0
ROPEbit	=	1
LADbit	=	2
CONVbit	=	3

;Banistatbits :-
Yexit	=	6
Xexit	=	7
an34st	=	3



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Sprite Driver labels :-
;--------------------
	
	;d0 	    - cursprt (0,1,2,3,4,5,6,7)
	;d1    		- curwptr (0,2,4,6,8,10,12,14)   	
	;d2     	- curlwptr(0,4,8,12,16,20,24,28) 
	;(a1)   	- current sprite movement data word
	;(a2)   	- word table access
	;(a3)		- long word table access

R		=	1		;right,x
L		=	2		;left,x
U		=	3		;up,y
D		=	4		;down,y
P		=	5		;plot,x,y
SPD		=	6		;speed,n
ST 		=	7		;define start
JST		=	8		;jump to start
JC		=	9		;jump conditionally to start until loc n = 1
W		=	10		;wait for n counts
WC		=	11		;wait conditional until loc n = 1
PW		=	12		;permanent wait
LP 		=	13		;loop start,repeat number
JLP		=	14		;next loop
SC		=	15		;set condition locn n
CC		=	16		;clear condition locn n	
NB		=	17		;set number of bobs
AN		=	18		;define bob animation 
FP		=	19		;set bob shape to frame 
ANC		=	20		;set anim counter
AS		=	21		;set anim speed	
AD		=	22		;set anim direction (0-3)
AW		=	23		;wait until specified anim frame	is reached 
ASC		=	24		;synchronise sfx with anim frame p1 & p2
SBS		=	25		;set bobstatus - bit n of bob n
CBS		=	26		;clr     "        "    "      "
LFT		=	27		;define lift 
SETOBJ	=	28		;set screen object counter (num objects to proceed) 
SD		=	29		;SD,sampnum,channel	- play sample 
PD		=	30		;set default player x,y start
GS		=	31		;goto screen n
NS		=	32		;goto next screen
PS		=	33		;goto previous screen
RSET 	=	34		;jump to start of sp data
SPD2	=	35		;set num pixels to add when moving
SCX		=	36		;set condition when bobX=plrX
SCY		=	37
WCXY	=	38		;wait till cond set then set curbobXY to target bobXY
XYD		=	39		;frame delay between setting next condition
SKCS	=	40		;skip/jump to SKIP if cond set	
SKCC	=	41		;skip/jump to SKIP if cond clear	
SLDN	=	42		;slowdown - num frame delay added to spd per frame

curmaxtok	=	42


SKIP	=	$fffb	;jump search label for skCS/CC
NC		=	$fffc	;unconditional feature
NXTSP	=	$fffd	;next sprite pattern starts here
NXTSCRN	=	$fffe	;next screen starts here	
LSTSCRN	=	$ffff	;no more screens



PC	=	0	;clear/set condition,x,0/1,c
T	=	0	;teleport,x,xnew,ynew
TS	=	0	;screen - screen teleport
SS	=	0	;set bobstatus - bit n of bob n
CS	=	0	;clr     "        "    "      "



MR	=	1	;player facing right
ML	=	2	;  "       "   left	
MU	=	3
MD	=	4
MJL	=	5
MJR	=	6
MJU	=	7
MJD	=	8
MUR	=	9
MUL	=	10
MUD	=	11
F	=	12			;fire
MDoffset	=	13


ON		=	1

JU		=	0		;Y-1
JX		=	1		;X+/-1
JUXY	=	2		;Y-1,X+/-1
JD		=	3		;Y+1
JDXY	=	4		;Y+1,X+/-1
JE		=	255		;end of jump table
E		=	$fffd	;end of current platform/block data
PE		=	$fffe	;end of current platform screen
BE		=	$fffe	;end of current block anim definition screen  
SF		=	$fffc	;set block anim frame


;bobstatus definition bits :-
;	alien :-
LIFTBIT		=	0
MOVEBIT		=	1



;System Labels :-
;-------------

OpenLib		= 	-552  
CloseLib	= 	-414 
forbid		= 	-132
enable		= 	-138
allocmem	=	-$c6
freemem		=	-$d2	
open		=	-30
close		=	-36
read		=	-42
delay		=	-198
		
Exec		=	4
Joy2		=	$dff00c


;******** DEFINE ALL MACROS HERE  *************************************


	SECTION PlatformDesigner,code_C



testjumpRLU	MACRO
	cmp.b	#MJL,joy_dir
	beq	jumpleft	
	cmp.b	#MJR,joy_dir
	beq	jumpright	
	cmp.b	#MJU,joy_dir
	beq	jumpUP	
	ENDM

DMAwait		MACRO
	move.l	#DMAWAITCTR,d0
\@	dbra	d0,\@
	ENDM	



mvePLrightj	MACRO
	cmp.w	#XMAX,x
	bge	\@
	addq.w	#PLSPEED,x
	bra	\@1
\@	
	clr.b	jumpflag
	clr.b	jfallflag
	move.b	#1,falling
	move.b	#maxfallWall,maxfallctr
\@1	
	ENDM

mvePLright	MACRO
	cmp.w	#XMAX,x
	bge	\@
	addq.w	#PLSPEED,x
\@	
	ENDM

mvePLCVright	MACRO	;d0 holds speed
	cmp.w	#XMAX,x
	bge	\@
	add.w	#1,x
\@	
	ENDM


mvePLleftj	MACRO
	cmp.w	#XMIN,x
	ble	\@
	subq.w	#PLSPEED,x
	bra	\@1
\@	
	clr.b	jumpflag
	clr.b	jfallflag
	move.b	#1,falling
	move.b	#maxfallwall,maxfallctr
\@1	
	ENDM

mvePLleft	MACRO
	cmp.w	#XMIN,x
	ble	\@
	subq.w	#PLSPEED,x
\@	
	ENDM

mvePLCVleft	MACRO		;d0 holds value to be added
	cmp.w	#XMIN,x
	ble	\@
	sub.w	#1,x
\@	
	ENDM

mvePLup	MACRO
	cmp.w	#YMIN,y
	ble	\@
	sub.w	#PLSPEED*4,y
\@	
	ENDM

mvePLdn	MACRO
	cmp.w	#YMAX,y
	bge	\@
	add.w	#PLSPEED*4,y
\@	
	ENDM

DOCALC	MACRO				;converts bobx/y to block pos
	moveq	#0,d0
	move.w	x,d0
	move.w	y,d1
	move.w	d1,d3
	sub.w	#\1,d0			;sub x off-set from x
	add.w	#\2,d1			;add  y	"     to y	
	lsr.w	#4,d0			;/16	x = 0-31
	lsr.w	#6,d1			;convert y to 0-39
	lsl.w	#2,d1			;*4
	move.w	d1,d2
	lsl.w	#2,d1			;*16
	add.w	d2,d1			;*20 - point to correct row	
	add.w	d1,d0			;point to correct byte
	move.w	d0,\3			;blockpos
	move.l	crunchscptr,a0
	add.l	d0,a0
	move.b	(a0),\4			;crunch byte

	lea	btypescreentab,a1
	asl.w	#1,d0			;form word ptr	
		
	move.b	numblocks(a0),\5+1	;status byte 1
	move.b	(a1,d0),\5
	move.b	1(a1,d0),\6			;comb btype
	move.b	numblocks*2(a0),\7	;status byte 2
	ENDM
;thus, hi-byte of \5 = feature (0-63), lo-byte holds bit 7 info

ONTOP	MACRO				;is player on top of a block ?
	moveq	#0,d0
	move.w	x,d0
	move.w	y,d1
	move.w	d1,d3
	sub.w	#HSR,d0			;sub x off-set from x
	add.w	#HSF-(PLSPEED*4),d1	;add  y	"     to y	
	lsr.w	#4,d0			;/16	x = 0-31
	lsr.w	#6,d1			;convert y to 0-39
	lsl.w	#2,d1			;*4
	move.w	d1,d2
	lsl.w	#2,d1			;*16
	add.w	d2,d1			;*20 - point to correct row	
	add.w	d1,d0			;point to correct byte
	move.w	d0,blockposrf2	;blockpos
	ENDM

COLBLK	MACRO				;is player on top of a block ?
	moveq	#0,d0
	move.w	x,d0
	move.w	y,d1
	move.w	d1,d3
	move.l	crunchscptr,a0
	sub.w	#\1,d0			;#HSCLR,d0
	add.w	#\2,d1			;#HSCHF,d1
	lsr.w	#4,d0
	move.b	d0,\3			;colblk019
	lsr.w	#6,d1			;convert y to 0-39
	lsl.w	#2,d1			;*4
	move.w	d1,d2
	lsl.w	#2,d1			*16
	add.w	d2,d1			*20 - point to correct row	
	add.w	d1,d0
	add.l	d0,a0
	move.w	d0,\4			;colblkpos
	move.l	a0,\5			;colblkptr
	lea	btypescreentab,a1
	asl.w	#1,d0			;form word ptr	
	moveq	#0,d1
	move.b	(a1,d0),d1
	and.b	#63,d1
	move.b	d1,\6			;colblktype
	move.b	1(a1,d0),\7		;comb btype
	ENDM

;	move.b	40(a1,d0),d1
;	and.b	#63,d1
;	move.b	d1,colblkUFtype				;btype under feet
;	move.b	41(a1,d0),colblkUFctype		;comb btype
;	move.b	(a1,d0),d1
;	and.b	#63,d1
;	move.b	d1,colblkAHtype				;btype at head
;	move.b	1(a1,d0),colblkAHctype
;	ENDM




;********************************************************************

INITIALISE

	movem.l	d0-a6,-(a7)
	move.l	exec,a6
 	lea 	dosname(pc),a1
 	moveq 	#0,d0			;version number - don't care
 	jsr 	OpenLib(a6)
	move.l	d0,dosbase
	move.l	d0,a6
	move.l	#16*2,d1		;wait till disk activity finished
	jsr	delay(a6)
	movea.l	exec,a6
	move.l	dosbase,a1
	jsr	closelib(a6)
	jsr	forbid(a6)
	moveq	#0,d0
	lea	gfxname(pc),a1
	jsr	openlib(A6)
	move.l	d0,a1
	move.l	$26(a1),copper1store
	move.l	$32(a1),copper2store
	jsr	closelib(a6)
	lea	$dff000,a5
	move.w	dmaconr(a5),dmastore
	move.w	intenar(a5),intenastore
	move.w	$10(a5),adkonstore


	move.l	#crunchscrns,crunchscptr	;ptr to cur crunchscrn start





;Now convert block numbers to blockscreen addresses (faster for decrunching)	 
;-forming a table of 320 Lwords.
	move.l	#bnumconvert,a1		;blockdata gfx ptr table		
	move.l	#blockdata,a2
	move.l	#16-1,d1			;y counter
cvl1	move.l	#blockWd-1,d2	;x counter (19)			
cvl2	move.l	a2,(a1)+		;address into convert table			
	add.l	#2,a2				;point to next gfx block to right
	dbra	d2,cvl2		
	add.l	#(blockHtp*bscwidth*PLANES)-40,a2	;y=y+1,x=0
	dbra	d1,cvl1

;Next, convert block numbers to plot position offsets - forming a
;table of 280 Lwords
	move.l	#6,d0				;offset to top left of screen
	move.l	#plotoffset,a1		;screen offset table
	move.l	#14-1,d1			;y counter
cvl3	move.l	#blockWd-1,d2	;x counter (19)			
cvl4	move.l	d0,(a1)+		;screen offset into table
	add.l	#2,d0				;next plot offset accross the scrn
	dbra	d2,cvl4		
	add.l	#(blockHtp*scwidth*PLANES)-40,d0	;y=y+1,x=0
	dbra	d1,cvl3


;next, we'll convert the bob frame numbers to memory 
;addresses (using D-PAINT screen to blitter ILBM format)
;To access mask frames simply add 40*BobscrnY*4 to each address 
	move.l	#fnumconvert,a1			
	move.l	#bobdata,a2				;1st bob address in a2
	move.l	#(BobscrnY/16)-1,d1		;y counter (16)
cvl5	move.l	#(20-1),d2			;x counter (20)			
cvl6	move.l	a2,(a1)+			;address into convert table			
	add.l	#2,a2					;next word/bob across screen
	dbra	d2,cvl6		
	add.l	#((40*16*PLANES)-40),a2	;y=y+1,x=0
	dbra	d1,cvl5
						

;Now set up the Sample Pointers

	move.l	#sfx,a0			;start of samples in chip ram
	move.l	#sampptr,a1		;  "   "	 sample ptr table	
	move.l	#sampdef,a2		;  "   "	 sample definition table
setsampptr
	moveq	#0,d0
	cmp.w	#LASTSAMP,(a2)	;last sample reached ?
	beq	start				;table finished !
	move.l	a0,(a1)			;move chip ptr into table
	move.w	(a2),d0			;samp length in WORDS 
	mulu.w	#2,d0			; "	"    " BYTES
	add.l	d0,a0			;a0 pts to next samp in chip ram
	add.l	#sampdeflength,a2	;a2 pts to next samp definition
	add.l	#4,a1			;a1 pts to next sampptr
	bra	setsampptr					



start	clr.w	printflag
	clr.w	objectctr
	clr.b	newscreenflag
	clr.w	runflag
	clr.w	rsetanisp
	clr.l	plrsampch0		;clr ALL chan bytes
	clr.b	flag
	clr.l	score
	clr.b	collisionflag
	clr.b	transpflag
	clr.b	firebutflag
	clr.b	liftflag
	clr.b	icectr
	clr.b	icedir
	clr.b	jfallflag
	clr.b	falling
	clr.b	scoredelay
	clr.b	pause
	clr.b	lostlifeflag
	clr.b	maxfallflag
	move.w	#STARTLIVES,lives
	move.w	#RT1,curframe		;set player to face right
	move.w	#RT1,walkdir
	move.w	#1,curscrn			;set current screen to 1	
	move.w	#colbarsp,colbarspst
	move.w	#COLSTART,colbar
	move.w	#$ffff,Newscpos		;use default newscpos
	clr.w	colbarf

	bsr	introscreen

	bsr	vertblank
	move.w 	#$7fff,dmacon(a5)	;all DMA off

	move.l 	#gamecop,cop1lc(a5)	;game screen copper list
	move.w	#0,copjmp1(a5)		;d0,	load cop1lc into cop prog ctr

	move.w	#%1000001001001111,dmacon(a5)	;enable blit + ch0-3
	bsr	zerobase				;clear sprite tokens + cond flags
	bsr	zerovisstatus			;zero visited status
	bsr	setscreen				;get target gfx screen + bobmdataptrs
	bsr	setupspecialblocks		;blocks player can interact with	
	bsr	gettok0
	bsr	swapplotbob
	bsr	waitblit
	bsr	vertblank
	move.w	#%1000001111001111,dmacon(a5)	;enable btpl+cop also

;	bsr	setint
	move.w	#$0202,alien1sampch2
	bsr	playsamp
	move.l	#400,d0
	bsr	waitd0Vblank
	bra	mainloop


zerovisstatus				
	move.l	#maxscreen-1,d0
zerovs1	lea	crunchscrns,a0
zerovs2	add.l	#numblocks*2,a0		;point to visited status
	move.l	#numblocks-1,d1	
zerovs3	bclr.b	#TAKEN,(a0)+
	dbra	d1,zerovs3
	dbra	d0,zerovs2			;point to next screen	
	lea	condtab,a0
	move.l	#maxcond,d0
lp3	clr.b	(a0)+
	dbra	d0,lp3
	rts	

introscreen
	bsr	vertblank
 	move.w 	#$7fff,dmacon(a5)	;all DMA off

	move.l	#blanksfx,d0		;set sprite 0 ptr to blank data
	move.w	d0,spt0ptr+6		;set Lword - intro copper
	move.w	d0,sp0ptr+6			;set Lword - game copper
	lsr.l	#8,d0
	move.w	d0,spt0ptr+2		;set Hword
	move.w	d0,sp0ptr+2			;set Hword
	move.w	#%1111,dmacon(a5)	;turn off ALL audio channels
	move.w	#2,aud0len(a5)
	move.l	#blanksfx,aud0lch(a5)
	move.l	#blanksfx,aud1lch(a5)
	move.l	#blanksfx,aud2lch(a5)
	move.l	#blanksfx,aud3lch(a5)
	DMAwait
	

	move.l 	#introcop,cop1lc(a5)	;intro screen copper list
	move.w	#$00,copjmp1(a5)		;d0,	load cop1lc into cop prog ctr

	bsr	introscpoint
	lea	intcol,a2
	bsr	copycol
	lea	screen1,a0
	move.l	#scsize/2,d0		;size (words)
clearsc	clr.w	(a0)+
	dbra	d0,clearsc

	bsr	vertblank
	move.w	#%1000001111001111,dmacon(a5)	;enable btpl,blt,cop+ch0-3
	move.l	#introtxt1,a0
	move.l	#screen1,a1
	add.l	#scwidth*8*50+10+6,a1		;+6 to centre the screen	
	bsr	print
	move.l	#introtxt2,a0
	move.l	#screen1,a1
	add.l	#scwidth*8*100+11+6,a1		;+6 to centre the screen	
	bsr	print


intloop	tst.b	ciaapra			;wait till jstk button pressed
	bmi	intloop

	move.l	#intcol,tmp1L
	bsr	fadeout
	rts


fadeout	move.l	#16-1,d1	;No colowrs-1
	clr.w	intfdflag
	move.l	tmp1L,a0
fadelp1	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	move.w	2(a0),d2		;get colour (xxxxrrrrggggbbbb)
	move.w	d2,d3
	move.w	d2,d4
	and.w	#$f,d2			;isolate blue
	and.w	#$f0,d3			;  "	green
	and.w	#$f00,d4		;  "	red
	lsr.w	#4,d3
	lsr.w	#8,d4
	tst.w	d2
	beq	intfd1
	subq.w	#1,d2
	move.w	#1,intfdflag
intfd1	tst.w	d3
	beq	intfd2
	subq.w	#1,d3
	move.w	#1,intfdflag
intfd2	tst.w	d4
	beq	intfd3
	subq.w	#1,d4
	move.w	#1,intfdflag
intfd3	lsl.w	#4,d3
	lsl.w	#8,d4
	or.w	d3,d2
	or.w	d4,d2
	move.w	d2,2(a0)		;store new col into coplist
	moveq	#80,d0
	bsr	VBdelay
	addq.l	#4,a0			;get next colour
	dbra	d1,fadelp1
	tst.w	intfdflag
	bne	fadeout	
intfde	rts


VBdelay	move.l	vposr(a5),d2
	and.l	#$0001ff00,d2
	cmp.l	#$00010000,d2	;wait on line 256 or greater
	blt	vertblank
	dbf	d0,VBdelay
	rts





MAINLOOP
	tst.b	lostlifeflag
	bne	lostlife
	bsr	chkprintscore
	bsr	readkeyboard
	cmp.b	#$C,key
	bne	mainL2
	cmp.w	#curmaxscreen,curscrn
	beq	mainL3
	addq.w	#1,curscrn
	bra	gscrn1	
mainL2	cmp.b	#$B,key
	bne	mainL3
	cmp.w	#1,curscrn
	beq	mainL3
	subq.w	#1,curscrn
	bra	gscrn1	
mainL3	tst.b	pause
	bne	mainloop
	bsr	readjoystick
;	bsr	updatecolbar
	bsr	gettok0			;set x/y + move all sprites
	bsr	chkliftcoln
	bsr	platdecode	
	bsr	chkliftcoln
	move	#%1000010000000000,dmacon(a5)	;set blit 'nasty'
	bsr	swapplotbob		;plot bobs + swap screens
	bsr	vertblank
	bsr	scpoint	
	move	#%0000010000000000,dmacon(a5)	;clear blit 'nasty'

;	bsr	checkblockcollision	process any block collision
	bsr	playsamp			;ready to play a sample ?
	tst.b	newscreenflag
	bne	gscrn1
	tst.b	collisionflag	;player hit an alien ?
	beq	loop0				;- no
	bsr	looselife			;#####	- yes DEATH SEQUENCE HERE
	lea	condtab,a0
;	tst.b	255(a0)
;	beq	gscrn1


loop0	cmp.b	#$40,key	;space-bar pressed ?
	bne	loop1				;no
	tst.b	jtab12flag		;yes
	beq	loop1
	not.b	jtab12			;toggle jumptables
	bra	loop1				;check for release
loop1	btst	#6,ciaapra	;left mouse button pressed ?
	bne	mainloop			;no - 
	bra	ende


lostlife
	clr.b	lostlifeflag
	move.b	#22,alien1sampch2
	bsr	playsamp
	move.l	#300,d0
	bsr	waitd0Vblank
	clr.b	icectr
bbbb	clr.b	icedir
	clr.b	jumpflag
	clr.b	jfallflag
	clr.b	joy_dir
	move.l	#gamecol,tmp1L
	bsr	fadeout
	bra	gscrn1



chkprintscore			;print only if scoredelay=1
	tst.b	scoredelay
	beq	chkpscE
	subq.b	#1,scoredelay
	bne	chkpscE
	bsr	printscore
chkpscE	rts	


vertblank2
	move.l	vposr(a5),d2
	and.l	#$0001ff00,d2
	lsr.l	#8,d2
	cmp.w	#50,d2		
	bne	vertblank2
	rts

waitd0Vblank
	move.l	vposr(a5),d2
	and.l	#$0001ff00,d2
	lsr.l	#8,d2
	cmp.w	#50,d2		
	bne	waitd0Vblank
	dbra	d0,waitd0Vblank
	rts


updatecolbar
	subq.w	#1,colbarspst
	bne	updtcbe
	move.w	#colbarsp,colbarspst
	tst.w	colbarf
	bne	updtcb2
	cmp.w	#COLEND,colbar
	bne	updtcb1
	move.w	#1,colbarf
	rts	
updtcb1	add.w	#COLADSUB,colbar
updtcbe	rts
updtcb2	cmp.w	#COLSTART,colbar
	bne	updtcb3
	clr.w	colbarf
	rts		
updtcb3 sub.w	#COLADSUB,colbar
	rts





;_______________
playsamp
;---------------
	tst.b		plrsampch0		;request being made?
	bne	playch0					;yes
chkaud1	tst.b	collectsampch1	;no - test other channels
	bne	playch1
chkaud2	tst.b	alien1sampch2
	bne	playch2
chkaud3	tst.b	alien2sampch3
	bne	playch3
	rts

playch0	clr.l	d0
	lea	sampptr,a0
	lea	sampdef,a1
	move.b	plrsampch0,d0		;sample number
	subq.w	#1,d0
	move.l	d0,d1
	clr.b	plrsampch0			;take request off play list	
	mulu.w	#sampdeflength,d0	;form target sample def offset	
	add.l	d0,a1				;a1 pts to target samp def
	move.w	#1,dmacon(a5)		;turn off chan0
	mulu.w	#4,d1				;form Lword prt from samp num	
	DMAwait			;wait until DMA knows about it
	move.l	(a0,d1),aud0lch(a5)
	move.w	(a1),aud0len(a5)
	move.w	2(a1),aud0vol(a5)
	move.w	4(a1),aud0per(a5)
	move.w	#$8201,dmacon(a5)

	DMAwait			;wait until DMA makes its back-up copy
	move.w	#2,aud0len(a5)
	move.l	#blanksfx,aud0lch(a5)
	bra	chkaud1		


playch1	clr.l	d0
	lea	sampptr,a0
	lea	sampdef,a1
	move.b	collectsampch1,d0
	subq.w	#1,d0
	move.l	d0,d1
	clr.b	collectsampch1
	mulu.w	#sampdeflength,d0
	add.l	d0,a1
	move.w	#2,dmacon(a5)
	mulu.w	#4,d1
	DMAwait
	move.l	(a0,d1),aud1lch(a5)
	move.w	(a1),aud1len(a5)
	move.w	2(a1),aud1vol(a5)
	move.w	4(a1),aud1per(a5)
	move.w	#$8202,dmacon(a5)

	DMAwait
	move.w	#2,aud1len(a5)
	move.l	#blanksfx,aud1lch(a5)
	bra	chkaud2		

playch2	clr.l	d0
	lea	sampptr,a0
	lea	sampdef,a1
	move.b	alien1sampch2,d0
	subq.w	#1,d0
	move.l	d0,d1
	clr.b	alien1sampch2
	mulu.w	#sampdeflength,d0
	add.l	d0,a1
	move.w	#4,dmacon(a5)
	mulu.w	#4,d1
	DMAwait
	move.l	(a0,d1),aud2lch(a5)
	move.w	(a1),aud2len(a5)
	move.w	2(a1),aud2vol(a5)
	move.w	4(a1),aud2per(a5)
	move.w	#$8204,dmacon(a5)

	DMAwait
	move.w	#2,aud2len(a5)
	move.l	#blanksfx,aud2lch(a5)
	bra	chkaud3		

playch3	
	clr.l	d0
	lea	sampptr,a0
	lea	sampdef,a1
	move.b	alien2sampch3,d0
	subq.w	#1,d0
	move.l	d0,d1
	clr.b	alien2sampch3
	mulu.w	#sampdeflength,d0
	add.l	d0,a1
	move.w	#8,dmacon(a5)
	mulu.w	#4,d1
	DMAwait
	move.l	(a0,d1),aud3lch(a5)
	move.w	(a1),aud3len(a5)
	move.w	2(a1),aud3vol(a5)
	move.w	4(a1),aud3per(a5)
	move.w	#$8208,dmacon(a5)

	DMAwait
	move.w	#2,aud3len(a5)
	move.l	#blanksfx,aud3lch(a5)
	rts



;First, we'll set up the alien movement pointers for the current screen

setscreen
	move.w	curscrn,tmp1
	move.l	#firstscreen,a2		
	cmp.w	#1,tmp1			;found current screen ?
	beq	setsc2			
setsc0	subq.w	#1,tmp1			
	beq	setsc2				;if=0
setsc1	addq.l	#2,a2
	cmp.w	#NXTSCRN,(a2)	;use (a2)+
	bne	setsc1
	bra	setsc0				;beq setsc0 safe to use ?

setsc2	addq.l	#2,a2		;past NXTSCRN 
	move.l	#spptr,a1
hr0	move.l	a2,(a1)			;store bob mdata adrs in spptr	
	addq.l	#4,a1			;point to next spptr
hr1	addq.l	#2,a2
	cmp.w	#NXTSP,(a2)
	bne	hr1
	addq.l	#2,a2
	cmp.w	#NXTSCRN,(a2)
	beq	hr2
	cmp.w	#LSTSCRN,(a2)
	bne	hr0


;We'll plot the current crunchscreen

hr2	move.w	curscrn,tmp1		;now point to current gfx screen
	move.l	#crunchscrns,a2
hr3	cmp.w	#1,tmp1
	beq	hr4
	subq.w	#1,tmp1
	add.l	#crunchscsize,a2	;nxt crunchscreen
	bra	hr3	

hr4	move.l	a2,crunchscptr
	bsr	setinfoscptr
	bsr	setupentryscreen
	bsr	setupblocktables


	move.l	#screen1,visibleptr	;make this the visible screen
	bsr	decrunch		;always decrunches to screen1
	lea	gamecol,a2
	bsr	copycol			;set up screen colours

	tst.w	runflag
	bne	hr5

	move.l	#scoretxt,a0
	move.l	#screen1,a1
	add.l	#scwidth*8*118+2+6,a1	;+6 to centre the screen	
	bsr	print
	move.l	#livestxt,a0
	move.l	#screen1,a1
	add.l	#scwidth*8*118+30+6,a1	
	bsr	print
	move.l	#screen1,destscptr
	bsr	print_info

hr5	move.l	#restscreen,destscptr	;original background screen
	bsr	copyscreen
	move.l	#screen2,destscptr		;screen2 = dest screen	
	bsr	copyscreen		;copy screen1 to screen2
	bsr	scpoint			;set up screen pointers
	clr.b	scrnflag
	rts


;sets binfoscrnptr to the 1st scrnpos of the current screen
;if no screen exists then binfoscrnptr is set to zero.

setinfoscptr
	moveq	#0,d0
	move.w	curscrn,d0		;current screen number
	subq.b	#1,d0
	move.l	#blockinfotab+2,a2	;ptr to 1st scrnpos, 1st screen
setpLP1	cmp.b	-2(a2),d0
	beq	setptFS
setpLP2	cmp.w	#BINFOSCRNEND,(a2)
	beq	setptES
	cmp.w	#BINFOTABEND,(a2)
	beq	setptET
	add.l	#ENTRYLENGTH,a2
	bra	setpLP2					;find end of screen/table
setptES	add.l	#4,a2			;point to next screen
	bra	setpLP1					;check next screen number
setptET	clr.l	binfoscrnptr	;screen not found - clr ptr
	move.l	a2,binfoendptr		;set end of table ptr
	rts	
setptFS	move.l	a2,binfoscrnptr	;screen found - set ptr to start
	rts



 ;Scans the current infoscreen for an entry for the current scpos
 ;If found (a2) points to scpos, if not found entryflag is set to 1

findentry
	clr.b	entryflag
	move.l	binfoscrnptr,a2		;binfoscreen empty ?
	cmp.l	#0,a2
	bne	findeLP			-no
findenE	move.b	#1,entryflag
findenY	rts
findeLP	cmp.w	(a2),d3			;found entry?
	beq	findenY			yes
	cmp.w	#BINFOSCRNEND,(a2)
	beq	findenE
	cmp.w	#BINFOTABEND,(a2)
	beq	findenE
	add.l	#ENTRYLENGTH,a2		;point to next scrnpos/entry
	bra	findeLP



setupentryscreen
	bsr	clearentryscreen
;1st we'll set the crunch block for ALL entryscreen blocks
	move.l	crunchscptr,a0
	lea 	entryscreen,a1
	move.l	#numblocks-1,d0
setEsc0	move.b	(a0)+,1(a1)
	add.l	#newentrylength,a1
	dbra	d0,setEsc0	


;now we'll copy from binfoscrn to entryscreen
	move.l	binfoscrnptr,a0		;pts to scpos
	lea	entryscreen,a1		
setEsc1	cmp.w	#BINFOSCRNEND,(a0)
	beq	setEsc3
	cmp.w	#BINFOTABEND,(a0)
	beq	setEsc3
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.l	crunchscptr,a2
	move.w	(a0),d0			;get scpos
	add.l	d0,a2
	lsl.w	#4,d0			
		
;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset

	move.b	Ecomtype(a0),(a1,d0)
	btst.b	#banistat,numblocks*2(a2)
	beq	setEs2a
	move.b	Ehi(a0),d1
	and.b	#$0f,d1
	move.b	d1,14(a1,d0)		;set Hi-offset
	addq.b	#1,14(a1,d0)		;adjust
	move.b	(a2),d2
	btst.b	#ANILOBLK,numblocks(a2)	;Lo frame=crunch or mask?
	beq	setEsc2		
	move.b	Emask(a0),d2
;...d2 thus holds ani Lo frame....d1 holds offset
setEsc2	add.b	d2,d1
	move.b	d1,2(a1,d0)		;set Hi
	move.b	d2,3(a1,d0)		;set start		

	moveq	#0,d3
	move.b	Ehi(a0),d3
	and.b	#$f0,d3
	lsr	#4,d3
	add.b	d2,d3
	move.b	d3,4(a1,d0)		;set current/start

setEs2a	move.b	Eanistat(a0),5(a1,d0)
	move.b	Espeed(a0),6(a1,d0)
	move.b	Espeed(a0),7(a1,d0)
;	move.b	#0,7(a1,d0)		Spdst
	move.b	Elocn(a0),8(a1,d0)
	move.b	Enewscrn(a0),9(a1,d0)
	move.b	Emask(a0),10(a1,d0)
	move.b	Escore(a0),11(a1,d0)
	move.b	Ereplace(a0),12(a1,d0)
	move.b	Ebtype(a0),13(a1,d0)
settak	btst.b	#TAKEN,numblocks*2(a2)
	beq	settakE
	btst.b	#COLCRMSK,numblocks(a2)		;set approp replace block
	beq	settak1
	move.b	12(a1,d0),10(a1,d0)			;mask=replace block
	btst.b	#BANISTAT,numblocks*2(a2)
	beq	settak0
	btst.b	#ANILOBLK,numblocks(a2)
	beq	settak0
	move.b	12(a1,d0),2(a1,d0)			;-thus, mask=collect+anim	
	move.b	12(a1,d0),4(a1,d0)
settak0	move.b	#0,14(a1,d0)
	bra	settakE	

settak1	move.b	12(a1,d0),1(a1,d0)
	btst.b	#BANISTAT,numblocks*2(a2)
	beq	settakE
	btst.b	#ANILOBLK,numblocks(a2)
	bne	settakE
	move.b	12(a1,d0),2(a1,d0)	-thus, crunch=collect+anim	
	move.b	12(a1,d0),4(a1,d0)
	move.b	#0,14(a1,d0)
	
settakE	add.l	#entrylength,a0
	bra	setEsc1
	
;now set up blocktype screen table
;-format = btype,combtype,btype,combtype,...280*2 bytes
setEsc3	move.l	crunchscptr,a0
	lea	btypescreentab,a1
	lea	blocktypetab,a3
	lea	entryscreen,a4
	lea	condtab,a5
	moveq	#0,d0
	moveq	#0,d3
setEsc4	moveq	#0,d1
	moveq	#0,d2
	bsr	findentry
	clr.b	1(a1,d0)		;initialise CombBtype to 0
	tst.b	entryflag
	bne	setEsc5
	move.b	Ecomtype(a2),1(a1,d0)
setEsc5	move.b	(a0),d1				;crunch byte
	move.b	Ebtype(a2),d2
	btst.b	#NEWBTYPE,numblocks(a0)	;re-defined block ?
	bne	setEsc6	
	move.b	(a3,d1),d2				;get btype+solstat
setEsc6	and.b	#%01111111,d2		;isolate btype
	cmp.b	#TOGlocn,d2
	bne	setEsc8
	moveq	#0,d4
	move.b	Elocn(a2),d4
	btst.b	#COLCRMSK,numblocks(a0)
	bne	targmsk
;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset
;target = crunch
	tst.b	(a5,d4)			;locn set/clr ?
	bne	setEsc7	
	move.b	Espeed(a2),1(a4)	
	move.b	Ehi(a2),12(a4)
	move.b	Ehi(a2),2(a4)
	bra	setEsc8
setEsc7	move.b	Ehi(a2),1(a4)	
	move.b	Ehi(a2),2(a4)	
	move.b	Espeed(a2),12(a4)
	bra	setEsc8
;target = mask
targmsk	move.b	#1,14(a4)		;set own-mask offset to 1
	tst.b	(a5,d4)				;locn set/clr ?
	bne	tmsk1	
	move.b	Espeed(a2),10(a4)	

	move.b	Espeed(a2),4(a4)	

	move.b	Ehi(a2),12(a4)
	move.b	Ehi(a2),2(a4)
	bra	setEsc8
tmsk1	move.b	Ehi(a2),10(a4)	
	move.b	Ehi(a2),2(a4)	

	move.b	Ehi(a2),4(a4)	

	move.b	Espeed(a2),12(a4)
	bra	setEsc8

setEsc8	move.b	d2,(a1,d0)
	addq.l	#1,a0
	addq.w	#2,d0
	addq	#1,d3
	add.l	#newentrylength,a4
	cmp.w	#numblocks*2,d0
	bne	setEsc4
	lea	$dff000,a5
	rts								

;		scpos - screen offset address (word)		
;		animation - Lo frame block gfx address
;			  - Lo, Hi, Current + Start frames + Speed
;		replace block - block gfx address
;		mask - block gfx address


clearentryscreen
	lea	entryscreen,a0
	move.l	#(entrylength-2)*numblocks,d0
fillelp	clr.b	(a0)+
	dbra	d0,fillelp
	rts


;need to include own/comp mask tables e.g anitabLMOI,anitabLMCI -extra speed ??

setupblocktables
	move.l	crunchscptr,a0		;point to current crunchscreen
	lea	btypescreentab,a2

	move.l	#btabB,btabBptr
	move.l	#btabMoB,btabMoBptr
	move.l	#btabMcB,btabMcBptr
	move.l	#banitabI,banitabIptr	
	move.l	#banitabB,banitabBptr	
	move.l	#banitabLMoI,banitabLMoIptr	
	move.l	#banitabLMcI,banitabLMcIptr	
	move.l	#banitabLMoB,banitabLMoBptr	
	move.l	#banitabLMcB,banitabLMcBptr	
	move.l	#banitabLCoI,banitabLCoIptr
	move.l	#banitabLCcI,banitabLCcIptr
	move.l	#banitabLCoB,banitabLCoBptr
	move.l	#banitabLCcB,banitabLCcBptr
	
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
setblk1	btst.b	#BANISTAT,numblocks*2(a0)
	bne	setban
	btst.b	#BPRIORITY,numblocks*2(a0)
	bne	setbB
setblk2	addq.l	#1,a0
	addq.w	#1,d0			;d0 holds current block scrnpos
	addq.w	#2,d2
	add.w	#NEWENTRYLENGTH,d1
	cmp.w	#numblocks,d0
	bne	setblk1
setblkE	move.l	btabBptr,a0
	move.w	#$ffff,(a0)
	move.l	btabMoBptr,a0
	move.w	#$ffff,(a0)
	move.l	btabMcBptr,a0
	move.w	#$ffff,(a0)
	move.l	banitabIptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabBptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabLMoIptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabLMcIptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabLMoBptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabLMcBptr,a0	
	move.w	#$ffff,(a0)
	move.l	banitabLCoIptr,a0
	move.w	#$ffff,(a0)
	move.l	banitabLCcIptr,a0
	move.w	#$ffff,(a0)
	move.l	banitabLCoBptr,a0
	move.w	#$ffff,(a0)
	move.l	banitabLCcBptr,a0
	move.w	#$ffff,(a0)
	rts	
	
setbB	btst.b	#USEMASK,numblocks*2(a0)
	bne	setbBM
	btst.b	#COLCRMSK,numblocks(a0)
	beq	setB1
	cmp.b	#TOGlocn,(a2,d2)			
	beq	setbaB1					;let anim routine take care of TOGloc BEH
setB1	move.l	btabBptr,a1
	move.w	d0,(a1)+			;plot posn into table
	move.w	d1,(a1)+
	move.l	a1,btabBptr
	bra	setblk2

setbBM	btst.b	#MASKTYPE,numblocks*2(a0)
	bne	setbBMc
	btst.b	#COLCRMSK,numblocks(a0)
	beq	setbBM1
	cmp.b	#TOGlocn,(a2,d2)			
	beq	sanLMoB					;let anim routine take care of TOGloc BEH
setbBM1	move.l	btabMoBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,btabMoBptr
	bra	setblk2
setbBMc	btst.b	#COLCRMSK,numblocks(a0)
	beq	sbBMc1
	cmp.b	#TOGlocn,(a2,d2)			
	beq	sanLMcB					;let anim routine take care of TOGloc BEH
sbBMc1	move.l	btabMcBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,btabMcBptr
	bra	setblk2



setban	btst.b	#USEMASK,numblocks*2(a0)
	bne	setbanM
	btst.b	#BPRIORITY,numblocks*2(a0)	;plr behind ?
	bne	setbaB1
	move.l	banitabIptr,a1	
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabIptr
	bra	setblk2
setbaB1	move.l	banitabBptr,a1	
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabBptr
	bra	setblk2

setbanM	btst.b	#BPRIORITY,numblocks*2(a0)	;plr behind ?
	bne	setanMB
	btst.b	#ANILOBLK,numblocks(a0)		;0=crunch 1=mask
	bne	sanLMI
	btst.b	#MASKTYPE,numblocks*2(a0)
	bne	sanLCcI
	move.l	banitabLCoIptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLCoIptr
	bra	setblk2
sanLCcI	move.l	banitabLCcIptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLCcIptr
	bra	setblk2

sanLMI	btst.b	#MASKTYPE,numblocks*2(a0)
	bne	sanLMcI
	move.l	banitabLMoIptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLMoIptr
	bra	setblk2
sanLMcI	move.l	banitabLMcIptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLMcIptr
	bra	setblk2

setanMB	btst.b	#ANILOBLK,numblocks(a0)
	bne	sanLMB
	btst.b	#MASKTYPE,numblocks*2(a0)
	bne	sanLCcB
	move.l	banitabLCoBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLCoBptr
	bra	setblk2
sanLCcB	move.l	banitabLCcBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLCcBptr
	bra	setblk2

sanLMB	btst.b	#MASKTYPE,numblocks*2(a0)
	bne	sanLMcB
sanLMoB	move.l	banitabLMoBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLMoBptr
	bra	setblk2
sanLMcB	move.l	banitabLMcBptr,a1
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.l	a1,banitabLMcBptr
	bra	setblk2







setupspecialblocks		;(other blocks player can interact with)
	rts
	move.l	#bstatdef,a1	;as usual we'll search for our screen
	move.w	curscrn,tmp1w	
	subq.w	#1,tmp1w
	beq	setspb2				;target screen reached
setspb1	cmp.w	#E,(a1)+	
	bne	setspb1	
	cmp.w	#BE,(a1)		;last screen searched !
	beq	setspbe				;put ERROR routine here (non-existant screen) 
	subq.w	#1,tmp1w
	bne	setspb1				;continue search
setspb2	cmp.w	#E,(a1)		;found target screen
	beq	setspbe
	move.l	a1,bstatdefptr	
	move.l	#banionoff,a0	;start of anim definitions
	move.l	a0,a2
setspb3	clr.l	d0
	move.l	a2,a0			;restore start of anim definitions
	move.w	(a1)+,d0		;block number
	lsl.w	#1,d0			;form word ptr (*2)	
	add.l	d0,a0			;add offset to ptr
	move.w	(a1)+,NumBlocks*20(a0)	;block status
	move.w	(a1)+,NumBlocks*22(a0)	;x1
	move.w	(a1)+,NumBlocks*24(a0)	;x2
	move.w	(a1)+,NumBlocks*26(a0)	;y1
	move.w	(a1)+,NumBlocks*28(a0)	;y2

	cmp.w	#3,NumBlocks*20(a0)	;block status =3 ?
	bne	setspb4	
	move.w	(a1)+,NumBlocks*32(a0)	;score
	move.w	(a1)+,NumBlocks*34(a0)	;newblock

setspb4	cmp.w	#4,NumBlocks*20(a0)	;enable jtable 1 or 2
	bne	setspb5	
	move.w	(a1)+,NumBlocks*34(a0)	;newblock
	
setspb5	cmp.w	#5,NumBlocks*20(a0)	;object = umbarella
	bne	setspb6	
	move.w	(a1)+,NumBlocks*34(a0)	;newblock

setspb6	cmp.w	#6,NumBlocks*20(a0)	;object = screen teleporter
	bne	setspb7	
	move.w	(a1)+,NumBlocks*34(a0)	;condition num/NC
	move.w	(a1)+,NumBlocks*32(a0)	;new screen

setspb7	cmp.w	#7,NumBlocks*20(a0)	;set/clr condition
	bne	setspbx	
	move.w	(a1)+,NumBlocks*32(a0)	;value
	move.w	(a1)+,NumBlocks*30(a0)	;condition number
	move.w	(a1)+,NumBlocks*34(a0)	;new block



setspbx	cmp.w	#E,(a1)	
	bne	setspb3
setspbe	rts	

;		blockstat	 meaning
;		---------	 -------
;		  0	block(object) inactive ('taken') 
;		  1	loose life if collide	 
;		  2	gain life if collide (set blockstat to 0 after)
;		  *	set platform feature condition
;		  *	clear platform feature condition
;		  3	collectable object (bnum,3,x1x2y1y2,scr,newblk)
;		  4	enable jumptable 1&2 (bnum,4,x1x2y1y2,newblk)
;		  5	umbarella (bnum,5,x1x2y1y2,newblk)
;		  6	door (bnum,6,x1x2y1y2,cond/NC,newscreen)
;		  7	setcond (bnum,7,x1x2y1y2,val,cond to set,newblock)

;banionoff	dcb.w	NumBlocks		0=animoff 1=infront 2=behind
;banispeed	dcb.w	NumBlocks	*2
;banispdst	dcb.w	NumBlocks	*4
;banictr		dcb.w	NumBlocks	*6
;banictrst	dcb.w	NumBlocks	*8
;banidir		dcb.w	NumBlocks	*10
;banidir2	dcb.w	NumBlocks	*12
;bframelo	dcb.w	NumBlocks	*14	1st anim frame
;bframehi	dcb.w	NumBlocks	*16	last	"	"
;bcurframe	dcb.w	NumBlocks	*18	curent block frame
;bstatus		dcb.w	NumBlocks	*20	>0 = player interaction poss
;bx1		dcb.w	NumBlocks	*22	x co-ord as hit from right
;bx2		dcb.w	NumBlocks	*24	"   "     "      "   left
;by1		dcb.w	NumBlocks	*26	y co-ord as hit from top
;by2		dcb.w	NumBlocks	*28	"   "     "      "   bottom
;condnum	dcb.w	NumBlocks	*30	platform feature conditions	
;bscore		dcb.w	NumBlocks	*32	collectable block score
;newblock	dcb.w	NumBlocks	*34	new block							

zerobase
	move.l	#x,a1
	move.l	#bobstat,a4
	move.l	#MB-1,d0	
lp1	clr.w	MB*2(a1)		;Y
	clr.w	MB*6(a1)		;sprttok
	clr.w	MB*20(a1)		;anictr
	clr.w	MB*22(a1)		;anictrst
	clr.w	MB*24(a1)		;anidir
	clr.w	MB*46(a1)		;syncsfx
	clr.w	MB*52(a1)		;spd2
	clr.w	MB*54(a1)		;spd2st
	clr.w	MB*100(a1)		;slowdown
	clr.w	(a1)+			;X
	clr.l	(a4)+			;bobstat = Lword
	dbra	d0,lp1	
	move.w	#NumBlocks-1,d0
	move.l	#bstatus,a0
	move.l	#banionoff,a1	
lp2	clr.w	(a0)+
	clr.w	(a1)+
	dbra	d0,lp2
	lea	setconXtab,a0
	move.l	#(MB*22)-1,d0	;zero condelay also
clrCXY	clr.w	(a0)+
	dbra	d0,clrCXY
	lea	condtab+100,a0		;zero 40 bytes (WCXY 100- data)
	moveq.l	#40-1,d0
clrCT	clr.b	(a0)+
	dbra	d0,clrCT
	clr.l	d1			;current sp word ofset	
	clr.l	d2			;current sp long w ofset
	clr.b	jumpflag
	clr.b	jfallflag
	clr.b	falling
	clr.b	lostlifeflag
	clr.b	maxfallctr
	clr.b	maxfallflag
	rts



setint	
	move.w	#$7fff,intena(a5)		;disable all interrupts
	move.l 	$70,oldint
	move.l 	#CustomAudioInterrupt,$70		
	move.w 	#%1100000010000000,intena(a5)	;enable ch0-3 audio interrupt (lev4)
	rts

;NOTE:- to calculate when to turn a sample off use -
;	microseconds = .279365 * period * length

;*********** Level 4 (Audio) Interrupt Routine **********************
    
CustomAudioInterrupt
 	movem.l	d0-d7/a0-a6,-(a7)
	lea	$dff000,a5
	move.w	intreqr(a5),d0

cint1	btst	#7,d0
	bne	aud0int
	btst	#8,d0
	bne	aud1int
	btst	#9,d0
	bne	aud2int
	btst	#10,d0
	bne	aud3int

intend	movem.l	(a7)+,d0-d7/a0-a6
	rte
	Dc.w 	$4ef9			;jmp (oldint)
oldint	dc.l	0	

aud0int	
	move.w	#2,aud0len(a5)
	move.l	#blanksfx,aud0lch(a5)
;process other channels ?
	move.w	#$80,intreq(a5)		;acknowledge chan0 interrupt
	bra	intend

aud1int	
	move.w	#2,aud1len(a5)
	move.l	#blanksfx,aud1lch(a5)
	move.w	#$100,intreq(a5)	;acknowledge chan1 interrupt
	bra	intend

aud2int	
	move.w	#2,aud2len(a5)
	move.l	#blanksfx,aud2lch(a5)
;process other channels ?
	move.w	#$200,intreq(a5)	;acknowledge chan2 interrupt
	bra	intend

aud3int	
	move.w	#2,aud3len(a5)
	move.l	#blanksfx,aud3lch(a5)
;process other channels ?
	move.w	#$400,intreq(a5)	;acknowledge chan3 interrupt
	bra	intend




;*************** blockscreen decrunch routine *************************	

decrunch
	bsr	bltB1const
	lea	entryscreen,a1
	lea	bnumconvert,a2			;block address table
	move.l	visibleptr,a3		;visible screen
	add.l	#bwidth,a3			;centre the scremn
	move.l	#blockHt-1,d0		;y counter (blocks)
decr0	move.l	#blockWd-1,d1	;x counter    "
decr1	moveq	#0,d2
	move.b	1(a1),d2			;get crunch block
	add.l	#newentrylength,a1
	lsl.w	#2,d2				;form Lword ptr -multiply by 4
	bsr	blitblock1				;(a2,d2)=src, a3=dest
	add.l	#2,a3				;next screen block position
	dbra	d1,decr1			;plot next block along x
	add.l	#(blockHtp*scwidth*planes-blockWd*2),a3		;y=y+1
	dbra	d0,decr0			;plot remaining blocks


;we'll now see if any additional blocks require to be masked 
	move.l	crunchscptr,a1
	moveq	#0,d0
	move.l	#numblocks-1,d1
pwmskLP	btst.b	#USEMASK,numblocks*2(a1)
	bne	pwmskY
pwmsk1	addq.l	#1,a1		;next block
	addq.l	#1,d0			;next screen pos of block
	dbra	d1,pwmskLP	
	rts			
pwmskY	move.l	d0,d3
	lsl.w	#4,d3			;*16
	lsl	#2,d0				;*4 (form Lword ptr)
	move.l	visibleptr,a3	;destination screen
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	lea	entryscreen,a4
	moveq	#0,d2
;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset

	move.b	10(a4,d3),d2	;get block number to be masked
	lsl	#2,d2				;form Lword ptr -multiply by 4
	lea	bnumconvert,a2		;block address table
			;4(a2,d2) = srcA mask gfx
			;(a2,d2) = srcB	block gfx
			;a3 = srcC dest gfx
	btst.b	#MASKTYPE,numblocks*2(a1)
	beq	pwmskO
	bsr	bltCMconst
	bsr	generatemask
	bra	pwmske
pwmskO	bsr	bltOMconst
	bsr	blitblockwOmask
pwmske	lsr	#2,d0			;restore d0
	bra	pwmsk1				;do remaining blocks


generatemask
	movem.l	d0-d5/a0-a4,-(a7)	;save registers to the stack
	move.l	a3,a0			;dest	
	move.l	(a2,d2),a3		;orig block gfx

	lea	maskbuffer,a4		;mask built up into here
	move.l	#16-1,d4		;line counter
gmskLP	clr.l	d5
	or.w	(a3),d5			;line1 plane1 OR 1st mask line
	or.w	40(a3),d5		;    1      2		"
	or.w	80(a3),d5		;    1	   3		"
	or.w	120(a3),d5		;    1	   4		"
	add.l	#40*4,a3		;point to nxt line in blkscrn
;(a3) now points to line2 plane1 	
	move.w	d5,(a4)			;line1 mask finished - copy to mask plane1
	move.w	d5,2(a4)		;copy to mask plane2
	move.w	d5,4(a4)		;"	"	  3
	move.w	d5,6(a4)		;"	"	  4
	add.l	#8,a4
;(a4) now points to line2 plane1 	
	dbra	d4,gmskLP		;do remaining lines

	move.l	a0,a3
	bsr	blitblockwCmask

	movem.l	(a7)+,d0-d5/a0-a4	;restore registers
	rts





;********************************************************************
;	Draw bobs/ blocks/ etc onto screen 
;********************************************************************


swapplotbob

g0	not.b	scrnflag 		;initially zero
	bne	g1		

	move.l	#screen1,destscptr
	bsr	blockcollision		;check every 2nd frame
	bsr	plotreplaceblock
	move.l	#xy1,bobxyptr
	bsr	restorebackgrnd
	bsr	plotblocksINF
	move.l	#xy1,a1
	bsr	copyxy				;copy xy to xytab 1
	bsr	plotbobs			;plot bobs onto screen1
	bsr	plotblocksBEH
	move.l	#screen1,visibleptr	;screen1 = visible screen
	rts
	
g1	move.l	#screen2,destscptr
	bsr	plotreplaceblock
	move.l	#xy2,bobxyptr
	bsr	restorebackgrnd
	bsr	plotblocksINF
	move.l	#xy2,a1
	bsr	copyxy
	bsr	plotbobs
	bsr	plotblocksBEH
	move.l	#screen2,visibleptr	;screen 2 = visible screen
	rts

copyxy	move.w	#(MB*2)-1,d1	;max num bobs - 1 
	move.l	#x,a2				;bob x tib(into a2
cpxy1	move.w	(a2)+,(a1)+		;copy xy into cur xytab
	dbra	d1,cpxy1			;next xy
	rts
			
	
plotblocksINF
  ;plot anim blocks 
	bsr	bltB1const
	lea	banitabI,a4	
plotA	cmp.w	#$ffff,(a4)
	beq	anLMoI
	
;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0		;plot posn of block
	move.w	(a4)+,d1		;offset to block in entrytab
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3	;destination screen
	lsl	#2,d0				;*4 (form Lword ptr - blocknum*4)
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	moveq	#0,d2
	lea	bnumconvert,a2		;block address table
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2		;current frame
plotA0	lsl	#2,d2			;form Lword ptr -multiply by 4
	bsr	blitblock1
	move.b	1(a0,d1),d4		;set Lo frame		
	bsr	banimate
	bra	plotA				;do remaining blocks



anLMoI	bsr	bltOMconst
	lea	banitabLMoI,a4	
plotA1	cmp.w	#$ffff,(a4)
	beq	anLMcI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3	;destination screen
	lsl	#2,d0				;*4 (form Lword ptr - blocknum*4)
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	lea	bnumconvert,a2		;block address table
	lea	entryscreen,a0		
	move.b	1(a0,d1),d2		;crunch block
	lsl	#2,d2				;form Lword ptr -multiply by 4
	bsr	bltB1const
	bsr	blitblock1			;copy A-D	

	moveq	#0,d2
	moveq	#0,d4
	move.b	4(a0,d1),d2		;get cur frame
	move.b	d2,d4
	add.b	14(a0,d1),d4
	lsl	#2,d2
	lsl	#2,d4
	bsr	bltOMconst
	bsr	blitblockwOmask2
	move.b	10(a0,d1),d4	;set Lo frame		
	bsr	banimate
	bra	plotA1				;do remaining blocks



anLMcI	lea	banitabLMcI,a4	
plotA2	cmp.w	#$ffff,(a4)
	beq	anLCoI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3	;destination screen
	lsl	#2,d0				;*4 (form Lword ptr - blocknum*4)
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	lea	bnumconvert,a2		;block address table
	lea	entryscreen,a0		
	move.b	1(a0,d1),d2		;crunch block
	lsl	#2,d2				;form Lword ptr -multiply by 4
	bsr	bltB1const
	bsr	blitblock1			;copy A-D	
	moveq	#0,d2

	move.b	4(a0,d1),d2		;get cur frame
	lsl	#2,d2
	bsr	genmaskingame
	bsr	bltCMconst
	bsr	blitblockwCmask
	move.b	10(a0,d1),d4	;set Lo frame		
	bsr	banimate
	bra	plotA2				;do remaining blocks


anLCoI	lea	banitabLCoI,a4	
plotA3	cmp.w	#$ffff,(a4)
	beq	anLCcI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3	;destination screen
	lsl	#2,d0				;*4 (form Lword ptr - blocknum*4)
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	lea	bnumconvert,a2		;block address table
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2		;cur frame
	lsl	#2,d2				;form Lword ptr -multiply by 4
	bsr	bltB1const
	bsr	blitblock1			;copy A-D	
	move.b	1(a0,d1),d4		;set Lo frame		
	bsr	banimate
	moveq	#0,d2
	moveq	#0,d4
	move.b	10(a0,d1),d2	;get mask frame
	lsl	#2,d2
	move.b	d2,d4
	btst.b	#TAKEN,numblocks*2(a1)	
	beq	anLCoIa
	btst.b	#COLCRMSK,numblocks(a1)
	bne	anLCoIb

anLCoIa	bsr	bltOMconst
	bsr	blitblockwOmask
	bra	plotA3

anLCoIb	bsr	genmaskingame
	bsr	bltCMconst			;create new mask for replace block
	bsr	blitblockwCmask
	bra	plotA3




anLCcI	lea	banitabLCcI,a4
plotA4	cmp.w	#$ffff,(a4)
	beq	anLMcBI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3	;destination screen
	lsl	#2,d0				;*4 (form Lword ptr - blocknum*4)
	lea	plotoffset,a2
	add.l	(a2,d0),a3		;set plot position
	lea	bnumconvert,a2		;block address table
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2		;cur frame
	lsl	#2,d2
	bsr	bltB1const
	bsr	blitblock1			;copy A-D	

	moveq	#0,d2
	move.b	10(a0,d1),d2	;mask frame
	lsl	#2,d2				;form Lword ptr -multiply by 4
	bsr	genmaskingame
	bsr	bltCMconst
	bsr	blitblockwCmask

	move.b	1(a0,d1),d4		;set Lo frame
	bsr	banimate
	bra	plotA4				;do remaining blocks
	rts

;THE FOLLOWING ROUTINES PLOT THE CRUNCHBLOCK ONLY - PLOTBLOCKSBEH WILL
;THEN PLOT THE MASK - THUS, CRUNCH-->BOBS-->MASK ARE PLOTTED IN THAT ORDER 


anLMcBI	bsr	bltB1const
	lea	banitabLMcB,a4
plotA5	cmp.w	#$ffff,(a4)
	beq	anLCcBI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	1(a0,d1),d2		;crunch block
	lsl	#2,d2
	bsr	blitblock1	
	bra	plotA5
	rts


anLCcBI	bsr	bltB1const
	lea	banitabLCcB,a4
plotA6	cmp.w	#$ffff,(a4)
	beq	anLCoBI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2		;crunch block
	lsl	#2,d2
	bsr	blitblock1	
	move.b	1(a0,d1),d4
	bsr	banimate
	bra	plotA6
	rts


anLCoBI	bsr	bltB1const
	lea	banitabLCoB,a4
plotA7	cmp.w	#$ffff,(a4)
	beq	anLMoBI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2		;cur frame
	lsl	#2,d2
	bsr	blitblock1	
	move.b	1(a0,d1),d4
	bsr	banimate
	bra	plotA7
	rts


anLMoBI	bsr	bltB1const
	lea	banitabLMoB,a4
plotA8	cmp.w	#$ffff,(a4)
	beq	SbOMBI

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	1(a0,d1),d2		;crunch block
	lsl	#2,d2
	bsr	blitblock1	
	bra	plotA8
	rts


;plot static, own mask (mask behind)
SbOMBI	bsr	bltB1const
	lea	btabMoB,a4
plotA9	cmp.w	#$ffff,(a4)
	bne	SbOMBIa
	rts

SbOMBIa moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1

;Plot only if the crunchblock is a collectable 
	move.l	crunchscptr,a1		
	add.l	d0,a1
	btst.b	#COLLBIT,numblocks*2(a1)
	beq	plotA9	
	btst.b	#COLCRMSK,numblocks(a1)
	bne	plotA9

	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	moveq	#0,d2
	move.b	1(a0,d1),d2		;crunch
	lsl	#2,d2
	bsr	blitblock1
	bra	plotA9




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;NOW PLOT BLOCKS BEHIND 


;banitabLMoBptr	dc.l	0


plotblocksBEH
;plot anim, no mask
	bsr	bltB1const
	lea	banitabB,a4	
plotB	cmp.w	#$ffff,(a4)
	beq	plotBEH

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2
	lsl	#2,d2
	bsr	blitblock1
	move.b	1(a0,d1),d4
	bsr	banimate
	bra	plotB


plotBEH
;plot static,no mask
	bsr	bltB1const
	lea	btabB,a4
plotC	cmp.w	#$ffff,(a4)
	beq	plotBOmaskBEH

	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	move.b	(a1),d2
	lsl	#2,d2
	bsr	blitblock1
	bra	plotC




;plot static, own mask
plotBOmaskBEH
	bsr	bltOMconst
	lea	btabMoB,a4
plotD	cmp.w	#$ffff,(a4)
	beq	plotBCmaskBEH

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	10(a0,d1),d2
	lsl	#2,d2
	bsr	blitblockwOmask
	bra	plotD


;plot static, computer mask
plotBCmaskBEH
	bsr	bltCMconst
	lea	btabMcB,a4
plotE	cmp.w	#$ffff,(a4)
	beq	anLMcB

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	10(a0,d1),d2
	lsl	#2,d2
	bsr	genmaskingame
	bsr	blitblockwCmask
	bra	plotE


;plot anim, computer mask, Lo=mask
anLMcB	bsr	bltCMconst
	lea	banitabLMcB,a4
plotF	cmp.w	#$ffff,(a4)
	beq	anLCcB

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2
	lsl	#2,d2
	bsr	genmaskingame
	bsr	blitblockwCmask
	move.b	10(a0,d1),d4		
	bsr	banimate
	bra	plotF


;plot anim, computer mask, Lo=Crunch
anLCcB	bsr	bltCMconst
	lea	banitabLCcB,a4
plotG	cmp.w	#$ffff,(a4)
	beq	anLCoB

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	10(a0,d1),d2		;mask frame
	lsl	#2,d2
	bsr	genmaskingame
	bsr	blitblockwCmask
	bra	plotG


;plot anim, own mask, Lo=Crunch
anLCoB	bsr	bltOMconst
	lea	banitabLCoB,a4
plotH	cmp.w	#$ffff,(a4)
	beq	anLMoB

 	moveq	#0,d0
	moveq	#0,d1
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	10(a0,d1),d2		;mask frame
	lsl	#2,d2
	bsr	blitblockwOmask
	bra	plotH


;plot anim, own mask, Lo=Mask
anLMoB	bsr	bltOMconst
	lea	banitabLMoB,a4
plotJ	cmp.w	#$ffff,(a4)
	bne	MblkJY
	rts
MblkJY
 	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d4
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	move.l	crunchscptr,a1		
	add.l	d0,a1
	move.l	destscptr,a3
	lsl	#2,d0
	lea	plotoffset,a2
	add.l	(a2,d0),a3
	moveq	#0,d2
	lea	bnumconvert,a2
	lea	entryscreen,a0		
	move.b	4(a0,d1),d2
	move.b	d2,d4
	add.b	14(a0,d1),d4
	lsl	#2,d2
	lsl	#2,d4
	bsr	blitblockwOmask2
	move.b	10(a0,d1),d4
	bsr	banimate
	bra	plotJ




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

genmaskingame
	move.l	a0,tmp1L
	move.l	a1,tmp2L
	move.l	(a2,d2),a0		;get block addrs
	lea	maskbuffer,a1		;mask built up into here
	move.l	#16-1,d3		;line counter
gmskgLP	clr.l	d5
	or.w	(a0),d5			;line1 plane1 OR 1st mask line
	or.w	40(a0),d5		;    1      2		"
	or.w	80(a0),d5		;    1	   3		"
	or.w	120(a0),d5		;   1	   4		"
	add.l	#40*4,a0		;point to nxt line in blkscrn
;(a3) now points to line2 plane1 	
	move.w	d5,(a1)			;line1 mask finished - copy to mask plane1
	move.w	d5,2(a1)		;copy to mask plane2
	move.w	d5,4(a1)		;"	"	  3
	move.w	d5,6(a1)		;"	"	  4
	addq.l	#8,a1
;(a4) now points to line2 plane1 	
	dbra	d3,gmskgLP		;do remaining lines
	move.l	tmp1L,a0
	move.l	tmp2L,a1
	rts
	;(a2,d2)=block
	;a0=dest


;-d4 holds Lo frame (mask or crunch)
;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset

;anistat	 bit 7 = base offset (0-7)
;		 bit 6 =	" 
;		 bit 5 =	" 	
;		 bit 4 = alter start frame	 
;		 bit 3 = anim direction 4 is complete/anim 3 & 4 dir status 
;		 bit 2 = anim direction (0-7)
;		 bit 1 = 	"	"
;		 bit 0 = 	"	" 
;anim direction value:-	0
;			1	Lo-Hi, Lo-Hi ....
;			2	Hi-Lo, Hi-Lo ....
;			3	Lo-Hi-Lo-Hi  .... 
;			4	Lo-Hi once only then stop
;			5
;			6
;			7


banimate	

	btst.b	#ANICOND,numblocks(a1)
	bne	banicon
	btst.b	#BANISTAT,numblocks*2(a1)
	bne	baniSp
	beq	banimE

banicon	move.b	5(a0,d1),d2	;anistat
	and.b	#%00000111,d2	;isolate dir	
	cmp.b	#4,d2			;Lo-Hi-Stop
	beq	baniSp

	lea	condtab,a1
	moveq	#0,d5
	move.b	8(a0,d1),d5		;get locn
	tst.b	(a1,d5)
	beq	banimE				;must be set to animate

baniSp	tst.b	7(a0,d1)	;peedst=0?
	beq	banim0	
	subq.b	#1,7(a0,d1)
banimE	rts			

banim0	move.b	6(a0,d1),7(a0,d1)	;restore speed
	move.b	4(a0,d1),d0				;curframe
	movem.l	d2-d3,tmp1L
	moveq	#0,d2
	moveq	#0,d3
	move.b	5(a0,d1),d2		;anistat
	and.b	#%00000111,d2	;isolate dir	
	lsl	#1,d2				;*2
	move.w	d2,d3	
	lsl	#1,d2				;*4
	add.w	d3,d2			;*6
	lea	banimJTab,a3
	jmp	(a3,d2)
	
banimJTab
	jmp	banLoHi
	jmp	banLoHi
	jmp	banHiLo
	jmp	banLHHL
	jmp	banLHst

	jmp	banLoHi
	jmp	banLoHi
	jmp	banLoHi
	jmp	banLoHi
	jmp	banLoHi


banLoHi	movem.l	tmp1L,d2-d3
	cmp.b	2(a0,d1),d4		;Lo=Hi (taken)?
	beq	banime
	cmp.b	2(a0,d1),d0		;Hi=curframe ?
	beq	banLH1				
	addq.b	#1,4(a0,d1)
	rts
banLH1	move.b	d4,4(a0,d1)	;reset to Lo frame
	rts
	
banHiLo	movem.l	tmp1L,d2-d3
	cmp.b	2(a0,d1),d4		;Lo=Hi (taken)?
	beq	banime
	cmp.b	d0,d4			;curframe=Lo ?
	beq	banHL1				
	subq.b	#1,4(a0,d1)
	rts
banHL1	add.b	14(a0,d1),d4	;add Hi offset to Lo	
	subq	#1,d4
	move.b	d4,4(a0,d1)			;reset cur to Hi frame
	rts

banLHHL	movem.l	tmp1L,d2-d3
	cmp.b	2(a0,d1),d4			;Hi=Lo (object taken) ?
	bne	banLHH
	move.b	d4,4(a0,d1)
	rts
banLHH	btst.b	#an34st,5(a0,d1)	;L-H or H-L ?
	bne	banHLLH
	cmp.b	2(a0,d1),d0			;Hi=curframe ?
	beq	banLHch				
banLHH1	addq.b	#1,4(a0,d1)
	rts	
banLHch	bset.b	#an34st,5(a0,d1)
banHLLH	cmp.b	d0,d4			;curframe=Lo ?
	beq	banHLch				
	subq.b	#1,4(a0,d1)
	rts
banHLch	bclr.b	#an34st,5(a0,d1)
	bra	banLHH1

banLHst	movem.l	tmp1L,d2-d3		;animates from Lo-Hi if Locn[>0] =1
	lea	condtab,a1	 			;"     Hi-Lo "	"   "   =0
	moveq	#0,d5
	move.b	8(a0,d1),d5			;get locn
	beq	banLHs0					;locn=0 - ignore H-L
	tst.b	(a1,d5)
	beq	banHLst
banLHs0	cmp.b	2(a0,d1),d0		;Hi=curframe ?
	bne	banLHs1				
	bset.b	#an34st,5(a0,d1)	;signal animseq complete	
	rts
banLHs1	addq.b	#1,4(a0,d1)
	rts
banHLst	cmp.b	d4,d0			;Lo=curframe ?
	bne	banHLs1				
	bset.b	#an34st,5(a0,d1)	;signal animseq complete	
	rts
banHLs1	subq.b	#1,4(a0,d1)
	rts



checkblockcollision
	move.l	#numblocks*2,d0
	move.l	#bstatus,a0
chkbcl1	tst.w	(a0,d0)
	bne	chkbcl3			
chkbcl2	subq.w	#2,d0			;bstatus = words (thus sub 2)
	bpl	chkbcl1
	rts
chkbcl3	move.l	d0,tmp1L		;d0/2 = screen pos of block
	add.l	d0,a0	

	move.w	x,d1				;player x
	move.w	y,d2			  	;"    y

;is player between x1 & x2 :-	
	cmp.w	NumBlocks*2(a0),d1	
	blt	chkbcle					;player < block x1				
	cmp.w	NumBlocks*4(a0),d1	
	bgt	chkbcle					;player > block x2
;is player between y1 & y2
	cmp.w	NumBlocks*6(a0),d2
	blt	chkbcle					;player ( block y1
	cmp.w	NumBlocks*8(a0),d2
	bgt	chkbcle					;player > block y2
;player collided with block, ya bas

	move.l	crunchscptr,a1
	lsr.w	d0					;/2 - form block screenpos
	add.l	d0,a1
	btst.b	#TAKEN,numblocks*2(a1)	;is object already taken ?
	bne	chksobe					;yes - exit

	cmp.w	#1,(a0)				;loose life ?
	bne	chkbcl5
	tst.w	lives
	bne	chkbcl4
	move.w	#STARTLIVES+1,lives
chkbcl4	subq.w	#1,lives
	bset.b	#TAKEN,numblocks*2(a1) 	;take object
	bsr	printlives
	rts

chkbcl5	cmp.w	#2,(a0)			;gain live ?
	bne	chkbcl6
	cmp.w	#9,lives
	beq	chkbcl4
	addq.w	#1,lives
	bset.b	#TAKEN,numblocks*2(a1) 	;take object
	bsr	printlives
	rts



chkbcl6	cmp.w	#3,(a0)				;collgctable+add score ?
	bne	chkbcl7
	bset.b	#TAKEN,numblocks*2(a1) 	;take object
	clr.l	d1
	move.w	NumBlocks*12(a0),d1		;score to add
	add.l	score,d1
	move.l	d1,score
	move.b	#3,collectsampch1			;collobj
chkbc6a	move.w	NumBlocks*14(a0),-NumBlocks*2(a0)	;bcurframe
	move.w	NumBlocks*14(a0),-NumBlocks*4(a0)		;bframehi
	move.w	NumBlocks*14(a0),-NumBlocks*6(a0)		;bframelo
 	clr.w	(a0)			;switch off blockstat
	bsr	printscore	
	bsr	chkobjectctr		;decrement objectctr ?
	rts

chkbcl7	cmp.w	#4,(a0)		;enable jumptable 1+2 ?
	bne	chkbcl8
	bset.b	#TAKEN,numblocks*2(a1) 	;take object
	move.b	#1,jtab12flag
	move.b	#3,collectsampch1
	bra	chkbc6a
		
chkbcl8	cmp.w	#5,(a0)		;object = umbarella
	bne	chkbcl9
	bset.b	#TAKEN,numblocks*2(a1) 	;take object
;	move.w	#1,umflag
	move.b	#3,collectsampch1
	bra	chkbc6a

chkbcl9	cmp.w	#6,(a0)		;screen-screen teleporter
	bne	chkbcla
	cmp.b	#MU,joy_dir
	bne	chkbcle
	tst.b	transpflag
	bne	chk9e	

	cmp.w	#NC,NumBlocks*14(a0)
	beq	chkbc9a
	clr.l	d1
	move.w	NumBlocks*14(a0),d1		;cond tab offset (0,1,2,3,,,)
	lea	condtab,a2					;condtab base address		
	tst.b	(a2,d1)					;Locn = 1 ?
	beq	chkbcle		

chkbc9a	move.w	NumBlocks*12(a0),curscrn
	move.b	#1,newscreenflag
	move.w	#$5,alien1sampch2		;doorslide
	move.b	#1,transpflag
chk9e	rts

chkbcla	cmp.w	#7,(a0)				;set/clr condition
	bne	chkbcle
	clr.l	d1
	move.w	NumBlocks*10(a0),d1		;cond tab offset (0,1,2,3,,,)
	lea	condtab,a2					;condtab base address		
	move.b	NumBlocks*12+1(a0),(a2,d1)			;set/clr that locn
	move.w	NumBlocks*14(a0),-NumBlocks*2(a0)	;bcurframe
	move.w	NumBlocks*14(a0),-NumBlocks*4(a0)	;bframehi
	move.w	NumBlocks*14(a0),-NumBlocks*6(a0)	;bframelo
	move.b	#1,numblocks*4(a1) 		;set vis/not vis to 1 (take obj)
 	clr.w	(a0)					;switch off blockstat
	move.b	#$04,collectsampch1
	rts

chkbcle	move.l	#bstatus,a0
	move.l	tmp1L,d0
	bra	chkbcl2		;process next block	

chkobjectctr
	tst.w	objectctr
	beq	chksobe
	subq.w	#1,objectctr
	bne	chksobe
	move.b	#4,collectsampch1	
chksobe	rts	
	


printflag	dc.w	0
printsp		dc.w	4
printspst	dc.w	4
p1_ascii_score	dc.b	"00000000",0		;8 digit score
		even
score		dc.l	0
lives		dc.w	5

print_info
	bsr	printlives
	bsr	printscore
	bsr	printscrnnum
	rts


printlives
 	movem.l	d0-d1/a0-a3,-(a7)
	moveq	#0,d0
	move.w	lives,d0
	add.w	#"0",d0
	move.w	#1,printflag					;tell print routine that we are only
	move.l	#screen1+scwidth*8*118+36+6,a1	;printing one digit
	bsr	print1
	move.l	#screen2+scwidth*8*118+36+6,a1	
	bsr	print1
	clr.w	printflag
	movem.l	(a7)+,d0-d1/a0-a3
	rts
			


;NOTE:-Will only work with numbers less than 600000 (approx.)

printscore	move.l	#"0000",p1_ascii_score
		move.l	#"0000",p1_ascii_score+4
		lea	p1_ascii_score+8,a0	
		move.l	score,d0		
decconvert_loop	divu	#10,d0
		swap	d0	    	;remainder in bottom 16
		addi.b	#"0",d0		;decimal to ASCII equivelant
		move.b	d0,-(a0)	;ASCII character into score buffer
		clr	d0				;clear remainder, leave only quotient
		swap	d0
		tst	d0				;quotient=0?
		bne.s	decconvert_loop
		move.l	#screen1+scwidth*8*118+8+6,a1	
		lea	p1_ascii_score+1,a0		;ignore 1st 3 digits
		bsr	print		
		move.l	#screen2+scwidth*8*118+8+6,a1	
		lea	p1_ascii_score+1,a0		;ignore 1st 3 digits
		bsr	print		
		rts

printscrnnum	move.l	#"0000",p1_ascii_score
		move.l	#"0000",p1_ascii_score+4
		lea	p1_ascii_score+8,a0	
		moveq	#0,d0
		move.w	curscrn,d0		
dcvrt_l2	divu	#10,d0
		swap	d0	    	;remainder in bottom 16
		addi.b	#"0",d0		;decimal to ASCII equivelant
		move.b	d0,-(a0)	;ASCII character into score buffer
		clr	d0				;clear remainder, leave only quotient
		swap	d0
		tst	d0				;quotient=0?
		bne.s	dcvrt_l2
		move.l	#screen1+scwidth*8*118+18+6,a1	
		lea	p1_ascii_score+5,a0		;ignore 1st 3 digits
		bsr	print		
		move.l	#screen2+scwidth*8*118+18+6,a1	
		lea	p1_ascii_score+5,a0		;ignore 1st 3 digits
		bsr	print		
		rts



	
print	move.b	(a0)+,d0		;get letter
	beq	printe					;0 = end of text

print1	move.l	#font,a2
	move.l	#alphabet,a3		
	clr.l	d1
print2	tst.b	(a3)			;no more characters left i.e. error 
	beq	printe
	cmp.b	(a3)+,d0			;target = curent character ?
	beq	found					;yes
print3	addq.l	#1,d1			;no - continue alphabet search
	bra	print2

found	add.l	d1,a2			;character/byte offset + font start		
found1	move.b	(a2),(a1)		;character data to screen 1st line
	move.b	fontwidth(a2),scwidth(a1)
	move.b	fontwidth*2(a2),scwidth*2(a1)
	move.b	fontwidth*3(a2),scwidth*3(a1)

	move.b	fontwidth*4(a2),scwidth*4(a1)	;2nd line
	move.b	fontwidth*5(a2),scwidth*5(a1)
	move.b	fontwidth*6(a2),scwidth*6(a1)
	move.b	fontwidth*7(a2),scwidth*7(a1)

	move.b	fontwidth*8(a2),scwidth*8(a1)	;3rd line
	move.b	fontwidth*9(a2),scwidth*9(a1)
	move.b	fontwidth*10(a2),scwidth*10(a1)
	move.b	fontwidth*11(a2),scwidth*11(a1)
 
	move.b	fontwidth*12(a2),scwidth*12(a1)	;4th line
	move.b	fontwidth*13(a2),scwidth*13(a1)
	move.b	fontwidth*14(a2),scwidth*14(a1)
	move.b	fontwidth*15(a2),scwidth*15(a1)

	move.b	fontwidth*16(a2),scwidth*16(a1)	;5th line
	move.b	fontwidth*17(a2),scwidth*17(a1)
	move.b	fontwidth*18(a2),scwidth*18(a1)
	move.b	fontwidth*19(a2),scwidth*19(a1)

	move.b	fontwidth*20(a2),scwidth*20(a1)	;6th line
	move.b	fontwidth*21(a2),scwidth*21(a1)
	move.b	fontwidth*22(a2),scwidth*22(a1)
	move.b	fontwidth*23(a2),scwidth*23(a1)

	move.b	fontwidth*24(a2),scwidth*24(a1)	;7th line
	move.b	fontwidth*25(a2),scwidth*25(a1)
	move.b	fontwidth*26(a2),scwidth*26(a1)
	move.b	fontwidth*27(a2),scwidth*27(a1)

	tst.w	printflag		;called from printdigit ?
	bne	printe				;yes - exit this routine 
	addq.l	#1,a1			;next character position on screen
	bra	print				;no -  continue text print 
printe	rts						




nextsc
	add.w	#1,curscrn
	add.l	#(blockWd*blockHt),crunchscptr
	bsr	decrunch
	jmp	start		

prevsc
	sub.w	#1,curscrn
	sub.l	#(blockWd*blockHt),crunchscptr
	bsr	decrunch
	jmp	start		



;***********************************************************************
;*
;*			SPRITE DRIVER
;*
;**********************************************************************



gettok0		;clr.b	liftflag
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
gettok	move.l	#spptr,a1	;base sprite pointer adrs in a1
	add.l	d2,a1
	move.l	a1,a3			;base address of pointer table
	move.l	(a1),a1			;now move sp ptr into a1							
	move.l	#x,a2			;access y via MB*2(a2)
	add.l	d1,a2
;	move.w	MB*6(a2),curtok		curent sp token into curtok



gtk1	tst.w	MB*6(a2)		;if = 0 then get new token
	beq	newtok					;add every token check here if
	cmp.w	#R,MB*6(a2)			;the current sp (a1) is pointing
	bne	gtk2					;to a non-token word.
	btst	#MOVEBIT,MB*16(a3)	;ckeck movement status bit 
	beq	right			
gtk2	cmp.w	#L,MB*6(a2)
	bne	gtk3
	btst	#MOVEBIT,MB*16(a3)		
	beq	left
gtk3	cmp.w	#U,MB*6(a2)
	bne	gtk4
	btst	#MOVEBIT,MB*16(a3)
	beq	up
gtk4	cmp.w	#D,MB*6(a2)
	bne	gtk5
	btst	#MOVEBIT,MB*16(a3)
	beq	down
gtk5   	cmp.w	#W,MB*6(a2)
	beq	wait
	cmp.w	#WC,MB*6(a2)
	beq	waitconditional
	cmp.w	#AW,MB*6(a2)
	beq	aniwait
	cmp.w	#WCXY,MB*6(a2)
	beq	waitcondXY

	bra	nextsprt		;unrecognised token - get nxt sp

;cur sp token = 0 - now get new token

newtok	clr.w	MB*18(a2)		;anim speedst	
	clr.w	MB*8(a2)			;player  speed (not speedst!)
	move.w	(a1),MB*6(a2)		;copy token into cur sprttok
	move.w	(a1)+,curtok		;copy to curtok + get nxt word
	cmp.w	#SKIP,curtok
	beq	newtok

ntok1	moveq	#0,d4
	move.w	curtok,d4		;anistat

	cmp.w	#curmaxtok,d4
	bgt	nextsprt

	lsl	#1,d4				;*2
	move.w	d4,d5	
	lsl	#1,d4				;*4
	add.w	d5,d4			;*6
	lea	bgettokJTab,a4
	jmp	(a4,d4)

bgettokJTab
	jmp	nextsprt			;token = 0
	jmp	right
	jmp	left
	jmp	up
	jmp	down
	jmp	setxy
	jmp	setspeed
	jmp	setrtnadrs
	jmp	getrtnadrs
	jmp	jumpcond
	jmp	setwait
	jmp	waitconditional
	jmp	permanentwait
	jmp	setloop
	jmp	rptloop
	jmp	setcond
	jmp	clearcond
	jmp	setnumbobs
	jmp	setanimation
	jmp	setframe
	jmp	setanictr
	jmp	setanispd
 	jmp	setanimdirection
	jmp	aniwait
	jmp	synchronisesfx
	jmp	setbobstat
	jmp	clrbobstat
	jmp	definelift
	jmp	setobjectctr
 	jmp	setsample
 	jmp	setDxy
	jmp	gotoscreen
	jmp	nextscreen
	jmp	prevscreen
	jmp	reset
	jmp	setspeed2
	jmp	setcondX
	jmp	setcondY
	jmp waitcondXY
	jmp setcondDelay
	jmp	skipCSET
	jmp	skipCCLR
	jmp	slowdown

nextsprt
	tst.w	d0
	beq	nxtsp2				;cur bob = player
	bsr	chkaliencoln
	bsr	chkcondXYdelay		;delay before next condXY can be set
	bsr	chksetcondX			;curbobX=plrX ?
	bsr	chksetcondY
	cmp.w	#1,MB*14(a2)	;animation status on ?
	bne	nxtsp2				;no - continue
	bsr	animate				;yes- animate bob	
nxtsp2	move.l	#spptr,a4	;spptr table base address
	add.l	d2,a4			;point to curent sprite ptr
	move.l	a1,(a4)			;transfer data ptr to cur sp ptr
	addq.w	#1,d0			;0,1,2,...7	
	addq.w	#2,d1			;0,2,4,6,8,,,14
	addq.w	#4,d2			;0,4,8,12,...28
	cmp.w	num_bobs,d0		;reset cursprt to 0 ?
 	bne	gettok				;no - get token
	clr.w	curtok
	rts




chkaliencoln
	move.b	d0,d3			;copy cur bob num
	movem.l	d0-d7/a0-a6,-(a7)	;save all registers to the stack.
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	lea	colloffset,a0
;is player between alien x1 & x2 ?  
	move.w	(a2),d0			;current alien x-coord
	tst.w	d0
	beq	missaln
	move.w	MB*38(a2),d2		;curr alien frame num
	lsl.w	#1,d2			;*2 - form word ptr	
	sub.w	(a0,d2),d0		;aliex x - x1 offset for that frame

	btst	#LIFTBIT,MB*16(a3)	;hit LIFT ?
	bne	missaln					;- yes

;process non-lift collision
	cmp.w	x,d0
	bgt	missaln				;alien x1 > player x
	move.w	(a2),d0			;restore alien x
	add.l	#MAXFRAME*2,a0	;point to x2 
	add.w	(a0,d2),d0		;alien x + x2 offset
	cmp.w	x,d0
	blt	missaln				;alien x2 ( player x
;is player between alien y1 & y2 ?
	move.w	MB*2(a2),d0		;current alien y-coord
	add.l	#MAXFRAME*2,a0	;point to y1 offset
	sub.w	(a0,d2),d0		;alien y - y1 offset
	cmp.w	y,d0
	bgt	missaln				;alien y1 > player y
	move.w	MB*2(a2),d0
	add.l	#MAXFRAME*2,a0	;point to y2 offset
	add.w	(a0,d2),d0		;alien y - y2 offset
	cmp.w	y,d0
	blt	missaln			;alien y2 < player y
hitaln	move.b	#1,collisionflag	;***  HIT ALIEN  ***
missaln	movem.l	(a7)+,d0-d7/a0-a6	;restore registers.
;	cmp.b	liftflag,d0
;	bne	missale
;	clr.b	liftflag
missale	rts	
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;process lift collision

chkliftcoln
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	num_bobs,d3
	subq.w	#1,d3
	moveq	#1,d4		;bob num
	lea	x+2,a2			;point to 1st alien
	lea	spptr+4,a3

chklft1	btst	#LIFTBIT,MB*16(a3)	;hit LIFT ?
	bne	chklft2			;- yes
nextlft	subq.w	#1,d3
	addq.l	#2,a2
	addq.l	#4,a3
	addq.w	#1,d4
	tst.w	d3
	bne	chklft1
chklftE	clr.b	liftflag
	rts

chklft2	lea	colloffset,a0
	move.w	(a2),d0			;current alien x-coord
	tst.w	d0
	beq	nextlft
	move.w	MB*38(a2),d2	;curr alien frame num
	lsl.w	#1,d2			;*2 - form word ptr	
	sub.w	(a0,d2),d0		;aliex x - x1 offset for that frame
;is plr between lft x1 & x2 ?
	cmp.w	x,d0	
	bgt	nextlft				;lift x1 > player x
	move.w	(a2),d0			;restore alien x
	add.l	#MAXFRAME*2,a0	;point to x2 
	add.w	(a0,d2),d0		;alien x + x2 offset
	cmp.w	x,d0
	blt	nextlft				;lift x2 < player x
;is plr at lft y1 ?
	move.w	MB*2(a2),d0		;current alien y-coord
	add.l	#MAXFRAME*2,a0	;point to y1 offset
	sub.w	(a0,d2),d0		;alien y - y1 offset
	move.w	y,d1
	cmp.w	d1,d0
	bne	nextlft
hitlft3	;tst.b	liftflag
	;bne	hitlftE		already on a lift
	tst.b	jumpflag		;plr jumping ?
	beq	hitlft4				;no
	tst.b	jfallflag		;yes - on the way up/down ?
	beq	chklftE				;up - exit
hitlft4	move.w	#WALK,planim
	clr.b	jumpflag
	clr.b	jfallflag
	clr.b	fallflag
	clr.b	icectr
	tst.b	maxfallflag		;fall onto lift ok?
	beq	hitlft5
	clr.b	maxfallflag
	move.b	#1,lostlifeflag
hitlft5	clr.b	maxfallctr
;	move.b	#15,plrsampch0
	move.b	d4,liftflag		;bob number (1,2,3,4,..)
hitlftE	rts	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


looselife
	clr.b	collisionflag
	tst.w	lives
	bne	losel1
	move.w	#STARTLIVES,lives
	move.b	#1,lostlifeflag
	bsr	printlives
	clr.b	jumpflag
	clr.b	jfallflag
	clr.b	joy_dir
	rts
losel1	subq.w	#1,lives
	move.b	#1,lostlifeflag
	bsr	printlives
	clr.b	jumpflag
	clr.b	jfallflag
	clr.b	joy_dir
	rts


setxy	move.w	(a1)+,(a2)		;copy x into cur spx + point to y	
	move.w	(a1)+,MB*2(a2)		;copy y into cur spy + get nxt token
	bra 	newtok				;act on token with same sprite

setDxy	moveq	#0,d4			;set default/user defined newsc x,y
	moveq	#0,d5
	move.w	Newscpos,d4
	cmp.w	#$ffff,d4
	beq	setDxyY
setDxyL	cmp.w	#blockwd,d4
	blt	setDxy1
	sub.w	#blockwd,d4
	addq	#1,d5
	bra	setDxyL
setDxy1	lsl.w	#4,d4			;*16
	add.w	#centreXoffset,d4
	move.w	d4,(a2)				;store x
	lsl.w	#6,d5				;*16*4
	move.w	d5,MB*2(a2)			;store y		
	bne	setDxy2		y=0 ?
	addq.w	#4,MB*2(a2)			;set y to 4
setDxy2	cmp.w	#260,Newscpos
	blt	newtok
	move.w	#YMAX-4,MB*2(a2)	;adjust y
	bra	newtok
setDxyY	move.w	(a1)+,(a2)	
	move.w	(a1)+,MB*2(a2)
	bra 	newtok

setspeed	
	move.w	(a1),MB*8(a2)		;copy speed into spspeed
	move.w	(a1)+,MB*10(a2)		;copy to spspeedst + get next token
	move.w	#1,MB*52(a2)		;set x & y offsets to 1
	move.w	#4,MB*54(a2)
	bra	newtok					;act on token with same sprite

setspeed2	
	move.w	(a1),MB*52(a2)
	move.w	(a1)+,d4			;store x-offset
	lsl.w	#2,d4				;*4
	move.w	d4,MB*54(a2)		;store y-offset
	clr.w	MB*8(a2)			;zero spspeed + spspeedst
	clr.w	MB*10(a2)
	bra	newtok					;act on token with same sprite

setsample
	move.w	(a1)+,d3			;sample number		
	move.w	(a1)+,d4			;channel	number
	move.l	#plrsampch0,a4
	move.b	d3,(a4,d4)	
	bra	newtok

setrtnadrs
	move.l	a1,MB*8(a3)		;copy a1 into sprtn	
	bra	newtok	

getrtnadrs
	move.l	MB*8(a3),a1		;copy sprtn into a1
	clr.w	MB*6(a2)		;clear current sprite token
	bra	nextsprt

setobjectctr
	move.w	(a1)+,objectctr
	bra	newtok

setnumbobs
	move.w	(a1)+,num_bobs
	bra	newtok

synchronisesfx				;ASC,samp+chan,p1,p2
	clr.w	d3
	move.b	(a1),MB*46(a2)		;store samp num (1-255)
	move.b	1(a1),(MB*46)+1(a2)	;store chanel number (0-3)
	add.l	#2,a1
	move.w	(a1)+,MB*48(a2)		;target  frame1
	move.w	(a1)+,MB*50(a2)		;target	frame2
syncse	bra	newtok
	
	
;syncsfx	dcb.w	MB	*46	1stByte=sampnum(0=none) 2nd=chan
;syncsfxp1	dcb.w	MB	*48	synchronise sfx to p1 & p2
;syncsfxp2	dcb.w	MB	*50	

jumpcond
	clr.w	d3
	move.w	(a1),d3			;condtable offset
	lea	condtab,a4			;base address in a4
	tst.b	(a4,d3)
	beq	getrtnadrs
	add.l	#2,a1
	clr.w	MB*6(a2)		;clear cur token
	bra	nextsprt

setwait	move.w	(a1),MB*12(a2)		;copy counter into sp wait
	bra	nextsprt

wait	tst.w	MB*12(a2)
	beq	wait1
	sub.w	#1,MB*12(a2)
	bra	nextsprt
wait1	addq.l	#2,a1
	bra	newtok


waitconditional
	clr.w	d3
	move.w	(a1),d3
	lea	condtab,a4
	tst.b	(a4,d3)
	bne	wcon1
	bra	nextsprt
wcon1	addq.l	#2,a1
	bra	newtok

permanentwait
	bra	nextsprt
	


skipCSET			;SKCS,cond,,,,,,SKIP,,,,
	clr.w	d3
	move.w	(a1)+,d3
	lea	condtab,a4
	tst.b	(a4,d3)
	bne	skcs1
	bra	newtok
skcs1	cmp.w	#SKIP,(a1)+
	bne	skcs1
	bra	newtok
skipCCLR			;SKCC,cond,,,,,,SKIP,,,,
	clr.w	d3
	move.w	(a1)+,d3
	lea	condtab,a4
	tst.b	(a4,d3)
	beq	skcc1
	bra	newtok
skcc1	cmp.w	#SKIP,(a1)+
	bne	skcc1
	bra	newtok
	
slowdown
	move.w	(a1)+,MB*100(a2)
	bne	sldnE
	clr.w	MB*10(a2)
	clr.w	MB*8(a2)
sldnE	bra	newtok


definelift
	bset	#LIFTBIT,MB*16(a3)		;lift status bit = ON
	bra	newtok

setcond
	clr.l	d3
	move.w	(a1)+,d3		;0,1,2,3,,, - condtab ptr	
	lea	condtab,a4			;condtab base address		
	move.b	#1,(a4,d3)		;set that locn
	bra	newtok
	
clearcond
	clr.l	d3
	move.w	(a1)+,d3	
	lea	condtab,a4
	clr.b	(a4,d3)
	bra	newtok



setcondX		  ;when bobX=plrX bobnum is stored in setcondXtab 
	move.l	a2,tmp1L  		;bobnum will always be >0
setcXLp	moveq	#0,d3
	move.w	(a1)+,d3		;0,1,2,3,,, - condtab ptr	
	move.w	d3,MB*56(a2)	
	add.l	#MB*2,a2		;*2 - table size = word
	tst.w	d3				;0 marks end
	bne	setcXLp
	move.l	tmp1L,a2
	bra	newtok
setcondY 
	move.l	a2,tmp1L
setcYLp	moveq	#0,d3
	move.w	(a1)+,d3	
	move.w	d3,MB*76(a2)	
	add.l	#MB*2,a2
	tst.w	d3
	bne	setcYLp
	move.l	tmp1L,a2
	bra	newtok

setconddelay
	move.w	(a1)+,MB*98(a2)
	bra	newtok

waitcondXY		;WCXY,cond
	lea	condtab,a4
	moveq	#0,d4
	move.w	(a1),d4				;get cond num
	tst.b	(a4,d4)				;cond set ?
	beq	nextsprt	
	move.b	(a4,d4),d4			;-yes, get X/Y ptr
	and.l	#$000000ff,d4
	lea	x,a4
	move.w	(a4,d4),(a2)		;copy X/Y to curbob
	move.w	MB*2(a4,d4),MB*2(a2)	
	addq.l	#2,a1
WconE	bra	newtok

chkcondXYdelay
	tst.w	MB*96(a2)
	beq	chkCXdE
	subq.w	#1,MB*96(a2)
chkCXdE	rts

chksetcondX		;checks to see if curbobX=plrX
	tst.w	MB*56(a2)	;chk 1st table entry - 0=table empty
	beq	chkSCE
	tst.w	MB*96(a2)	;setcondelay=0 ?
	bne	chkSCE
	move.w	(a2),d4		;curbobX
	cmp.w	x,d4
	beq	chkSCX0
	addq.w	#1,d4
	cmp.w	x,d4
	bne	chkSCE
chkSCX0	move.l	a2,tmp1L
	lea	condtab,a4
	moveq	#0,d4
chkSCXL	move.w	MB*56(a2),d4	;get specified cond
	beq	chkSCE1					;end of table reached
	tst.b	(a4,d4)				;found free cond ?
	beq	chkSCXY					;-yes
	add.l	#MB*2,a2
	bra	chkSCXL
chkSCXY	move.b	d1,(a4,d4)		;store curbobnum*2 (always >0) in condtab
	move.l	tmp1L,a2	
	move.w	MB*98(a2),MB*96(a2)	;restore delay
	and.w	#$0fff,coltest
	addq.w	#4,coltest
	rts
chkSCE1	move.l	tmp1L,a2	
chkSCE	rts

chksetcondY		;checks to see if curbobY=plrY
	tst.w	MB*76(a2)
	beq	chkSCE
	tst.w	MB*96(a2)
	bne	chkSCE
	move.w	MB*2(a2),d4	curbobY
	cmp.w	y,d4
	beq	chkSCY0
	addq.w	#4,d4
	cmp.w	y,d4
	bne	chkSCE
chkSCY0	move.l	a2,tmp1L
	lea	condtab,a4
	moveq	#0,d4
chkSCYL	move.w	MB*76(a2),d4
	beq	chkSCE1
	tst.b	(a4,d4)
	beq	chkSCXY
	add.l	#MB*2,a2
	bra	chkSCYL

	


setbobstat			;SBS,bitnum,bobnum
	clr.l	d3
	clr.l	d4
	move.w	(a1)+,d3		;bobstatus bit
	move.w	(a1)+,d4		;bob number 0,1,2,3,4,,, 
	mulu	#4,d4           ;bob offset 0,4,8,12,14,,
	move.l	#bobstat,a4	
	bset	d3,(a4,d4)
	bra	newtok	

clrbobstat			;CBC,bitnum,bobnum
	clr.l	d3
	clr.l	d4
	move.w	(a1)+,d3	
	move.w	(a1)+,d4	
	mulu	#4,d4           
	move.l	#bobstat,a4	
	bclr	d3,(a4,d4)
	bra	newtok	

setframe
	move.w	(a1)+,MB*38(a2)		;frame num to curframe
	clr.w	MB*14(a2)		;switch off current anim (if any)
	bra 	newtok			;get next token



setanimdirection			
	move.w	(a1)+,MB*24(a2)		;0=lo to hi  1=hi to lo
	clr.w	MB*26(a2)		;2=lo to hi to lo
	cmp.w	#3,MB*24(a2)
	bne	newtok		
	move.w	(a1),MB*40(a2)		;wait
	move.w	(a1)+,MB*42(a2)		;waitst
	move.w	#1,MB*44(a2)		;set AD3flag
	bra	newtok			

aniwait	move.w	(a1),d3
	cmp.w	MB*38(a2),d3		;target = current frame ?
	bne	nextsprt		;no	
	add.l	#2,a1			;yes - get next token 
	bra	newtok
	bra	nextsprt	;don't clr curtok(MB*6(a2)) as we are	
				;still waiting	

setanimation
	move.w	(a1)+,MB*34(a2)		;1st frame to framelo
	move.w	(a1)+,MB*36(a2)		;2nd frame to framehi
	tst.w	MB*24(a2)		;test anim direction
	beq	setanr
	cmp.w	#2,MB*24(a2)		;anim direction
	beq	setanr			;do ani lo-hi first
	;do ani hi-lo
	move.w	MB*36(a2),MB*38(a2)	;framehi to curframe
	bra	setan2
	;do ani lo-hi
setanr	move.w	MB*34(a2),MB*38(a2)	;framelo to curframe
setan2	move.w	(a1),MB*16(a2)		;anim speed
	move.w	(a1)+,MB*18(a2)		;anim speedst	
	move.w	#1,MB*14(a2)		;switch on animation	
	bra	newtok


setanictr
	move.w	(a1),MB*20(a2)		;animation counter
	move.w	(a1)+,MB*22(a2)		;anim ctrst
	bra	newtok		
	
setanispd
	move.w	(a1),MB*16(a2)		;anim speed
	move.w	(a1)+,MB*18(a2)		;anim speedst	
	bra	newtok		

animate	tst.w	MB*44(a2)		;AD=3 ?
	beq	ani1					;no
	tst.w	MB*40(a2)			;yes. wait = 0 ?
	beq	ani1					;yes-animate
	subq.w	#1,MB*40(a2)		;no
	rts
ani1	tst.w	MB*18(a2)		;speed = 0 ?
	beq	ani2
	subq.w	#1,MB*18(a2)	
	rts
ani2	move.w	MB*16(a2),MB*18(a2)	;restore speed
	tst.w	MB*24(a2)			;anim direction
	beq	anilohi
	cmp.w	#2,MB*24(a2)		;lo to hi (to lo )?
	bne	anihilo
	tst.w	MB*26(a2)			;0=lo to hi  1=hi to lo
	beq	anilohi

;animate from ani hi to ani lo
anihilo	move.w	MB*38(a2),d3	;curent frame
	cmp.w	MB*34(a2),d3 	 	;= first frame ?
	beq	ani2lrs					;yes
	subq.w	#1,MB*38(a2)		;cur frame=cur frame-1
	bsr	playanimsfx
	rts
ani2lrs	cmp.w	#2,MB*24(a2)	;lo to hi to lo ?
	beq	ani2z	
	cmp.w	#1,MB*22(a2)		;last anictr ?
	beq	ani5					;stay at last frame
	bne	ani2lr2
	
;test anim counter - test only on the ani hi-lo stage here 
ani2z	tst.w	MB*20(a2)		;anim ctronoff
	beq	ani2lra					;ctr disabled
	subq.w	#1,MB*22(a2)
	bne	ani2lra
	clr.w	MB*14(a2)			;switch off anim	
	move.w	MB*20(a2),MB*22(a2)		;reset anim ctr
	clr.w	MB*26(a2)			;do ani lo-hi next
aniend	bsr	playanimsfx
	rts

ani2lra	clr.w	MB*18(a2)		;anispdst
	clr.w	MB*26(a2)			;do ani lo-hi next
	bra	ani4	
ani2lr2	move.w	MB*36(a2),MB*38(a2)		;reset to ani hi frame	
	bra	ani4

;animate from ani lo to ani hi
anilohi	move.w	MB*38(a2),d3	;curent frame
	cmp.w	MB*36(a2),d3 		;= last frame ?
	beq	ani3
	addq.w	#1,MB*38(a2)		;next frame
	bsr	playanimsfx
	rts
ani3	cmp.w	#2,MB*24(a2)	;lo to hi to lo ?
	bne	ani3a
	clr.w	MB*18(a2)			;anispdst
	move.w	#1,MB*26(a2)		;do ani hi-lo next
	bra	ani4
ani3a	cmp.w	#1,MB*22(a2)	;last anictr ? 
	beq	ani5
	move.w	MB*34(a2),MB*38(a2)	;reset to 1st bob frame

ani4	cmp.w	#2,MB*24(a2)	;2=handled by anihilo
	beq	aniend					
ani5	tst.w	MB*20(a2)		;anim ctronoff
	beq	aniend					;ctr disabled
	subq.w	#1,MB*22(a2)
	bne	aniend
	clr.w	MB*14(a2)				;switch off anim	
	move.w	MB*20(a2),MB*22(a2)		;reset anim ctr
	move.w	MB*42(a2),MB*40(a2)		;incase AD=3 restore wait
	bsr	playanimsfx
	rts

playanimsfx
	tst.b	MB*46(a2)		;samp num=0 (disabled)
	beq	plansde
	move.w	MB*38(a2),d3	;cur frame
	cmp.w	MB*48(a2),d3	;curframe=target1 ?
	beq	plannow
	cmp.w	MB*50(a2),d3	;curframe=target2 ?
	bne	plansde
plannow	clr.l	d4
	move.b	MB*46+1(a2),d4
	lea	plrsampch0,a4
	move.b	MB*46(a2),(a4,d4)
	rts
plansde	rts


;syncsfx	dcb.w	MB	*46	1stByte=sampnum(0=none) 2nd=chan
;syncsfxp1	dcb.w	MB	*48	synchronise sfx to p1 & p2
;syncsfxp2	dcb.w	MB	*50	


setloop
	move.w	(a1)+,MB*4(a2)		;store loop cotr & point to nxt token
	move.l	a1,MB*12(a3)		;store loop return address
	bra	newtok

rptloop
	cmp.w	#1,MB*4(a2)			;loop ctr = 0 ?
	ble	rptl1					;branch	if <= 1
	subq.w	#1,MB*4(a2)
	move.l	MB*12(a3),a1		;no - get loop start address
rptl1	bra	newtok

nextscreen
	addq.w	#1,curscrn
	move.w	#%0000000110000000,dmacon(a5)	;disable copp+btpl DMA
	bsr	setscreen
	bsr	zerobase
	bsr	setupspecialblocks	;blocks player can interact with	
	clr.b	newscreenflag
	bsr	gettok0
	bsr	swapplotbob
	bsr	waitblit
	bsr	vertblank
	move.w	#%1000000110000000,dmacon(a5)	;enable copp+btpl DMA
	bra	mainloop

prevscreen
	subq.w	#1,curscrn
	move.w	#%0000000110000000,dmacon(a5)	;disable copp+btpl DMA
	bsr	setscreen
	bsr	zerobase
	bsr	setupspecialblocks
	clr.b	newscreenflag
	bsr	gettok0
	bsr	swapplotbob
	bsr	waitblit
	bsr	vertblank
	move.w	#%1000100110000000,dmacon(a5)	;enable copp+btpl DMA
	bra	mainloop

gotoscreen
	move.w	(a1),curscrn
gscrn1	move.w	#%0000000110000000,dmacon(a5)	;disable copp+btpl DMA
	bsr	setscreen
	bsr	zerobase
	bsr	setupspecialblocks	
	clr.b	newscreenflag
	bsr	gettok0
	bsr	platdecode	
	move.w	#WALK,planim
	move.w	#RT1,curframe
	cmp.b	#MR,joy_dir
	beq	gscrn1a
	move.w	#LT1,curframe
gscrn1a	tst.b	onropeflag
	beq	gscrn2
	move.w	#CLIMB,planim
	move.w	#UD1st,curframe
gscrn2	bsr	swapplotbob
	bsr	waitblit
	bsr	vertblank
	move.w	#%1000000110000000,dmacon(a5)	;enable copp+btpl DMA
	bra	mainloop


right	tst.w	MB*8(a2)		;spspeed = 0 ?
	beq	right1					;yes - move sprite
	subq.w	#1,MB*8(a2)
	bra 	nextsprt
right1	move.w	MB*10(a2),MB*8(a2)	;restore spspeed
	move.w	MB*100(a2),d4		;add frame delay/slowdown
	add.w	d4,MB*10(a2)
	move.w	MB*52(a2),d4
	add.w	d4,(a2) 			;add  to sprite x
	btst	#LIFTBIT,MB*16(a3)	;bob a lift ?
	beq	right2			no
	tst.b	liftflag
	beq	right2
	cmp.b	liftflag,d0
	bne	right2
	move.w	MB*52(a2),d4
	add.w	d4,x			;move plr same speed as lift	
right2	move.w	(a1),d3		;target x into d3
	cmp.w	(a2),d3			;cur sp x = target x ?			
	bgt	nextsprt			;no - get next sprite
	addq.l	#2,a1			;yes - point to next token 
	clr.w	MB*6(a2)	   	;clear current sprite token
	bra	nextsprt			;move next sprite

left	tst.w	MB*8(a2)		
	beq	left1			
	subq.w	#1,MB*8(a2)
	bra 	nextsprt
left1	move.w	MB*10(a2),MB*8(a2)
	move.w	MB*100(a2),d4		;add frame delay/slowdown
	add.w	d4,MB*10(a2)
	move.w	MB*52(a2),d4
	sub.w	d4,(a2)				;subtract from sprite x
	btst	#LIFTBIT,MB*16(a3)	;bob a lift ?
	beq	left2					;no
	tst.b	liftflag
	beq	left2
	cmp.b	liftflag,d0
	bne	left2
	move.w	MB*52(a2),d4
	sub.w	d4,x				;move plr same speed as lift	
left2	move.w	(a1),d3			;target x into d3
	cmp.w	(a2),d3				;cur sp x = target x ?			
	blt	nextsprt				;no - get next sprite
	addq.l	#2,a1				;yes - point to next token 
	clr.w	MB*6(a2)	     	;clear current sprite token
	bra	nextsprt				;move next sprite


up	tst.w	MB*8(a2)	
	beq	up1		
	subq.w	#1,MB*8(a2)
	bra 	nextsprt
up1	move.w	MB*10(a2),MB*8(a2)
	move.w	MB*100(a2),d4		;add frame delay/slowdown
	add.w	d4,MB*10(a2)
	move.w	MB*54(a2),d4
	sub.w	d4,MB*2(a2)			;subtract from sprite y
	btst	#LIFTBIT,MB*16(a3)	;bob a lift ?
	beq	up2			no
	tst.b	liftflag
	beq	up2
	cmp.b	liftflag,d0
	bne	up2
	btst.b	#7,hsrh+3
	bne	up2
	btst.b	#7,hslh+3
	bne	up2
	move.w	MB*54(a2),d4
	sub.w	d4,y				;move plr same speed as lift	
up2	move.w	(a1),d3			
	cmp.w	MB*2(a2),d3		
	blt	nextsprt		
	addq.l	#2,a1			
	clr.w	MB*6(a2)		        
	bra	nextsprt		
	

down	tst.w	MB*8(a2)	
	beq	down1		
	subq.w	#1,MB*8(a2)
	bra 	nextsprt
down1	move.w	MB*10(a2),MB*8(a2)
	move.w	MB*100(a2),d4		;add frame delay/slowdown
	add.w	d4,MB*10(a2)
	move.w	MB*54(a2),d4
	add.w	d4,MB*2(a2)			;add 1 to sprite y
	btst	#LIFTBIT,MB*16(a3)	;bob a lift ?
	beq	down2					;no
	tst.b	liftflag
	beq	down2
	cmp.b	liftflag,d0
	bne	down2
	btst.b	#7,hsrf+3			;check solid at right foot
	bne	down2
	btst.b	#7,hslf+3			;check solid at left foot
	bne	down2
	move.w	MB*54(a2),d4
	add.w	d4,y				;move plr same speed as lift	
down2	move.w	(a1),d3			
	cmp.w	MB*2(a2),d3		
	bgt	nextsprt		
	addq.l	#2,a1			
	clr.w	MB*6(a2)		        
	bra	nextsprt		

				
	
reset	move.l	MB*4(a3),a1		;transfer spptrst to cur spptr
	bra	newtok	


;***********************************************************************
;*
;*			BLITTER   ROUTINES
;*
;**********************************************************************

;CHIP REGISTERS

intena	=	$9a
intenar	=	$1c			;read
intreq	=	$9c
intreqr	=	$1e			;read
dmacon	=	$96
dmaconr	=	$2			;read
color00	=	$180
vhposr	=	$6
vposr	=	$4
adkcon	=	$9e
adkconr	=	$10

;AUDIO REGISTERS

aud0lch	=	$a0
aud1lch	=	$b0
aud2lch	=	$c0
aud3lch	=	$d0
aud0len	=	$a4
aud1len	=	$b4
aud2len	=	$c4
aud3len	=	$d4
aud0vol	=	$a8
aud1vol	=	$b8
aud2vol	=	$c8
aud3vol	=	$d8
aud0per	=	$a6
aud1per	=	$b6
aud2per	=	$c6
aud3per	=	$d6

;COPPER REGISTERS

cop1lc	=	$80
cop2lc	=	$84
copjmp1	=	$88
copjmp2	=	$8a

;BITPLANE REGISTERS

bplcon0	=	$100
bplcon1	=	$102
bplcon2	=	$104
bpl1pth	=	$0e0
bpl1ptl	=	$0e2
bpl2pth	=	$0e4
bpl2ptl	=	$0e6
bpl1mod	=	$108
bpl2mod	=	$10a
diwstrt	=	$08e
diwstop	=	$090
ddfstrt	=	$092
ddfstop	=	$094

;BLITTER REGISTERS

bltcon0	=	$40
bltcon1	=	$42
bltcpth	=	$48
bltcptl	=	$4a
bltbpth	=	$4c
bltbptl	=	$4e
bltapth	=	$50
bltaptl	=	$52
bltdpth	=	$54
bltdptl	=	$56
bltcmod	=	$60
bltbmod	=	$62
bltamod	=	$64
bltdmod	=	$66
bltsize	=	$58
bltcdat	=	$70
bltbdat	=	$72
bltadat	=	$74
bltafwm	=	$44
bltalwm	=	$46

ciaapra	=	$bfe001
	



waitblit
	btst	#14,dmaconr(a5)			;wait 2* for bug in old chip
wblt1	btst	#14,dmaconr(a5)		;blitter busy ?
	bne	wblt1						;loop back if it is
	rts


vertblank
	move.l	vposr(a5),d2
	and.l	#$0001ff00,d2
	cmp.l	#$00010000,d2			;wait on line 256 or greater
	blt	vertblank
	rts



plotbobs
	moveq	#0,d6
	moveq	#0,d5
	move.w	num_bobs,d6		;plots highest to lowest bob num -
	beq	nxtbob				;thus bob 0 (player) has top priority       	
	sub.w	#1,d6
	bsr	bltbobconstP
tstbob	move.l	#x,a1		;bob x base
	move.w	d6,d5			;,,,3,2,1,0
	mulu.w	#2,d5			;,,,6,4,2,0 (form ptr)	
	add.l	d5,a1			;point to correct bobxy
	move.l	d5,d4
	add.l	#curframe,d5	;point to current frame
	move.l	d5,a0
	move.w	(a0),d5			;current frame to d5
	lsl.w	#2,d5			;form Lword ptr -multiply by 4
	lea	fnumconvert,a4		;frame address table
	move.l	(a4,d5),d4		;target address into d4
	move.l	d4,d5
	add.l	#(40*BobscrnY*4),d4		;mask address
	move.l	d5,bobd
	move.l	d4,maskd
	bsr	blit_bob			;d6 holds current bob
nxtbob	dbra	d6,tstbob
	rts

blit_bob
	move.l	destscptr,d2	;current bitplane pointer
	moveq	#0,d1	
	move.l	d1,d0
	move.w	(a1),d0			;x into d0
	divu.w	#16,d0			;form word ptr for screen (0-19)
	move.l	d0,d4
	asl.w	d4
	move.w	MB*2(a1),d1		;y into d1
	mulu.w	#scwidth,d1
	add.w	d4,d1			;d1 points to correct x/y point
	move.l	d1,xyptr
	add.l	d1,d2			;d2 now points to correct byte in screen
	swap	d0	
	and.l	#$f,d0			;get bits 0-3
	ror.w	#4,d0			;move to bits 12-15	
	move.w	d0,tmp1w
	or.w	#$0fca,d0		;0fc2
	move.w	d0,tmp2w


;**** BLIT BOB ONTO LOGICAL (HIDDEN) SCREEN ******

	bsr 	waitblit
	move.w	tmp1w,bltcon1(a5)		;shift for source B
	move.w	tmp2w,bltcon0(a5)		;shift + Lf code for source A	
	move.l	bobd,bltbpth(a5)		;bob data start 
	move.l	maskd,bltapth(a5)		;mask data  "
	move.l	d2,bltcpth(a5)			;back gfx   "
	move.l	d2,bltdpth(a5)			;back gfx   "
	move.w	#(16*64*planes)+bwidth4/2,bltsize(a5)
	rts
bltbobconstP
	bsr 	waitblit
	move.l	#$ffff0000,bltafwm(a5)
	move.w	#40-2-2,bltamod(a5)		;mask modulo
	move.w	#40-2-2,bltbmod(a5)		;bob modulo
	move.w	bmod,bltcmod(a5)		;back gfx  "
	move.w	bmod,bltdmod(a5)		;dest gfx  "
	rts


;this routine restores the background under all active bobs 
;...restoring from LOWEST bob to HIGHEST bob (opposite to plot bobs)

restorebackgrnd
restb1	moveq	#0,d6
	moveq	#0,d5
	bsr	bltbobconstR
tstbob2	move.w	d6,d5		;,,,3,2,1,0
	move.l	bobxyptr,a1
	mulu.w	#2,d5			;,,,16,8,4,0 (form ptr)	
	add.l	d5,a1			;point to correct xy table
	bsr	dorestore			;d6 holds current bob
nxtbob2	addq.w	#1,d6
	cmp.w	num_bobs,d6
	bne	tstbob2
	rts

dorestore
	move.l	destscptr,d2	;current bitplane pointer
	moveq	#0,d0	
	moveq	#0,d1
	move.w	(a1),d0			;x into d0
	divu.w	#16,d0
	move.l	d0,d4
	asl.w	d4
	move.w	MB*2(a1),d1		;y into d1
	mulu	#scwidth,d1
	add.w	d4,d1			;d1 points to correct x/y point
	move.l	d1,d5
	add.l	#restscreen,d5	
	add.l	d1,d2			;d2 now points to correct byte in screen

	bsr	waitblit
	move.l	d2,bltdpth(a5)		;= dest screen
	move.l	d5,bltapth(a5)		;= source screen
	move.w	#(16*64*planes)+bwidth4/2,bltsize(a5)
	rts
bltbobconstR
	bsr	waitblit
	move.l	#-1,bltafwm(a5)
	move.w	bmod,bltamod(a5)	;source modulo 
	move.w	bmod,bltdmod(a5)	;dest modulo
	move.w	#0,bltcon1(a5)		;normal mode
	move.w	#%0000100111110000,bltcon0(a5)   	;A=D , enable A,D
	rts	


plotreplaceblock
	bsr	bltB1const
	move.l	crunchscptr,a1
	lea	entryscreen,a4
	moveq	#0,d0
	move.l	#numblocks-1,d1
plotRLP	btst.b	#READY,numblocks*2(a1)
	bne	repblk
plotR1	addq.l	#1,a1			;next block
	addq.l	#1,d0				;next screen pos of block
	dbra	d1,plotRLP	
plotRE	rts			
repblk	move.l	destscptr,a3
	move.l	d0,d3				;save d0
	move.l	d0,d5
	lsl	#4,d5					;*16
	lsl	#2,d0					;*4 (form Lword ptr)
	lea	plotoffset,a2
	add.l	(a2,d0),a3			;set plot position
	move.l	a3,dptrtmp
	move.l	(a2,d0),d4			;plot offset in d4
	moveq	#0,d2
	lea	bnumconvert,a2			;block address table
	move.b	(a1),d2				;get crunch block
	btst.b	#COLCRMSK,numblocks(a1)		;replace block=crunch/mask?
	bne	repblkA	
	move.b	12(a4,d5),d2		;get replace block
repblkA	lsl	#2,d2				;form Lword ptr -multiply by 4
	bsr	blitblock1				;plot to screen 1
	lea	restscreen,a3
	add.l	d4,a3
	bsr	blitblock1

	btst.b	#USEMASK,numblocks*2(a1)
	beq	repblk0
	moveq	#0,d2
	move.b	10(a4,d5),d2				;get block number to be masked
	btst.b	#COLCRMSK,numblocks(a1)		;replace block=crunch/mask?
	beq	repblkB
	move.b	12(a4,d5),d2
	beq	repblk0						;repblk=0 - plot crunch only
repblkB	lsl	#2,d2
	btst.b	#MASKTYPE,numblocks*2(a1)
	beq	repblkC
	bsr	bltCMconst
	bsr	generatemask
	bsr	blitblockwCmask				;plot to restscreen 
	move.l	dptrtmp,a3		   		;"	destscreen	
	bsr	blitblockwCmask
	bra	repblk0	
repblkC	bsr	bltOMconst
	bsr	blitblockwOmask
	move.l	dptrtmp,a3
	bsr	blitblockwOmask

repblk0	cmp.l	#screen2,destscptr		;drawn block on both screens ?
	bne	repblk1							;- no
	bclr.b	#READY,numblocks*2(a1)		;- yes - don't plot again
repblk1	move.l	d3,d0
	bra	plotR1




copyscreen
	bsr	waitblit
	move.l	destscptr,bltdpth(a5)		;dest=bitplane
	move.l	visibleptr,bltapth(a5)		;src A = bitmapped gfx data
	clr.w	bltamod(a5)					;no source modulo
	clr.w	bltdmod(a5)					;no dest modulo
	clr.w	bltcon1(a5)					;normal mode
	move.l	#-1,bltafwm(a5)
	move.w	#$09f0,bltcon0(a5)			;copy A to D
	move.w	#%0000000000011010,bltsize(a5) 	;1024*26(w) (4 bitplanes)
	rts

BltB1const
	bsr	waitblit
	move.l	#-1,bltafwm(a5)
	move.w	#bscwidth-2,bltamod(a5)		;source modulo
	move.w	#scwidth-2,bltdmod(a5)
	move.w	#0,bltcon1(a5)				;normal mode
	move.w	#$09f0,bltcon0(a5)			;copy A to D
	move.l	#-1,bltafwm(a5)
	rts
blitblock1
	bsr	waitblit
	move.l	(a2,d2),bltapth(a5)			;srcA = block gfx 
	move.l	a3,bltdpth(a5)				;dest=bitplane
	move.w	#(blockHtp*64*planes)+blockWdp/2,bltsize(a5)
	rts


bltOMconst
	bsr	waitblit
	move.l	#-1,bltafwm(a5)
	move.w	#bscwidth-2,bltamod(a5)		;block modulo
	move.w	#bscwidth-2,bltbmod(a5)		;block modulo
	move.w	#scwidth-2,bltcmod(a5)		;back gfx  "
	move.w	#scwidth-2,bltdmod(a5)
	move.w	#$0fca,bltcon0(a5)	
	move.w	#0,bltcon1(a5)				;normal mode
	rts
blitblockwOmask
	bsr 	waitblit
	move.l	4(a2,d2),bltapth(a5)		;mask gfx
	move.l	(a2,d2),bltbpth(a5)			;block gfx
	move.l	a3,bltcpth(a5)				;dest gfx
	move.l	a3,bltdpth(a5)				;dest gfx
	move.w	#(blockHtp*64*planes)+blockWdp/2,bltsize(a5)
	rts
blitblockwOmask2
	bsr 	waitblit
	move.l	(a2,d4),bltapth(a5)			;mask gfx
	move.l	(a2,d2),bltbpth(a5)			;block gfx
	move.l	a3,bltcpth(a5)				;dest gfx
	move.l	a3,bltdpth(a5)				;dest gfx
	move.w	#(blockHtp*64*planes)+blockWdp/2,bltsize(a5)
	rts

bltCMconst
	bsr 	waitblit
	move.l	#-1,bltafwm(a5)
	move.w	#0,bltamod(a5)				;source modulo
	move.w	#bscwidth-2,bltbmod(a5)		;block modulo
	move.w	#scwidth-2,bltcmod(a5)		;back gfx  "
	move.w	#$0fca,bltcon0(a5)	
	move.w	#0,bltcon1(a5)				;normal mode
	rts
blitblockwCmask
	bsr 	waitblit
	move.l	#maskbuffer,bltapth(a5)		;mask gfx
	move.l	(a2,d2),bltbpth(a5)			;block gfx
	move.l	a3,bltcpth(a5)				;dest gfx
	move.l	a3,bltdpth(a5)				;dest gfx
	move.w	#(blockHtp*64*planes)+blockWdp/2,bltsize(a5)
	rts




;*********************************************************************
;*********************************************************************




readkeyboard				
	moveq	#0,d0
	move.b	$bfec01,d0		
	not 	d0
	ror.b	#1,d0
	move.b	d0,key
	cmp.b	#$19,d0				;P-pause
	bne	readKB2
	move.b	#1,pause
	bra	readKBe
readKB2	cmp.b	#$18,d0			;O-pause
	bne	readKBe
	clr.b	pause
readKBe	rts	



readjoystick
	move.b	transpflag,transpflagst
	move.b	firebutflag,firebutflagst
	clr.b	transpflag
	clr.b	firebutflag
	clr.b	joy_dir
	move.w	$dff00c,d0			;joy1data to d0
	btst	#1,d0	
	beq	jstk2
	tst.b	ciaapra
	bmi	jright
	move.b	#MJR,joy_dir
	rts
jstk2	btst	#9,d0
	beq	jstk3
	tst.b	ciaapra
	bmi	jleft
	move.b	#MJL,joy_dir
	rts
jstk3	move.w	d0,d1
	lsr.w	#1,d1
	eor.w	d0,d1
	btst	#0,d1
	beq	jstk4
	tst.b	ciaapra
	bmi	jdown
	move.b	#MJD,joy_dir
	rts
jstk4	btst	#8,d1
	beq	jstk5
	tst.b	ciaapra
	bmi	jup
	move.b	#MJU,joy_dir
	rts
jstk5	tst.b	ciaapra
	bmi	jstk6
jstk6	tst.b	ciaapra
	bmi	jstkE
	move.b	#F,joy_dir
	clr.b	pause
	move.b	firebutflagst,firebutflag
	rts
jstkE	clr.b	Jcentreflag
	rts							;jstk at centre if joy_dir = 0	


jright	move.b	#MR,joy_dir
;	bra	jstk3
	rts
jleft	move.b	#ML,joy_dir
;	bra	jstk3
	rts
jup	cmp.b	#MR,joy_dir
	bne	jup1
	move.b	#MUR,joy_dir
	rts
jup1	cmp.b	#ML,joy_dir
	bne	jup2
	move.b	#MUL,joy_dir
	rts	
jup2	move.b	#MU,joy_dir
	move.b	transpflagst,transpflag
	rts
jdown	move.b	#MD,joy_dir
	rts





;***********************************************************************
;*
;*			PLATFORM DECODER ROUTINES
;*
;**********************************************************************


HSR	=	38				;41right	x	decrease to move HS to left	
HSL	=	44				;47left	x	increase  "  	"      left
HSH	=	4*4				;head	y	increase to move HS down
HSF	=	16*4			;foot	y


HSRR	=	33			;right	x	decrease to move HS to left	
HSLR	=	HSRR+15		;left 	x
HSHR	=	4*4			;head	y	increase to move HS down
HSFR	=	16*4		;foot	y

HSCLR	=	41			;block collision
HSCHF	=	8*4			;hot-spots


PLATDECODE
	cmp.w	#828,y
	bgt	fallER
	bsr	convertxy
	bsr	animateplayer
	clr.b	surfaceflag
	clr.b	fallflag
	tst.b	liftflag
	bne	platJ
	tst.b	jumpflag
	bne	dojump
	bsr	chkblock
platJ	bsr	chkjoystick
	bsr	slideplr
	bsr	resetanim
platE	rts
fallER	bsr	looselife
	rts	



chkblock		;on top or inside block ?
	cmp.b	#ON,ontopblock
	bne	chkINblock

;- on top of block. Add `ACTION` blocks here e.g. conv,etc
chkONblock	
	clr.b	onconvflag
	lea	entryscreen,a0
	moveq	#0,d0
	cmp.b	#CVR,hsrf+2			;conveyor under right foot ?
	bne	chkblk0					;no - check left foot
	move.w	blockposrf,d0
	lsl	#4,d0					;*16 - point to entry
	btst.b	#0,5(a0,d0)			;yes - get conv direcion (R/L)
	bne	conveyorLr				;left conv under right foot
	beq	conveyorRr				;right "		"	"
chkblk0	cmp.b	#CVR,hslf+2		;conveyor under left foot ?
	bne	chkblk1					;no 
	clr.w	planim				;animation = OFF
	move.w	blockposlf,d0
	lsl	#4,d0					;*16 - point to entry
	btst.b	#0,5(a0,d0)			;yes - get conv direcion (R/L)
	bne	conveyorLl
	beq	conveyorRl

;add non-action blocks here
;*** important *** -do all `air` (rope/ladd,etc) chks before `ground` chks 
chkblk1	
	move.w	#1,testnum1
	bsr	onladder	
	bsr	onrope
	bsr	onice
	bsr	onpipe
	bsr	ongrnd
	tst.b	onladdflag
	bne	chkblkE
	tst.b	onropeflag
	bne	chkblkE
	tst.b	oniceflag
	bne	chkblkE
	tst.b	onpipeflag
	bne	chkblkE
	tst.b	ongrndflag
	bne	chkblkE
	bra	fall

chkblkE	tst.b	falling		
	beq	chkbkEb
	tst.b	maxfallflag
	beq	chkblk2
	btst.b	#GRNDBIT,hslfB2
	bne	ckGndY
	btst.b	#GRNDBIT,hsrfB2
	bne	ckGndY
	cmp.b	#LAD,hsrf+2			;fell onto a rope/ladd ?
	beq	fall					;-yes-	continue falling until plr hits
	cmp.b	#ROPE,hsrf+2		;ground or screen bottom
	beq	fall
ckGndY	clr.b	falling
	clr.b	maxfallflag
	clr.b	maxfallctr
	bsr	looselife
	rts
chkblk2	tst.b	onladdflag		;fell onto a rope/ladd ?
	bne	chkbkrl
	tst.b	onropeflag
	beq	chkbk2e					;- no (landed on something else)
chkbkrl	tst.b	surfaceflag	
	bne	chkbk2e					;fell onto rope+grnd/ice/etc
	cmp.b	#MU,joy_dir			;- yes
	beq	chkbrlY
	cmp.b	#MUL,joy_dir
	beq	chkbrlY
	cmp.b	#MUR,joy_dir
	beq	chkbrlY
	bne	fall

chkbk2e	move.b	#15,plrsampch0
chkbrlY	clr.b	falling
chkbkEb	clr.b	maxfallctr
	rts



chkINblock
	bsr	onladder
	tst.b	onladdflag
	bne	chkblkE
	bsr	onrope
	tst.b	onropeflag
	bne	chkblkE
	bra	fall


slideplr
	tst.b	oniceflag
	beq	slidee

	cmp.b	#MR,icedir
	bne	slide1
	cmp.b	#MR,joy_dir
	beq	slidex
	bne	slideY
slide1	cmp.b	#ML,icedir
	bne	slideY
	cmp.b	#ML,joy_dir
	beq	slidex
	bne	slideY	
		
slideY	tst.b	icectr
	beq	slidepe	
	subq.b	#1,icectr
	move.l	#inertiatab,a0
	clr.l	d0
	move.b	icectr,d0			;0-19
	tst.b	(a0,d0)
	beq	slidex					;0=don't move
	cmp.b	#MR,icedir
	bne	slidepL
	btst.b	#7,hsrh1+3			;check solid at right head		
	bne	slidee
	cmp.w	#XMAX,x
	bge	slidee
	move.w	#R1st+1,curframe
	mvePLright
	rts
slidepL	btst.b	#7,hslh1+3		;check solid at left head		
	bne	slidee
	cmp.w	#XMIN,x
	ble	slidee
	move.w	#L1st+2,curframe
	mvePLleft
slidex	rts
slidee	clr.b	icectr
slidepe	clr.b	icedir
	rts


resetanim
	tst.b	joy_dir
	bne	rsetanE
	tst.b	icectr				;plr sliding ?
	bne	rsetanE
	cmp.w	#WALK,planim
	bne	rsetanC
	tst.w	rsetanisp
	beq	rsetan1
	subq.w	#1,rsetanisp
	rts
rsetan1	move.w	#RSETSP,rsetanisp
	cmp.w	#R1st,curframe
	blt	rsetL
	cmp.w	#RLst,curframe
	bgt	rsetL
rsetR	cmp.w	#RT1,curframe
	beq	rsetanE
	cmp.w	#RT2,curframe
	beq	rsetanE
	bsr	anplWRS		
	rts
rsetL	cmp.w	#L1st,curframe
	blt	rsetanC
	cmp.w	#LLst,curframe
	bgt	rsetanC
	cmp.w	#LT1,curframe
	beq	rsetanE
	cmp.w	#LT2,curframe
	beq	rsetanE
	bsr	anplWLS
rsetanE	rts
rsetanC	cmp.w	#UD1st,curframe
	blt	rsetanE
	cmp.w	#UDLst,curframe
	bgt	rsetanE
	tst.b	onladdflag				;fallen off bottom lad/rope onto grnd ?
	bne	rsetaC1						;-both must=0
	tst.b	onropeflag
	beq	rsetaCY
rsetaC1	cmp.b	#MD,onladdflag		;at top of lad/rope ?
	beq	rsetaCY
	cmp.b	#MD,onropeflag
	bne	rsetanE
rsetaCY	move.w	walkdir,curframe
	rts


;both feet must be on ice +/- space/lad/rop to allow sliding
onice	clr.b	oniceflag
	cmp.b	#ICE,hslf+2			;ice under left foot ?
	beq	oniceL
	btst.b	#ICEbit,hslfc
	bne	oniceL	
	cmp.b	#ICE,hsrf+2			;ice under right foot ?	
	beq	oniceR
	btst.b	#ICEbit,hsrfc
	bne	oniceR	
oniceE	;clr.b	icectr
	;clr.b	icedir
	rts
oniceL	cmp.b	#ICE,hsrf+2		;ice under right foot ?
	beq	oniceY
	btst.b	#ICEbit,hsrfc
	bne	oniceY	
	cmp.b	#LAD,hsrf+2
	beq	oniceY
	btst.b	#LADbit,hsrfc
	bne	oniceY	
	cmp.b	#ROPE,hsrf+2
	beq	oniceY
	btst.b	#ROPEbit,hsrfc
	bne	oniceY	
	tst.b	hsrf+2				;space under right foot ? 
	beq	oniceY
	bne	oniceE
oniceR	cmp.b	#ICE,hslf+2		;ice under left foot ?
	beq	oniceY
	btst.b	#ICEbit,hslfc
	bne	oniceY	
	cmp.b	#LAD,hslf+2
	beq	oniceY
	btst.b	#LADbit,hslfc
	bne	oniceY	
	cmp.b	#ROPE,hslf+2
	beq	oniceY
	btst.b	#ROPEbit,hslfc
	bne	oniceY	
	tst.b	hslf+2				;space under right foot ? 
	bne	oniceE
oniceY	move.b	#1,oniceflag
	move.w	#WALK,planim
	move.b	#1,surfaceflag
	rts
	
	
;both feet must be on pipe +/- space/lad/rop to allow player to fall through
onpipe	clr.b	onpipeflag
	cmp.b	#PIPE,hslf+2
	beq	onpipeL
	cmp.b	#PIPE,hsrf+2
	beq	onpipeR
onpipeE	rts
onpipeL	cmp.b	#PIPE,hsrf+2
	beq	onpipeY
	cmp.b	#LAD,hsrf+2
	beq	onpipeY
	cmp.b	#ROPE,hsrf+2
	beq	onpipeY
	tst.b	hsrf+2
	beq	onpipeY
	bne	onpipeE
onpipeR	cmp.b	#PIPE,hslf+2
	beq	onpipeY
	cmp.b	#LAD,hslf+2
	beq	onpipeY
	cmp.b	#ROPE,hslf+2
	beq	onpipeY
	tst.b	hslf+2
	bne	onpipeE
onpipeY	tst.b	joy_dir
	beq	onpipeE
	move.b	#1,onpipeflag
	move.w	#WALK,planim
	move.b	#1,surfaceflag
	rts


ongrnd	clr.b	ongrndflag	
	btst.b	#GRNDBIT,hslfB2
	bne	ongrndY
	btst.b	#GRNDBIT,hsrfB2
	beq	ongrndE
ongrndY	move.b	#1,ongrndflag
	move.w	#WALK,planim
	move.b	#1,surfaceflag
ongrndE	rts

onspace	clr.b	onspaceflag	
	tst.b	hslf+2
	bne	onspcE
	tst.b	hsrf+2
	bne	onspcE
	move.b	#1,onspaceflag
	clr.b	surfaceflag
onspcE	rts


onconv	clr.b	onconvflag	
	cmp.b	#CVR,hslf+2
	beq	onconvY
	cmp.b	#CVR,hsrf+2
	bne	onconvE
onconvY	move.b	#1,onconvflag
	move.w	#WALK,planim
	move.b	#1,surfaceflag
onconvE	rts



;a bit more work this one - check for ladder above,below or both, check
;for solid at left and right head & foot
;use rope hot-spots if jumping L/R
onladder
	clr.b	onladdflag
	tst.b	jumpflag
	beq	onlada	
	cmp.b	#MU,jumpflag
	beq	onlada
	cmp.b	#LAD,hsrrh+2
	bne	onladE
	cmp.b	#LAD,hsrlh+2
	bne	onladE
	beq	onladUD
onlada	cmp.b	#LAD,hsrf+2			;chk ladder here - BOTH feet
	bne	Unlad						;must be on ladder 
	cmp.b	#LAD,hslf+2
	bne	Unlad
	cmp.b	#LAD,hsrh+2				;check right head only
	beq	onladUD	
	btst.b	#LADbit,hsrhc			;check right head only
	bne	onladUD	
onlad0	move.b	#MD,onladdflag		;both feet on ladder - can move down 
	cmp.b	#ON,ontopblock
	beq	onladD	
onladUD	move.b	#MUD,onladdflag		;can move up/down		
	clr.b	oniceflag
onladU	move.w	#CLIMB,planim
onladE	rts
onladD	clr.b	oniceflag
onladE1	move.w	#WALK,planim		;ground at feet
	rts
Unlad	btst.b	#LADbit,hsrfc		;chk CombBtype Lad bit now 
	beq	Unlad1			
	btst.b	#LADbit,hslfc
	beq	Unlad1
	cmp.b	#LAD,hsrh+2				;check right head only
	beq	onladUD	
	btst.b	#LADbit,hsrhc			;check right head only
	bne	onladUD	
	bra	onlad0

Unlad1	cmp.b	#LAD,hsrh+2			;check right head
	beq	Unlad2
	btst.b	#LADbit,hsrhc
	beq	onladE
Unlad2	cmp.b	#LAD,hslh+2			;check left head
	beq	Unlad3
	btst.b	#LADbit,hslhc
	beq	onladE
Unlad3	move.b	#MU,onladdflag		;can move up
	cmp.b	#ON,ontopblock
	bne	onladU						;feet inside block	
	btst.b	#GRNDBIT,hsrfB2			;ground	at feet ?
	bne	onladE1
	btst.b	#GRNDBIT,hslfB2
	bne	onladE1
	beq	onladU						;no - on ladder 


onrope	clr.b	onropeflag
	clr.b	onropeflagofst
	btst.b	#ROPEbit,hsrrfc
	bne	onrp1	
	cmp.b	#ROPE,hsrrf+2			;here we make sure that both
	bne	Unrope						;right & left foot hot-spots are
onrp1	btst.b	#ROPEbit,hsrlfc		;within the rope block	
	bne	onrp2
	cmp.b	#ROPE,hsrlf+2
	bne	Unrope
onrp2	move.b	#MD,onropeflag
	btst.b	#ROPEbit,hsrrhc			;is block above rope ?
	bne	onropUD
	cmp.b	#ROPE,hsrrh+2
	beq	onropUD
	cmp.b	#ON,ontopblock
	beq	onropeE				
onropUD	move.b	#MUD,onropeflag		
onropeE	move.w	blockposrrf,d0		;ensure L+R rope = same block posn
	cmp.w	blockposrlf,d0
	bne	onropNO
	move.w	#CLIMB,planim
	rts
onropNO	clr.b	onropeflagofst
	clr.b	onropeflag
	rts

Unrope	btst.b	#ROPEbit,hsrrhc
	bne	Unrpc
	cmp.b	#ROPE,hsrrh+2
	bne	unropE2
Unrpc	btst.b	#ROPEbit,hsrlhc
	bne	Unrpd
	cmp.b	#ROPE,hsrlh+2
	bne	unropE2
Unrpd	move.w	blockposrrf,d0
	cmp.w	blockposrlf,d0		
	bne	unropE2
	move.b	#MU,onropeflag		
	tst.b	hsrrf+2					;if space at feet
	bne	unropE1						;then allow U/D
	tst.b	hsrlf+2
	beq	onropUD	
unropE1	cmp.b	#ON,ontopblock
	beq	unrpE1a				
	move.b	#MUD,onropeflag		
	move.w	#CLIMB,planim		
	rts
unrpE1a	move.w	#WALK,planim		
	rts

unropE2	cmp.b	#ROPE,midHbtype
	beq	unropE3
	btst.b	#ROPEbit,midHcbtype
	bne	unropE3
	cmp.b	#ROPE,midFbtype
	beq	unRpE2a
	btst.b	#ROPEbit,midFcbtype
	beq	unrpE
unrpE2a	move.b	#MD,onropeflagofst
	bra	unropE4
unropE3	move.b	#MU,onropeflagofst
unropE4	cmp.b	#ON,ontopblock
	beq	unRpE5				
	move.b	#MUD,onropeflagofst
unRpE	rts
unRpE5	move.w	#WALK,planim
	rts


dojump	bsr	landed1
	tst.b	d0
	bne	dojumpE
	tst.b	jfallflag			;- moving up or down ?
	beq	dojump1					;- up
	bsr	landed2					;- down
	tst.b	d0		
	bne	dojumpE
dojump1	cmp.b	#MU,jumpflag
	beq	jumpUP
	cmp.b	#ML,jumpflag
	beq	jumpleft
	cmp.b	#MR,jumpflag
	beq	jumpright
;	rts
dojumpE	clr.b	jumpflag
	clr.b	jfallflag
	tst.b	onropeflag
	bne	dojmpEb
	tst.b	onladdflag
	bne	dojmpEb
	move.b	#15,plrsampch0
	rts
dojmpEb	move.b	#8,plrsampch0
	rts


landed1	cmp.b	#MU,joy_dir				;must be pushing forward/diag to land on
	beq	landa							;a rope or ladder
	cmp.b	#MUL,joy_dir
	beq	landa
	cmp.b	#MUR,joy_dir
	bne	landE1
landa	bsr	onladder	
	tst.b	onladdflag
	bne	landE3
	bsr	onrope
	tst.b	onropeflag
	bne	landE3
	cmp.b	#MU,jumpflag
	bne	landE1
	cmp.b	#MU,onropeflagofst
	beq	landb
	cmp.b	#MUD,onropeflagofst
	bne	landE1
landb	move.b	#MU,onropeflag
	moveq	#0,d0
	move.b	colblk019,d0
	asl.w	#4,d0
	add.w	#centreXoffset,d0
	move.w	d0,x
	clr.b	oniceflag
	rts
landE1	moveq	#0,d0
	rts	
landed2	cmp.b	#ON,ontopblock		
	bne	landE1
	clr.b	onladdflag
	clr.b	onropeflag
	bsr	onice
	tst.b	oniceflag
	bne	landE2
	bsr	onpipe
	tst.b	onpipeflag
	bne	landE2
	bsr	ongrnd
	tst.b	ongrndflag
	bne	landE2
	bsr	onconv
	tst.b	onconvflag
	beq	landE1
landE2	moveq	#1,d0
	rts	
landE3	moveq	#1,d0
	move.w	#CLIMB,planim
	move.w	#UD1st,curframe
	rts	




animateplayer
	tst.b	fallflag
	beq	anpl0
	cmp.w	#WALK,planim			;allow R/L WALK anim when falling
	bne	anplE
anpl0	tst.w	anispeed
	beq	anpl1
	subq.w	#1,anispeed
anplE	rts
anpl1	move.w	#PLANISP,anispeed
	cmp.w	#CLIMB,planim
	beq	anplC
	cmp.w	#WALK,planim
	beq	anplW	
	rts

anplC	cmp.w	#MU,jumpflag
	beq	anplCE
	tst.b	joy_dir
	bne	anplC0
	rts
anplC0	tst.b	onropeflag		
	beq	anplC2
	cmp.b	#MD,onropeflag
	bne	anplC1		
	cmp.b	#MU,joy_dir					;at top of rope & jstk forward
	beq	anplCE							;- don't animate
anplC1	cmp.b	#MU,joy_dir			
	beq	anplC2							;only jstk U/D allows animation
	cmp.b	#MD,joy_dir
	bne	anplCE
anplC2	cmp.w	#UD1st,curframe			;make sure we are in range
	blt	anplCI							;- if not then initialise to
	cmp.w	#UDLst,curframe				;#UD1st
	ble	anplCS
anplCI	move.w	#UD1st,curframe			
anplCS	cmp.w	#UDLst,curframe
	beq	anplC3
	addq.w	#1,curframe
	rts
anplC3	move.w	#UD1st,curframe
anplCE	rts

anplW	cmp.b	#ML,joy_dir
	beq	anplWL
	cmp.b	#MR,joy_dir
	beq	anplWR
	cmp.b	#MJL,joy_dir
	beq	anplWL
	cmp.b	#MJR,joy_dir
	beq	anplWR
	cmp.b	#MR,jumpflag
	beq	anplWR
	cmp.b	#ML,jumpflag
	beq	anplWL
	rts
anplWR	cmp.b	#ML,icedir
	beq	anplE
	cmp.w	#R1st,curframe
	blt	anplWRI
	cmp.w	#RLst,curframe
	ble	anplWRS
anplWRI	move.w	#RT1,curframe			
	move.w	#RT1,walkdir
	rts
anplWRS	cmp.w	#RLst,curframe
	beq	anplWR2
	addq.w	#1,curframe
	move.w	#RT1,walkdir
	bsr	chksfx
	rts
anplWR2	move.w	#R1st,curframe
	move.w	#RT1,walkdir
	bsr	chksfx
	rts

anplWL	cmp.b	#MR,icedir
	beq	anplE	
	cmp.w	#L1st,curframe
	blt	anplWLI
	cmp.w	#LLst,curframe
	ble	anplWLS
anplWLI	move.w	#LT1,curframe			
	move.w	#LT1,walkdir
	rts
anplWLS	cmp.w	#LLst,curframe
	beq	anplWL2
	addq.w	#1,curframe
	move.w	#LT1,walkdir
	bsr	chksfx
	rts
anplWL2	move.w	#L1st,curframe
	move.w	#LT1,walkdir
	bsr	chksfx
	rts


chksfx	tst.b	jumpflag
	bne	chksfxE
	tst.b	fallflag
	bne	chksfxE
	cmp.w	#RT1,curframe
	beq	chksfxP
	cmp.w	#LT1,curframe
	beq	chksfxP
chksfxE	rts
chksfxP	move.b	#15,plrsampch0
	rts

chkjoystick
	tst.b	fallflag
	bne	chkjE
	move.b	joy_dir,d0
	cmp.b	#MR,d0	
	beq	pright
	cmp.b	#ML,d0
	beq	pleft	
	cmp.b	#MU,d0
	beq	pup
	cmp.b	#MD,d0
	beq	pdown
	cmp.b	#MJR,d0
	beq	jumpright
	cmp.b	#MJL,d0
	beq	jumpleft
	cmp.b	#MJU,d0
	beq	jumpUP
;	bsr	slideplr		plr on ice ?
;	bsr	resetanim	
chkjE	rts	




synchsample
	tst.b	jumpflag
	bne	syncspe
	cmp.w	#plsyncsfxp1,curframe
	beq	syncsok	
	cmp.w	#plsyncsfxp2,curframe
	bne	syncspe
syncsok	move.b	#15,plrsampch0
syncspe	rts
	

pright	tst.b	onconvflag
	bne	prightE
	tst.b	oniceflag
	beq	pright0
	cmp.b	#ML,icedir
	beq	prightE
	tst.b	onconvflag
	bne	prightE
pright0	cmp.b	#MUD,onropeflag
	beq	prightE
	cmp.b	#MUD,onropeflagofst
	beq	prightE
	cmp.b	#MUD,onladdflag				;`in` ladder ?
	bne	pright1	
	btst.b	#7,hsrh1+3
	bne	prightE
	btst.b	#7,hsrf1+3
	bne	prightE
	beq	prightM	
pright1	btst.b	#7,hsrh1+3
	bne	prightE
	tst.b	fallflag
	bne	prighE1
	tst.b	jumpflag
	beq	prightM
	btst.b	#7,hsrf1+3
	bne	prightE
prightM	mvePLright		macro
	tst.b	oniceflag
	beq	prightE
	cmp.b	#SLIDEMAX,icectr
	beq	prighM1
	addq.b	#2,icectr
prighM1	move.b	#MR,icedir
prightE	rts
prighE1	move.w	#WALK,planim			;allow R/L anim when falling
	rts

pleft	tst.b	onconvflag
	bne	pleftE
	tst.b	oniceflag
	beq	pleft0
	cmp.b	#MR,icedir
	beq	pleftE
	tst.b	onconvflag
	bne	pleftE
pleft0	cmp.b	#MUD,onropeflag
	beq	pleftE
	cmp.b	#MUD,onropeflagofst
	beq	pleftE
	cmp.b	#MUD,onladdflag
	bne	pleft1	
	btst.b	#7,hslh1+3
	bne	pleftE
	btst.b	#7,hslf1+3
	bne	pleftE
	beq	pleftM	
pleft1	btst.b	#7,hslh1+3
	bne	pleftE
	tst.b	fallflag
	bne	pleftE1
	tst.b	jumpflag
	beq	pleftM
	btst.b	#7,hslf1+3
	bne	pleftE
pleftM	mvePLleft		macro
	tst.b	oniceflag
	beq	pleftE
	cmp.b	#SLIDEMAX,icectr
	beq	pleftM1
	addq.b	#2,icectr
pleftM1	move.b	#ML,icedir
pleftE	rts
pleftE1	move.w	#WALK,planim		;allow R/L anim when falling
	rts

pup	cmp.b	#MUD,onladdflag	
	beq	pupM
	cmp.b	#MU,onladdflag	
	beq	pupM
	cmp.b	#MUD,onropeflag	
	beq	pupM
	cmp.b	#MU,onropeflag	
	beq	pupM
	cmp.b	#MU,onropeflagofst	
	beq	pupM2
	cmp.b	#MUD,onropeflagofst	
	beq	pupM2
	;other checks here ?
	bra	pupe
pupM	btst.b	#7,hslh1+3
	bne	pupe
	btst.b	#7,hsrh1+3
	bne	pupe
	mvePLup		macro
	clr.b	oniceflag
	move.w	#CLIMB,planim
pupe	rts
pupM2	moveq	#0,d0
	move.b	colblk019,d0
	asl.w	#4,d0
	add.w	#centreXoffset,d0
	move.w	d0,x
	move.b	onropeflagofst,onropeflag
	bra	pupM



pdown	cmp.b	#MUD,onladdflag	
	beq	pdownM
	cmp.b	#MD,onladdflag	
	beq	pdownM
	cmp.b	#MUD,onropeflag	
	beq	pdownM
	cmp.b	#MD,onropeflag	
	beq	pdownM
	cmp.b	#MD,onropeflagofst	
	beq	pdownM2
	cmp.b	#MUD,onropeflagofst	
	beq	pdownM2
	;other checks here ?
	bra	pdowne
pdownM	mvePLdn		macro
	move.w	#CLIMB,planim
	clr.b	oniceflag
pdowne	rts
pdownM2	moveq	#0,d0
	move.b	colblk019,d0
	asl.w	#4,d0
	add.w	#centreXoffset,d0
	move.w	d0,x
	move.b	onropeflagofst,onropeflag
	bra	pdownM


conveyorLl		;process leftconv block under left foot
;	testjumpRLU			insert jump r/l/up macro
	btst.b	#7,hslh+3				;check solid left-plspeed foot-4
	bne	convE
	clr.l	d0
	move.w	blockposlf,d0			;block screen position	
	lea	entryscreen,a0		
	lsl	#4,d0		*16
	tst.b	7(a0,d0)
	bne	convLE						;move only when speedst=0	
	mvePLCVleft
convLE	move.w	#WALK,planim
	move.b	#ML,onconvflag
convE	bsr	onrope
	bsr	onladder
	bra	chkblkE						;chk fall status

conveyorLr		;process leftconv block under right foot
;	testjumpRLU			insert jump r/l/up macro
	btst.b	#7,hslh+3
	bne	convE
	clr.l	d0
	move.w	blockposrf,d0
	lea	entryscreen,a0		
	lsl	#4,d0		*16
	tst.b	7(a0,d0)
	bne	convLE						;move only when speedst=0	
	mvePLCVleft
	move.w	#WALK,planim
	move.b	#ML,onconvflag
	bra	convE

conveyorRr		;process rightconv block under right foot
;	testjumpRLU			insert jump r/l/up macro
	btst.b	#7,hsrh+3			;check solid right-plspeed foot-4
	bne	convE
	clr.l	d0
	move.w	blockposrf,d0		;block screen position	
	lea	entryscreen,a0		
	lsl	#4,d0		*16
	tst.b	7(a0,d0)
	bne	convRE					;move only when speedst=0	
	mvePLCVright
convRE	move.w	#WALK,planim
	move.b	#MR,onconvflag
	bra	convE
conveyorRl		;process rightconv block under left foot
;	testjumpRLU			insert jump r/l/up macro
	btst.b	#7,hsrh+3
	bne	convE	
	clr.l	d0
	move.w	blockposlf,d0	
	lea	entryscreen,a0		
	lsl	#4,d0		*16
	tst.b	7(a0,d0)
	bne	convRE					;move only when speedst=0	
	mvePLCVright
	move.w	#WALK,planim
	move.b	#MR,onconvflag
	bra	convE


fall	clr.b	icectr
	clr.b	icedir
	move.b	#1,fallflag
	move.b	#1,falling
	cmp.w	#YMAX,y
	blt	fallY
	bsr	looselife
	rts
fallY	addq.w	#4*PLSPEED,y
	addq.b	#1,maxfallctr
	cmp.b	#MAXFALL,maxfallctr
	bne	fallE
	clr.b	maxfallctr
	move.b	#1,maxfallflag
fallE	rts
	

jumpUP	btst.b	#7,hsrh+3
	bne	jumpUE
	btst.b	#7,hslh+3
	bne	jumpUE
	tst.b	jumpflag				;currently jumping ?
	bne	jumpU1						;yes
	cmp.b	#ON,ontopblock
	beq	jumpU0
	tst.b	liftflag
	bne	jumpU0
	tst.b	onropeflag
	bne	jumpU0
	tst.b	onladdflag
	beq	jumpE
jumpU0	clr.b	icectr
	clr.b	icedir
	clr.b	jfallflag
	clr.b	liftflag
	move.b	#MU,jumpflag		
	move.w	#CLIMB,planim
	move.w	#UD1st,curframe
	move.l	#jumpUDtable+1,a0	
	move.b	(a0),jumpUdelay
	move.l	a0,jumptabptr
jumpU1	tst.b	jumpUdelay			;ready to move ?
	beq	jumpUm						;yes
	subq.b	#1,jumpUdelay			;no
	rts

jumpUm	move.l	jumptabptr,a0
	tst.b	jfallflag				;move up or down ?
	beq	jumpU4						;- up

jumpDn	move.b	#1,Jfallflag
	mvePLdn		macro
	subq.l	#1,a0
	cmp.l	#jumpUDtable-1,a0
	beq	jumpUE
	move.b	(a0),jumpUdelay
	move.l	a0,jumptabptr
	rts		

jumpU4	mvePLup		macro
	addq.l	#1,a0					;point to next delay
	cmp.b	#JE,(a0)
	beq	jumpUR
	move.b	(a0),jumpUdelay
	move.l	a0,jumptabptr
	cmp.b	#MR,joy_dir
	beq	pright
	cmp.b	#ML,joy_dir
	beq	pleft
	rts
jumpUR	subq.l	#1,a0				;point to previous delay
	move.b	#1,jfallflag
	move.b	(a0),jumpUdelay
	rts
jumpUE	clr.b	jfallflag
	clr.b	jumpflag
	rts	


jumpright
;	cmp.b	#ML,onconvflag
;	beq	jumpE
	cmp.b	#ML,icedir
	beq	jumpE
	tst.b	jumpflag				;currently jumping ?
	bne	jumpR1						;yes
	cmp.b	#ON,ontopblock
	beq	jumpR0
	tst.b	liftflag
	bne	jumpR0
	tst.b	onropeflag				;on a rope ?
	bne	jumpR0						;- yes
	tst.b	onladdflag
	beq	jumpE
jumpR0	cmp.w	#XMAX,x
	beq	jumpUP
	btst.b	#7,hsrh1+3
	bne	jumpUP
	move.b	#MR,jumpflag		
	clr.b	jfallflag
	clr.b	liftflag
	move.w	#WALK,planim
	move.l	#jumpRLtable1,jumptabptr
	move.b	#MAXFALLjump,maxfallctr
	cmp.b	#ON,ontopblock
	beq	jumpR2
jumpR1	btst.b	#7,hsrf1+3
	bne	jumpE1
jumpR2	btst.b	#7,hsrh1+3
	bne	jumpE1
	move.b	#MR,icedir				;allow sliding when land on ice
	move.b	#40,icectr
	move.l	jumptabptr,a0
	move.b	(a0)+,d0
	move.l	a0,jumptabptr
	cmp.b	#JU,d0
	beq	JJU		
	cmp.b	#JUXY,d0
	beq	JJUXYR		
	cmp.b	#JX,d0
	beq	JJXR		
	cmp.b	#JDXY,d0
	beq	JJDXYR		
	cmp.b	#JD,d0
	beq	JJD		
	cmp.b	#JE,d0
	beq	jumpRE
	rts						;### ERROR shouldn't reach here	
JJU	mvePLup
	rts		
JJUXYR	mvePLup
	mvePLrightj
	rts	
JJXR	mvePLrightj
	rts
JJDXYR	addq.w	#PLSPEED*4,y
	mvePLrightj
	move.b	#1,jfallflag
	rts
JJD	addq.w	#PLSPEED*4,y
	move.b	#1,jfallflag
	rts
jumpRE	clr.b	jumpflag
	clr.b	jfallflag
jumpE	rts
jumpE1	clr.b	maxfallctr
	clr.b	icectr
	clr.b	icedir
	clr.b	jumpflag
	clr.b	jfallflag
	cmp.b	#MUD,onladdflag
	bne	jumpE
	move.w	#CLIMB,planim
	rts



jumpleft
;	cmp.b	#MR,onconvflag
;	beq	jumpE
	cmp.b	#MR,icedir
	beq	jumpE
	tst.b	jumpflag					;currently jumping ?
	bne	jumpL1							;yes
	cmp.b	#ON,ontopblock
	beq	jumpL0
	tst.b	liftflag
	bne	jumpL0
	tst.b	onropeflag
	bne	jumpL0
	tst.b	onladdflag
	beq	jumpE
jumpL0	btst.b	#7,hslh1+3
	bne	jumpUP
	cmp.w	#XMIN,x
	beq	jumpUP
	clr.b	jfallflag
	clr.b	liftflag
	move.b	#ML,jumpflag				;no
	move.w	#WALK,planim
	move.l	#jumpRLtable1,jumptabptr
	move.b	#MAXFALLjump,maxfallctr
	cmp.b	#ON,ontopblock
	beq	jumpL2
jumpL1	btst.b	#7,hslf1+3
	bne	jumpE1
jumpL2	btst.b	#7,hslh1+3
	bne	jumpE1
	move.b	#ML,icedir					;allow sliding when land on ice
	move.b	#40,icectr
	move.l	jumptabptr,a0
	move.b	(a0)+,d0
	move.l	a0,jumptabptr
	cmp.b	#JU,d0
	beq	JJU		
	cmp.b	#JUXY,d0
	beq	JJUXYL		
	cmp.b	#JX,d0
	beq	JJXL		
	cmp.b	#JDXY,d0
	beq	JJDXYL		
	cmp.b	#JD,d0
	beq	JJD		
	cmp.b	#JE,d0
	beq	jumpRE
	rts							;### ERROR shouldn't reach here	
JJUXYL	mvePLup
	mvePLleftj
	clr.b	Jfallflag
	rts	
JJXL	mvePLleftj
	rts
JJDXYL	addq.w	#PLSPEED*4,y
	mvePLleftj
	move.b	#1,jfallflag
	rts

;        default    IncScoreBlk	  repl.block   score(10's)  0000 1000

blockcollision
	lea	entryscreen,a1
	moveq	#0,d2
	move.w	colblkpos,d2
	lsl	#4,d2		*16
	add.l	d2,a1
	move.l	colblkptr,a0
	move.b	colblktype,d1
	beq	bcol	
	cmp.b	#DEAD,d1
	beq	hitdead
	cmp.b	#DOOR,d1
	beq	hitdoor
	cmp.b	#TELEPORT,d1
	beq	hittransp
	cmp.b	#SETLOCn,d1
	beq	hitsetloc
	cmp.b	#CLRLOCn,d1
	beq	hitclrloc
	cmp.b	#TOGLOCn,d1
	beq	hittogloc
	cmp.b	#XtraLife,d1
	beq	hitxtralife

bcol	btst.b	#TAKEN,numblocks*2(a0)
	bne	bcolE
	btst.b	#COLLBIT,numblocks*2(a0)  		;is block a collectable ?
	beq	bcolE			   					;- no	

;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset
	
bcolS	btst.b	#COLCRMSK,numblocks(a0)		;set approp replace block
	beq	bcol0
	move.b	12(a1),10(a1)
	btst.b	#BANISTAT,numblocks*2(a0)
	beq	bcol1
	btst.b	#ANILOBLK,numblocks(a0)
	beq	bcol1
	move.b	12(a1),2(a1)				;-thus, mask=collect+anim
	move.b	12(a1),4(a1)
	move.b	#0,14(a1)					;set Hi-offset to 0
	bra	bcol1	
bcol0	move.b	12(a1),1(a1)
	btst.b	#BANISTAT,numblocks*2(a0)
	beq	bcol1
	btst.b	#ANILOBLK,numblocks(a0)
	bne	bcol1
	move.b	12(a1),2(a1)				;-thus, crunch=collect+anim
	move.b	12(a1),4(a1)
	move.b	#0,14(a1)					;set Hi-offset to 0
bcol1	moveq	#0,d1
	move.b	11(a1),d1					;score to add
	add.l	score,d1
	move.l	d1,score
	bset.b	#TAKEN,numblocks*2(a0)	
	move.b	#3,collectsampch1
	move.b	#SCOREDEL,scoredelay
	btst.b	#BANISTAT,numblocks*2(a0)
	bne	bcolE
	btst.b	#BPRIORITY,numblocks*2(a0)
	bne	bcolE
	bset.b	#READY,numblocks*2(a0)	
bcolE	rts
hitdead	bsr	looselife
	rts	

;bcomtype,Crunch,Hi,Strt,Cur,anistat,Sp,Spst,Loc,newscrn,msk,score
;,repl,btype,Hi-offset
hitdoor	
	btst.b	#Xexit,5(a1)		;automatic door (X) ?
	beq	hitd1	
	tst.b	jumpflag
	bne	bcolE					;plr can't jump into door (L/R)
	cmp.w	#Xmin,x				;- yes
	bne	hitd0
	cmp.b	#ML,onconvflag
	beq	hitdY
	cmp.b	#ML,joy_dir
	bne	bcolE
	beq	hitdY
hitd0	cmp.w	#Xmax,x
	bne	bcolE
	cmp.b	#MR,onconvflag
	beq	hitdY
	cmp.b	#MR,joy_dir
	beq	hitdY
	bne	bcolE
	
hitd1	btst.b	#Yexit,5(a1)	;automatic door (Y) ?
	beq	hitMD					;-no
	cmp.w	#Ymin,y				;-0
	beq	hitdUP
	cmp.w	#Ymax,y				;-820 
	blt	bcolE
	tst.b	maxfallflag
	bne	bcolE	
	tst.b	fallflag
	bne	hitdY					;allow plr to fall into door
	tst.b	jumpflag
	bne	hitdY					;allow plr to jump up into door
	cmp.b	#MD,joy_dir
	beq	hitdY
	bne	bcolE	

hitdUP	tst.b	jumpflag
	bne	hitdY
hitMD	cmp.b	#MU,joy_dir
	bne	bcolE
hitdY	tst.b	8(a1)			;key needed ?
	beq	hitd3					;-no
	lea	condtab,a2
	moveq	#0,d0
	move.b	8(a1),d0
	tst.b	(a2,d0)				;locn set ?
	bne	hitd3
	tst.b	Jcentreflag
	bne	hitdE	
	move.b	#1,Jcentreflag
	move.b	#9,alien1sampch2	;locked door sfx
	rts
hitd3	tst.b	transpflag
	bne	bcolE	
	move.w	#$ffff,Newscpos					;initialise to use default newscXYpos
	btst.b	#NEWSCPOSbit,numblocks(a0)		;use default XYpos?
	beq	hitd4								;-yes
	move.b	11(a1),Newscpos
	move.b	12(a1),Newscpos+1	
hitd4	move.b	9(a1),curscrn+1
	addq.b	#1,curscrn+1
	move.b	#1,newscreenflag
	move.b	#17,collectsampch1				;doorslide
	move.b	#1,transpflag
hitdE	rts

hittransp
	cmp.b	#MU,joy_dir
	bne	bcolE
	tst.b	transpflag
	bne	bcolE	
	moveq	#0,d0
	moveq	#0,d1
	move.b	11(a1),d0				;new blockpos - hi byte 
	lsl.w	#8,d0
	move.b	12(a1),d0				;new blockpos - lo byte
hittr1	cmp.w	#blockwd,d0
	blt	hittr2
	sub.b	#blockwd,d0
	addq	#1,d1
	bra	hittr1
hittr2	move.l	d0,d2
	lsl.w	#4,d0					;*16
	add.w	#centreXoffset,d0
	move.w	d0,x
	lsl.w	#6,d1
	move.w	d1,y		
	cmp.w	#260,d2					;plr on bottom EDGE of screen ?
	blt	hittr3
	subq.w	#4,y					;-yes, adjust y
hittr3	move.b	#12,plrsampch0
	move.b	#1,transpflag
	rts

hitsetloc
	btst.b	#TAKEN,numblocks*2(a0)
	bne	bcolE
	lea	condtab,a3
	moveq	#0,d0
	move.b	8(a1),d0				;get loc to set
	move.b	#255,(a3,d0)
bcolSR	btst.b	#COLCRMSK,numblocks(a0)		;set approp replace block
	beq	bcol2
	move.b	12(a1),10(a1)
	btst.b	#BANISTAT,numblocks*2(a0)
	beq	bcol3
	btst.b	#ANILOBLK,numblocks(a0)
	beq	bcol3
	move.b	12(a1),2(a1)
	move.b	12(a1),4(a1)
	move.b	#0,14(a1)
	bra	bcol3	
bcol2	move.b	12(a1),1(a1)
	btst.b	#BANISTAT,numblocks*2(a0)
	beq	bcol3
	btst.b	#ANILOBLK,numblocks(a0)
	bne	bcol3
	move.b	12(a1),2(a1)
	move.b	12(a1),4(a1)
	move.b	#0,14(a1)
bcol3	moveq	#0,d1
	move.b	11(a1),d1	score to add
	add.l	score,d1
	move.l	d1,score
	move.b	#6,collectsampch1
	bset.b	#TAKEN,numblocks*2(a0)	
	move.b	#SCOREDEL,scoredelay
	btst.b	#BANISTAT,numblocks*2(a0)
	bne	bcolE
	btst.b	#BPRIORITY,numblocks*2(a0)
	bne	bcolE
	bset.b	#READY,numblocks*2(a0)	
	rts
hitclrloc
	btst.b	#TAKEN,numblocks*2(a0)
	bne	bcolE
	lea	condtab,a3
	moveq	#0,d0
	move.b	8(a1),d0				;get loc to set
	clr.b	(a3,d0)
	bra	bcolSR
hittogloc
	cmp.b	#F,joy_dir
	bne	bcolE
	tst.b	firebutflag
	bne	bcolE	
	lea	condtab,a3
	moveq	#0,d0
	move.b	8(a1),d0				;get loc to toggle
	move.b	#1,firebutflag
	btst.b	#COLCRMSK,numblocks(a0)		;set approp replace block
	beq	bcol4

	move.b	6(a1),12(a1)
	move.b	6(a1),4(a1)
	not.b	(a3,d0)					;0=use mask block,  1=use replace block
	beq	bcol5
	move.b	2(a1),12(a1)
	move.b	2(a1),4(a1)
	bra	bcol5	
bcol4	move.b	6(a1),12(a1)
	not.b	(a3,d0)					;0=use crunch block,  1=use replace block
	beq	bcol5
	move.b	2(a1),12(a1)
bcol5	moveq	#0,d1
	move.b	11(a1),d1				;score to add
	add.l	score,d1
	move.l	d1,score
	move.b	#SCOREDEL,scoredelay
	bset.b	#READY,numblocks*2(a0)	
	move.b	#6,collectsampch1
	rts

hitXtraLife
	btst.b	#TAKEN,numblocks*2(a0)
	bne	bcolE
	addq.w	#1,lives
	bsr	printlives
	bra	bcolS	

convertxy

	DOCALC	HSR,HSH,blockposrh,hsrh,hsrh+2,hsrhc,hsrhB2
	DOCALC	HSL,HSH,blockposlh,hslh,hslh+2,hslhc,hslhB2
	DOCALC	HSR,HSF,blockposrf,hsrf,hsrf+2,hsrfc,hsrfB2
	DOCALC	HSL,HSF,blockposlf,hslf,hslf+2,hslfc,hslfB2
	DOCALC	HSR-PLSPEED,HSH,blockposrh1,hsrh1,hsrh1+2,hsrh1c,hsrh1B2
	DOCALC	HSL+PLSPEED,HSH,blockposlh1,hslh1,hslh1+2,hslh1c,hslh1B2
	DOCALC	HSR-PLSPEED,HSF,blockposrf1,hsrf1,hsrf1+2,hsrf1c,hsrf1B2
	DOCALC	HSL+PLSPEED,HSF,blockposlf1,hslf1,hslf1+2,hslf1c,hslf1B2

	ONTOP		;returns with blockpos in blockposrf2
	
	COLBLK	HSCLR,HSCHF,colblk019,colblkpos,colblkptr,colblktype,tmp1b
	COLBLK	HSCLR,HSF,midF019,midFbpos,midFbptr,midFbtype,midFcbtype
	COLBLK	HSCLR,HSH,midH019,midHbpos,midHbptr,midHbtype,midHcbtype

	DOCALC	HSRR,HSHR,blockposrrh,hsrrh,hsrrh+2,hsrrhc,tmp1b        ;these 4 hot-
	DOCALC	HSLR,HSHR,blockposrlh,hsrlh,hsrlh+2,hsrlhc,tmp1b        ;spots detect
	DOCALC	HSRR,HSFR,blockposrrf,hsrrf,hsrrf+2,hsrrfc,tmp1b        ;a rope
	DOCALC	HSLR,HSFR,blockposrlf,hsrlf,hsrlf+2,hsrlfc,tmp1b


;we'll now check to see if plr is ontop or inside a block
	clr.b	ontopblock
	move.w	blockposrf,d0
	cmp.w	blockposrf2,d0		;rf2 = 1 pixel above rf
	bne	convertontop		
	rts
convertontop
	move.b	#ON,ontopblock
	rts	


;				screen height = 224, width = 312



curtok		dc.w	0
curscrn		dc.w	0
x1		dc.w	0
x2		dc.w	0
planim		dc.w	0	    ;WALK or CLIMB
planimdir	dc.w	0	    ;current player direction
rsetanisp	dc.w	0
walkdir		dc.w	0

key		dc.b	0
joy_dir		dc.b	0
upflag		dc.b	0
dnflag		dc.b	0
jumpflag	dc.b	0	    ;1=currently jumping
jumpflagst	dc.b	0
jtab12flag	dc.b	0	    ;1=both tables enabled	
jtab12		dc.b	0	    ;0=use jtable1, 1=use jtable2
jumpUdelay	dc.b	0
ontopblock	dc.b	0
Jfallflag	dc.b	0	    ;1=on the way down
transpflag	dc.b	0
transpflagst	dc.b	0
Jcentreflag	dc.b	0
Jcentreflagst	dc.b	0
firebutflag	dc.b	0
firebutflagst	dc.b	0
liftflag	dc.b	0
bobislift	dc.b	0
icectr		dc.b	0
icedir		dc.b	0
pldirstat	dc.b	0
planimstat	dc.b	0
oniceflag	dc.b	0
ongrndflag	dc.b	0
onropeflag	dc.b	0
onropeflagofst	dc.b	0
onspaceflag	dc.b	0
onpipeflag	dc.b	0
onladdflag	dc.b	0
onconvflag	dc.b	0
surfaceflag	dc.b	0
lostlifeflag	dc.b	0
maxfallflag	dc.b	0
fallflag	dc.b	0
falling		dc.b	0
maxfallctr	dc.b	0
colblk019	dc.b	0	    ;holds blockpos on x-axis (0-19)
midF019		dc.b	0
midH019		dc.b	0




	even
hsrh		dc.l	0	    ;hot-spot bytes
hslh		dc.l	0	    ;hi word = crunchscreen block number 
hsrf		dc.l	0
hslf		dc.l	0
hsrh1		dc.l	0
hslh1		dc.l	0
hsrf1		dc.l	0
hslf1		dc.l	0
hsrrh		dc.l	0
hsrlh		dc.l	0
hsrrf		dc.l	0
hsrlf		dc.l	0
hsrhB2		dc.l	0	    ;these bytes hold status byte 2
hslhB2		dc.l	0 
hsrfB2		dc.l	0
hslfB2		dc.l	0
hsrh1B2		dc.l	0
hslh1B2		dc.l	0
hsrf1B2		dc.l	0
hslf1B2		dc.l	0


hsrhc		dc.l	0	    ;these bytes hold the approp. CombBtypes	
hslhc		dc.l	0 
hsrfc		dc.l	0
hslfc		dc.l	0
hsrh1c		dc.l	0
hslh1c		dc.l	0
hsrf1c		dc.l	0
hslf1c		dc.l	0
hsrrhc		dc.l	0
hsrlhc		dc.l	0
hsrrfc		dc.l	0
hsrlfc		dc.l	0


colblkptr	dc.l	0	    ;btst.b numblocks*4(colblkptr)
midFbptr	dc.l	0
midHbptr	dc.l	0
colblktype	dc.w	0	    ;block type (0-63)
colblkpos	dc.w	0	    ;scrn block pos
midFbpos	dc.w	0
midHbpos	dc.w	0
midFbtype	dc.w	0
midHbtype	dc.w	0
midFcbtype	dc.w	0
midHcbtype	dc.w	0




blockposrf	dc.w	0	    ;screen position of block at hot-spot
blockposlf	dc.w	0
blockposrh	dc.w	0
blockposlh	dc.w	0
blockposrf1	dc.w	0	    ;screen position of block at hot-spot
blockposlf1	dc.w	0
blockposrh1	dc.w	0
blockposlh1	dc.w	0
blockposrf2	dc.w	0
blockposrrf	dc.w	0
blockposrlf	dc.w	0
blockposrrh	dc.w	0
blockposrlh	dc.w	0


jumptabptr	dc.l	0


jumpRLtable1	dc.b	JU,JU,JU,JU,JU,JU,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JUXY,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX
		dc.b	JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JX,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JDXY,JD,JD,JD,JD,JD,JD,JD,JD,JE
jumpRLtable2	dc.b	JU,JU,JU,JU,JU,JX,JX,JX,JX,JX,JD,JD,JD,JD,JD,JE
		

;numbers = frame delay
jumpUDtable	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,2,2,JE


;	20 bytes, where 1=move, 0=don't move :-
inertiatab	dc.b	1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0
		dc.b	0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1
		dc.b	1,0,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1
		dc.b	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
inertiaend

SLIDEMAX	=	inertiaend-inertiatab
JSLIDEMAX	=	SLIDEMAX/2	
	even

*** Exit Program ***

ende	clr.w	aud0vol(a5)
	clr.w	aud1vol(a5)
	clr.w	aud2vol(a5)
	clr.w	aud3vol(a5)
	lea	$dff000,a5
	bsr	vertblank
	move.w	#$7fff,dmacon(a5)
;	move.w	#$7fff,intena(a5)
	or.w	#$8000,dmastore
	or.w	#$8000,intenastore
	or.w	#$8000,adkonstore
	move.w	dmastore,dmacon(a5)
	move.w	intenastore,intena(a5)
	move.w	adkonstore,adkcon(a5)
	move.l	copper1store,cop1lc(a5)
	move.l	copper2store,cop2lc(a5)
	move.w	#0,copjmp1(a5)
	move.l	exec,a6
	jsr	enable(a6)
	movem.l	(a7)+,d0-a6
	moveq.l	#$00,d0
	rts




*** Set up Bitplane Pointers ***

introscpoint
	move.l	#screen1+bwidth,d1		

	move.w	d1,intpl1+6
	swap	d1
	move.w	d1,intpl1+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,intpl2+6
	swap	d1
	move.w	d1,intpl2+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,intpl3+6
	swap	d1
	move.w	d1,intpl3+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,intpl4+6
	swap	d1
	move.w	d1,intpl4+2
	rts


scpoint	
	move.l	visibleptr,d1		

	add.l	#bwidth,d1		    ;centre the screen
	move.w	d1,plane1+6
	swap	d1
	move.w	d1,plane1+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,plane2+6
	swap	d1
	move.w	d1,plane2+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,plane3+6
	swap	d1
	move.w	d1,plane3+2

	swap	d1
	add.l	#scwidth,d1
	move.w	d1,plane4+6
	swap	d1
	move.w	d1,plane4+2
	rts


copycol	
	move.l	#blockdata,a1 		
	add.l	#bscwidth*256*PLANES,a1
	add.l	#2,a2			            ;1st colour value
	move.l	#15,d1			            ;copy 1st 16 colours
copyc1	move.w	(a1)+,(a2)
	add.l	#4,a2
	dbra	d1,copyc1
	rts
	

;****************** Storage area *******************************


oldcop 		dc.l 	0
gfxname 	dc.b	"graphics.library",0
		even
gfxbase 	dc.l  	0
dosname		dc.b	"dos.library",0
		even
dosbase		dc.l	0

		even


	;****** ALWAYS ADD TO END OF EACH dc BLOCK ******

x		dcb.w	MB		    ;bob x co-ordinates
y		dcb.w	MB	        ;*2	 "  y     "
loopctr		dcb.w	MB	    ;*4		
sprttok		dcb.w	MB	    ;*6  		
spspeed		dcb.w	MB	    ;*8		
spspeedst	dcb.w	MB	    ;*10		
spwait		dcb.w	MB	    ;*12		
anionoff	dcb.w	MB	    ;*14	anim status
anispeed	dcb.w	MB	    ;*16
anispdst	dcb.w	MB	    ;*18
anictr		dcb.w	MB	    ;*20
anictrst	dcb.w	MB	    ;*22
anidir		dcb.w	MB	    ;*24
anidir2		dcb.w	MB	    ;*26
bobtmp1		dcb.w	MB	    ;*28
bobtmp2		dcb.w	MB	    ;*30 
bobtmp3		dcb.w	MB	    ;*32
framelo		dcb.w	MB	    ;*34	1st anim frame
framehi		dcb.w	MB	    ;*36	last	"	"
curframe	dcb.w	MB	    ;*38	curent bob frame
ad3wait		dcb.w	MB	    ;*40
ad3waitst	dcb.w	MB	    ;*42
ad3onoff	dcb.w	MB	    ;*44
syncsfx		dcb.w	MB	    ;*46	if sfx=0 then sync disabled
syncsfxp1	dcb.w	MB	    ;*48	synchronise sfx to p1 & p2
syncsfxp2	dcb.w	MB	    ;*50	
bobspeedx2	dcb.w	MB,ALSPEED	    ;*52	num pixels to add to x
bobspeedy2	dcb.w	MB,ALSPEED*4	;*54	num pixels to add to y

setconXtab	dcb.w	MB*10	;*56	space for 10-1 condXs for each bob
setconYtab	dcb.w	MB*10	;*76	space for 10-1 condYs for each bob
setcondel	dcb.w	MB*2	;*96
sldown		dcb.w	MB	    ;*100

;		DC.B		*102



spptr		dcb.l	MB		;pointer to current spdat posn
spptrst		dcb.l	MB	    ;*4 		
prtn		dcb.l	MB	    ;*8		
looprtn		dcb.l	MB	    ;*12 		
bobstat		dcb.l	MB	    ;*16	bob status bits

				;bit num - alien								
				;   0   1=bob is a lift
				;   1	0=move bob	   1=stop bob
	
				;bit num - player (i.e. 1st longword)								
				
				;   1   0=off ice	   1=on ice      
		
			
xy1		dcb.w	MB		;xy co-ords for screen 1
		dcb.w	MB
xy2		dcb.w	MB		;xy co-ords for screen 2
		dcb.w	MB


;Block Animation Definition

banionoff	dcb.b	NumBlocks		;0=animoff 1=infront 2=behind
banispeed	dcb.b	NumBlocks	    ;*1
banispdst	dcb.b	NumBlocks	    ;*2
banictr		dcb.b	NumBlocks	    ;*3
banictrst	dcb.b	NumBlocks	    ;*4
banidir		dcb.b	NumBlocks	    ;*5
banidir2	dcb.b	NumBlocks	    ;*6
bframelo	dcb.b	NumBlocks	    ;*7	1st anim frame
bframehi	dcb.b	NumBlocks	    ;*8	last	"	"
bcurframe	dcb.b	NumBlocks	    ;*9	curent block frame
bstatus		dcb.b	NumBlocks	    ;*10	>0"= player interaction poss
bx1		dcb.b	NumBlocks	        ;*11	x co-ord as hit from right
bx2		dcb.b	NumBlocks	        ;*12	"   "     "      "   left
by1		dcb.b	NumBlocks	        ;*13	y co-ord as hit from top
by2		dcb.b	NumBlocks	        ;*14	"   "     "      "   bottom
bsetclrcond	dcb.b	NumBlocks	    ;*15	platform feature conditions	
bscore		dcb.b	NumBlocks	    ;*16	collectable block score
newblock	dcb.b	NumBlocks	    ;*17	new block							
							

							
;		blockstat	 meaning
;		---------	 -------
;		  0	block(object) inactive ('taken') 
;		  1	loose life if collide	 
;		  2	gain life if collide (set blockstat to 0 after)
;		  *	set platform feature condition
;		  *	clear platform feature condition
;		  3	collectable object (bnum,3,x1x2y1y2,scr,newblk)
;		  4	enable jumptable 1&2 (bnum,4,x1x2y1y2,newblk)
;		  5	umbarella (bnum,5,x1x2y1y2,newblk)
;		  6	door (bnum,6,x1x2y1y2,cond/NC,newscreen)
;		  7	setclrcond (bnum,7,x1x2y1y2,val,condnum,newblock)

;	***** EXPAND THIS YA BAAAASS *****

		;dc.w	blocknum,statnum,parameters
bstatdef	dc.w	E
		dc.w	E
		dc.w	E	    ;end of screen 3
		dc.w	E	    ;end of screen 4
		dc.w	E	    ;end of screen 5
		dc.w	E	    ;end of screen 6
		dc.w	E	    ;end of screen 7
		dc.w	E	    ;end of screen 8
		dc.w	E	    ;end of screen 9
		dc.w	E	    ;end of screen 10
		dc.w	E	    ;end of screen 11
		dc.w	E	    ;end of screen 12
		dc.w	E	    ;end of screen 13
		dc.w	E	    ;end of screen 14
		dc.w	E	    ;end of screen 15
		dc.w	E	    ;end of screen 16
		dc.w	E	    ;end of screen 17
		dc.w	E	    ;end of screen 18
		dc.w	E	    ;end of screen 19
		dc.w	E	    ;end of screen 20
		dc.w	BE
bstatdefptr	dc.l	0	



;ALIEN MOVEMENT TABLE :-

		even
;screen 1
firstscreen	dc.w	NXTSP
		dc.w	NB,8,PD,64,768,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,64,4,AD,2,AN,81,84,3,ST,D,468,U,4,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,112,344,AD,2,AN,81,84,3,ST,U,4,D,344,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,160,4,AD,2,AN,81,84,3,ST,D,344,U,4,JST
		dc.w	NXTSP
		dc.w	ANC,0,SPD,1,P,208,344,AD,2,AN,81,84,3,ST,U,4,D,344,JST
		dc.w	NXTSP
		dc.w	ANC,0,SPD,1,P,256,4,AD,2,AN,81,84,3,ST,D,280,U,4,JST
		dc.w	NXTSP
		dc.w	ANC,0,SPD,0,P,368,768,ST,AN,10,19,2,L,32,AN,0,9,2,R,368,JST
		dc.w	NXTSP
		dc.w	ANC,0,SPD,1,P,48,564,FP,0,WC,2,P,48,576,ST,AN,0,9,4
		dc.w	R,241,AN,10,19,4,L,32,JST
		dc.w	NXTSP


				
;screen 2
		dc.w	NXTSCRN
		dc.w	NB,9,PD,48,192,PW
		dc.w	NXTSP
		dc.w	P,176,4,SPD,1,AD,2,AN,81,84,3,ST,D,280,U,4,JST	
		dc.w	NXTSP
		dc.w	P,272,4,SPD,1,AD,2,AN,81,84,3,ST,D,280,U,4,JST	
		dc.w	NXTSP
		dc.w	P,112,152,AD,2,AN,81,84,3,PW	
		dc.w	NXTSP
		dc.w	P,129,192,SPD,1,ST,AN,10,19,4,L,48,AN,0,9,4,R,129,JST
		dc.w	NXTSP
		dc.w	P,155,176,SPD,1,AD,2,ST,AN,87,90,3,R,322,AN,91,94,3
		dc.w	L,155,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,240,640,SPD,1,ST,AN,0,9,4,R,352,AN,10,19,4,L,240,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,352,640,SPD,1,ST,AN,10,19,4,L,240,AN,0,9,4,R,352,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,64,732,AD,2,AN,81,84,3,SPD,1,ST,U,252,D,732,JST
		dc.w	NXTSP


;screen 3
		dc.w	NXTSCRN
		dc.w	NB,10,PD,48,320,PW
		dc.w	NXTSP
		dc.w	SPD,1,AD,2,AN,81,84,3,P,208,320,ST,U,0,D,320,JST
		dc.w	NXTSP
		dc.w	SPD,1,AD,2,AN,81,84,3,P,256,256,ST,U,0,D,256,JST
		dc.w	NXTSP
		dc.w	SPD,1,AD,2,AN,81,84,3,P,304,0,ST,D,256,U,0,JST
		dc.w	NXTSP
		dc.w	SPD,1,AD,2,AN,81,84,3,P,352,320,ST,U,0,D,320,JST
		dc.w	NXTSP
		dc.w	LFT,ANC,0,AN,58,59,6,P,272,596,SPD,1,WC,5,ST,L,161,U,512,D,596
		dc.w	R,272,JST
		dc.w	NXTSP
		dc.w	LFT,ANC,0,AN,58,59,6,P,225,752,SPD,1,WC,4,ST,U,384,D,752,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,265,768,SPD,1,ST,AN,0,9,4,R,360,AN,10,19,4,L,265,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,360,768,SPD,1,ST,AN,10,19,4,L,265,AN,0,9,4,R,360,JST
		dc.w	NXTSP
		dc.w	P,73,64,SPD,1,ST,AN,0,9,4,R,135,AN,10,19,4,L,73,JST
		dc.w	NXTSP

;screen 4
		dc.w	NXTSCRN
		dc.w	NB,11,PD,352,768,PW
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,W,50,ST,AN,0,9,2,R,368,AN,10,19,2,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,W,130,ST,AN,0,9,2,R,368,AN,10,19,2,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,W,260,ST,AN,0,9,2,R,368,AN,10,19,2,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,278,384,ST,AN,10,19,4,L,41,AN,0,9,4,R,278,JST
		dc.w	NXTSP
		dc.w	P,138,344,FP,54,ST,W,100,ANC,1,AD,0,AN,54,57,5
		dc.w	W,100,AD,1,AN,54,57,5,AW,57,JST
		dc.w	NXTSP
		dc.w	P,186,344,FP,54,ST,W,150,ANC,1,AD,0,AN,54,57,5
		dc.w	W,100,AD,1,AN,54,57,5,AW,57,JST
		dc.w	NXTSP
		dc.w	P,234,344,FP,54,ST,W,200,ANC,1,AD,0,AN,54,57,5
		dc.w	W,100,AD,1,AN,54,57,5,AW,57,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,42,64,ST,AN,0,9,4,R,87,AN,10,19,4,L,42,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,96,84,AN,58,59,6,ST,WC,8,D,148,R,224,W,50
		dc.w	L,96,U,84,W,50,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,208,768,AN,58,59,6,ST,WC,7,U,596,W,50
		dc.w	D,768,JST
		dc.w	NXTSP


;screen 5
		dc.w	NXTSCRN
		dc.w	NB,5,PD,48,320,PW
		dc.w	NXTSP
		dc.w	P,48,500,SPD,1,AD,2,AN,81,84,3,ST,U,0,D,548,JST	
		dc.w	NXTSP
		dc.w	P,370,200,SPD,1,AD,2,ST,AN,91,94,3,L,20,AN,87,90,3
		dc.w	R,370,JST
		dc.w	NXTSP
		dc.w	ST,P,330,584,FP,95,SPD,1,LP,3,W,200,ANC,1,AD,0
		dc.w	AN,95,97,10,W,80,AD,1,AN,95,97,10,W,80,JLP,SD,18,3
		dc.w	AD,2,ANC,0,AN,91,94,4,L,286,AN,105,108,3,L,272
		dc.w	AS,2,SPD,3,D,648,FP,95,LP,2,W,100,ANC,1,AD,0
		dc.w	AN,95,97,10,W,50,AD,1,AN,95,97,10,W,50,JLP,SD,18,3
		dc.w	AD,2,ANC,0,AN,105,108,2,SPD,2,U,508,AN,91,94,2,SPD,1
		dc.w	L,226,FP,94,L,113,AN,91,94,4,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,0,W,100,AN,109,112,3,ST,P,356,0,D,208,SD,9,2,L,320
		dc.w	D,592,SD,9,2,L,273,D,656,SD,9,2,L,257,D,784,SD,9,2
		dc.w	L,225,D,852,P,0,0,W,100,JST
		dc.w	NXTSP

;screen 6
		dc.w	NXTSCRN
		dc.w	NB,6,PD,48,320,PW
		dc.w	NXTSP
		dc.w	P,144,400,AD,2,AN,81,84,3,SPD,1,ST,U,0,D,400,JST
		dc.w	NXTSP
		dc.w	P,192,168,AD,2,AN,81,84,3,SPD,1,ST,U,0,D,400,JST
		dc.w	NXTSP
		dc.w	P,288,400,AD,2,AN,81,84,3,SPD,1,ST,U,0,D,400,JST
		dc.w	NXTSP
		dc.w	P,354,768,SPD,1,ST,AD,0,AN,71,78,2,ASC,$0D03,73,73
		dc.w	L,170,AW,71,AD,1,ASC,$0D03,76,76,R,354,AW,71,JST
		dc.w	NXTSP
		dc.w	P,30,312,AD,2,ST,AN,87,90,3,R,369,AN,91,94,3,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,0,AD,2,AN,113,115,5,ST,P,362,768,W,150,L,166
		dc.w	D,828,JST
		dc.w	NXTSP

;screen 7
		dc.w	NXTSCRN
		dc.w	NB,8,PD,48,320,PW
		dc.w	NXTSP
		dc.w	P,112,0,AD,2,AN,81,84,3,SPD,1,ST,D,220,U,0,JST	
		dc.w	NXTSP
		dc.w	P,160,220,AD,2,AN,81,84,3,SPD,1,ST,U,0,D,220,JST
		dc.w	NXTSP
		dc.w	P,208,0,AD,2,AN,81,84,3,SPD,1,ST,D,220,U,0,JST	
		dc.w	NXTSP
		dc.w	P,127,768,FP,116,ANC,1,ST,CC,14,W,300,AN,116,118,4
		dc.w	AW,118,SC,14,FP,116,W,2,JST
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,AN,119,121,7,ST,WC,14,P,127,768,SD,14,3,R,361,JST
		dc.w	NXTSP
		dc.w	P,324,576,FP,122,ANC,1,ST,CC,15,W,270,AN,122,124,4
		dc.w	AW,124,SC,15,FP,122,W,2,JST
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,AN,119,121,7,ST,WC,15,P,309,576,SD,14,2,L,32,JST
		dc.w	NXTSP

;screen 8
		dc.w	NXTSCRN
		dc.w	NB,10,PD,48,320,PW
		dc.w	NXTSP
		dc.w	P,107,384,SPD,1,ST,AN,0,9,4,R,278,AN,10,19,4,L,107,JST
		dc.w	NXTSP
		dc.w	P,278,384,SPD,1,ST,AN,10,19,4,L,107,AN,0,9,4,R,278,JST
		dc.w	NXTSP
		dc.w	SPD,0,AD,2,AN,113,115,5,P,294,768,ST,L,73,R,294,JST
		dc.w	NXTSP
		dc.w	SPD,0,AD,2,AN,113,115,5,P,73,768,ST,R,294,L,73,JST
		dc.w	NXTSP
		dc.w	P,218,384,FP,54,ST,W,200,ANC,1,AD,0,AN,55,57,5
		dc.w	W,100,AD,1,AN,55,57,5,AW,57,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ST,P,129,476,FP,187,W,100,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,788,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ST,P,257,476,FP,187,W,150,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,788,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ST,P,177,472,FP,187,W,200,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,596,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ST,P,208,472,FP,187,W,250,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,596,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP

;screen 9
		dc.w	NXTSCRN
		dc.w	NB,9,PD,48,320,PW
		dc.w	NXTSP
		dc.w	P,216,384,SPD,1,ST,AN,10,19,4,L,122,AN,0,9,4,R,216,JST
		dc.w	NXTSP
		dc.w	P,227,704,SPD,4,ST,AN,60,62,14,AD,0,L,46,AD,1,R,355,JST
		dc.w	NXTSP
		dc.w	P,49,140,SPD,0,AD,2,ST,AN,87,90,4,R,305,AN,91,94,4,L,49,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,48,712,AN,166,168,2,ST,AD,0,R,353,AD,1,L,48,JST
		dc.w	NXTSP
		dc.w	W,200,SPD,1,P,48,712,AN,166,168,2,ST,AD,0,R,353,AD,1,L,48,JST
		dc.w	NXTSP
		dc.w	W,600,SPD,1,P,48,712,AN,166,168,2,ST,AD,0,R,353,AD,1,L,48,JST
		dc.w	NXTSP
		dc.w	SPD,0,ST,P,138,244,FP,187,W,100,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,404,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP
		dc.w	SPD,0,ST,P,176,248,FP,187,W,150,ANC,1,AD,0,AN,187,190,10
		dc.w	AW,190,W,10,D,404,AN,190,195,2,AW,195,P,0,0,JST
		dc.w	NXTSP

;screen 10
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,257,768,ST,AN,0,9,4,R,302,AN,10,19,4,L,257,JST
		dc.w	NXTSP
		dc.w	P,340,768,SPD,4,ST,AN,60,62,14,AD,0,L,34,AD,1,R,340,JST
		dc.w	NXTSP
		dc.w	P,34,768,SPD,4,ST,AN,60,62,14,AD,1,R,340,AD,0,L,34,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP



;screen 11
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,30,736,AD,2,ST,AN,87,90,3,R,320,AN,91,94,3
		dc.w	L,30,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,109,112,6,AD,1,ST,P,45,0,D,588,SD,9,3,R,96,D,720
		dc.w	SD,9,3,R,110,D,784,SD,9,3,R,319,D,852,P,0,0,W,100,JST
		dc.w	NXTSP
		dc.w	SPD,0,W,150,AN,109,112,6,AD,1,ST,P,45,0,D,588,SD,9,3
		dc.w	R,96,D,720,SD,9,3,R,110,D,784,SD,9,3,R,319,D,852,P,0,0,W,100,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ANC,1,ST,FP,63,P,137,216,W,10,FP,64,W,50,FP,65,W,50
		dc.w	FP,66,W,100,D,800,SD,5,2,AN,66,70,6,AW,70,P,0,0,W,20,JST
		dc.w	NXTSP
		dc.w	SPD2,2,ANC,1,ST,FP,63,P,249,216,W,40,FP,64,W,50,FP,65,W,50
		dc.w	FP,66,W,100,D,800,SD,5,2,AN,66,70,6,AW,70,P,0,0,W,20,JST
		dc.w	NXTSP

;screen 12
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,91,772,ST,AN,0,9,4,R,368,AN,10,19,4,L,91,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,250,772,ST,AN,10,19,4,L,91,AN,0,9,4,R,368,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 13
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 14
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,160,64,ST,AN,0,9,4,R,368,AN,10,19,4,L,30,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,180,64,ST,AN,10,19,4,L,30,AN,0,9,4,R,368,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,230,708,ST,AN,10,19,4,L,30,AN,0,9,4,R,230,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 15
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,2,AD,2,W,250,ST,P,370,64,AN,154,157,6,L,319
		dc.w	FP,154,SPD,0,SPD2,2,D,768,SD,9,3,AN,154,157,6,SPD,2
		dc.w	L,56,AN,158,161,6,R,330,AW,161,FP,161,SPD,0,SPD2,2
		dc.w	SD,13,2,U,448,SPD,2,AN,158,161,6,R,366,JST
		dc.w	NXTSP
		dc.w	SPD,2,AD,2,W,450,ST,P,370,64,AN,154,157,6,L,319
		dc.w	FP,154,SPD,0,SPD2,2,D,768,SD,9,3,AN,154,157,6,SPD,2
		dc.w	L,56,AN,158,161,6,R,330,AW,161,FP,161,SPD,0,SPD2,2
		dc.w	SD,13,2,U,448,SPD,2,AN,158,161,6,R,366,JST
		dc.w	NXTSP
		dc.w	SPD,2,AD,2,W,650,ST,P,370,64,AN,154,157,6,L,319
		dc.w	FP,154,SPD,0,SPD2,2,D,768,SD,9,3,AN,154,157,6,SPD,2
		dc.w	L,56,AN,158,161,6,R,330,AW,161,FP,161,SPD,0,SPD2,2
		dc.w	SD,13,2,U,448,SPD,2,AN,158,161,6,R,366,JST
		dc.w	NXTSP
		dc.w	SPD,2,AD,2,W,850,ST,P,370,64,AN,154,157,6,L,319
		dc.w	FP,154,SPD,0,SPD2,2,D,768,SD,9,3,AN,154,157,6,SPD,2
		dc.w	L,56,AN,158,161,6,R,330,AW,161,FP,161,SPD,0,SPD2,2
		dc.w	SD,13,2,U,448,SPD,2,AN,158,161,6,R,366,JST
		dc.w	NXTSP
		dc.w	SPD,2,AD,2,W,1050,ST,P,370,64,AN,154,157,6,L,319
		dc.w	FP,154,SPD,0,SPD2,2,D,768,SD,9,3,AN,154,157,6,SPD,2
		dc.w	L,56,AN,158,161,6,R,330,AW,161,FP,161,SPD,0,SPD2,2
		dc.w	SD,13,2,U,448,SPD,2,AN,158,161,6,R,366,JST
		dc.w	NXTSP

;screen 16
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,SPD2,2,P,58,768,AN,113,115,4,AD,2,ST,R,214,L,58,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,43,836,XYD,20,SCY,100,0,AN,169,172,3,ST,AD,1
		dc.w	U,0,AD,0,D,836,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,173,177,1,ST,P,0,0,WCXY,100,SD,14,3,R,366,CC,100,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP


;screen 17
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,320,0,AD,2,AN,81,84,3,ST,D,740,U,0,JST	
		dc.w	NXTSP
		dc.w	SPD,4,P,94,128,AN,60,62,14,ST,AD,1,R,276,AD,0,L,94,JST
		dc.w	NXTSP
		dc.w	SPD,4,P,276,128,AN,60,62,14,ST,AD,0,L,94,AD,1,R,276,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,91,24,XYD,20,SCX,100,101,0,AN,178,181,3,ST,AD,0
		dc.w	R,277,AD,1,L,91,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,173,177,1,ST,P,0,0,WCXY,100,SD,14,3,D,832,CC,100,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,173,177,1,ST,P,0,0,WCXY,101,SD,14,3,D,832,CC,101,JST
		dc.w	NXTSP

;screen 18
		dc.w	NXTSCRN
		dc.w	NB,10,PD,275,4,PW
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,97,576,W,10,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,30,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,129,576,W,20,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,10,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,20,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,161,576,W,40,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,40,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,193,576,W,100,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,20,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,225,576,W,110,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,30,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,257,576,W,80,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,40,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,20,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,289,576,W,60,FP,64,W,20,FP,65
		dc.w	W,20,FP,66,W,30,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,321,576,W,20,FP,64,W,10,FP,65
		dc.w	W,20,FP,66,W,10,D,800,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,112,388,AN,58,59,12,ST,WC,16,U,84,D,388,JST
		dc.w	NXTSP

;screen 19
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	P,180,704,SPD,2,ST,AD,0,AN,71,78,4,ASC,$0D03,73,73
		dc.w	L,110,AW,71,AD,1,ASC,$0D03,77,77,R,180,AW,71,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP


;screen 20
		dc.w	NXTSCRN
		dc.w	NB,9,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,176,64,ST,AN,0,9,4,R,336,AN,10,19,4,L,176,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,336,64,ST,AN,10,19,4,L,176,AN,0,9,4,R,336,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,158,396,W,20,FP,64,W,10,FP,65
		dc.w	W,20,FP,66,W,30,D,672,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,205,396,W,40,FP,64,W,10,FP,65
		dc.w	W,20,FP,66,W,20,D,672,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,250,396,W,60,FP,64,W,10,FP,65
		dc.w	W,20,FP,66,W,10,D,672,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	spd2,2,ANC,1,ST,FP,63,P,286,396,W,80,FP,64,W,10,FP,65
		dc.w	W,20,FP,66,W,30,D,672,SD,5,3,AN,66,70,6,AW,70,P,0,0,W,10,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,357,688,XYD,20,SCY,100,0,AN,169,172,3
		dc.w	ST,AD,1,U,388,AD,0,D,856,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,173,177,1,ST,P,0,0,WCXY,100,SD,14,2,L,35,CC,100,JST
		dc.w	NXTSP

;screen 21
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	P,109,320,SPD,4,AN,60,62,14,ST,AD,1,R,323,AD,0,L,109,JST
		dc.w	NXTSP
		dc.w	P,323,320,SPD,4,AN,60,62,14,ST,AD,0,L,109,AD,1,R,323,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 22
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,84,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,161,768,ST,AN,0,9,4,R,288,AN,10,19,4,L,161,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,109,112,4,AD,1,SD,19,2,W,50,ST,P,112,0,D,848
		dc.w	P,0,0,W,100,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,109,112,4,AD,1,W,200,ST,P,112,0,D,848,P,0,0
		dc.w	W,100,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,345,4,XYD,20,SCX,100,101,0,AD,2,ST,AN,91,94,4,L,30
		dc.w	AN,87,90,4,R,345,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,182,186,4,ST,P,0,0,WCXY,100,SD,8,3,D,832,CC,100,JST
		dc.w	NXTSP
		dc.w	SPD,0,AN,182,186,4,ST,P,0,0,WCXY,101,SD,8,3,D,832,CC,101,JST
		dc.w	NXTSP
		

;screen 23
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,0,AN,109,112,4,AD,1,ST,P,112,0,D,528,SD,9,3,R,125
		dc.w	D,784,SD,9,3,R,351,D,848,P,0,0,W,100,JST
		dc.w	NXTSP
		dc.w	SPD,0,W,150,AN,109,112,4,AD,1,ST,P,112,0,D,528,SD,9,3
		dc.w	R,125,D,784,SD,9,3,R,351,D,848,P,0,0,W,100,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,341,768,AN,71,78,4,ST,AD,0,L,88,AD,1,R,341,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,159,88,AN,212,221,6,ST,D,676,U,88,JST
		dc.w	NXTSP
		dc.w	SPD,1,W,6,P,207,608,AN,212,221,6,ST,U,88,D,608,JST
		dc.w	NXTSP
		dc.w	SPD,1,W,12,P,255,88,AN,212,221,6,ST,D,608,U,88,JST
		dc.w	NXTSP


;screen 24
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,47,112,FP,0,WC,15,P,47,128,ST,AN,0,9,4,R,101
		dc.w	AN,10,19,4,L,47,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,144,28,AD,2,AN,81,84,3,SPD,1,ST,D,784,U,28,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,192,476,AD,2,AN,81,84,3,SPD,1,ST,U,28,D,476,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,240,28,AD,2,AN,81,84,3,SPD,1,ST,D,782,U,28,JST
		dc.w	NXTSP
		dc.w	ANC,0,P,288,476,AD,2,AN,81,84,3,SPD,1,ST,U,28,D,476,JST
		dc.w	NXTSP

;screen 25
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,AD,2,AN,113,115,6,ST,P,106,704,ST,R,343,L,106,JST
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,AD,2,AN,113,115,6,ST,P,343,704,ST,L,106,R,343,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 26
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,4,PW
		dc.w	NXTSP
		dc.w	P,336,128,SPD,1,ST,AN,10,19,4,L,182,AN,0,9,4,R,336,JST
		dc.w	NXTSP
		dc.w	P,261,320,SPD,1,ST,AN,10,19,4,L,125,AN,0,9,4,R,261,JST
		dc.w	NXTSP
		dc.w	P,96,520,SPD,0,AN,166,168,2,ST,AD,0,R,354,AD,1,L,96,JST
		dc.w	NXTSP
		dc.w	P,143,136,SPD,0,AN,166,168,2,ST,AD,0,R,338,AD,1,L,143,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,48,24,XYD,20,SCY,100,0,ST,AD,0,AN,169,172,1
		dc.w	D,580,AD,1,U,24,JST
		dc.w	NXTSP
		dc.w	SPD,0,ST,WCXY,100,AN,173,177,1,R,350,CC,100
		dc.w 	P,0,0,JST	
		dc.w	NXTSP


;screen 27
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 28
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,255,624,AN,162,165,10,AD,2,ST,R,354,L,255,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,352,744,AN,162,165,10,AD,2,ST,L,47,R,352,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,47,744,AN,162,165,10,AD,2,ST,R,352,L,47,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,32,96,AD,2,ST,AN,125,128,4,R,367
		dc.w	AN,129,132,4,L,32,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 29
		dc.w	NXTSCRN
		dc.w	NB,8,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,367,32,XYD,20,SCX,100,101,102,103,104,0
		dc.w	ST,AN,129,132,4,L,32,AN,125,128,4,R,367,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,367,32,XYD,20,SCX,100,101,102,103,104,0,W,80
		dc.w	ST,AN,129,132,4,L,32,AN,125,128,4,R,367,JST
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,ST,WCXY,100,AN,133,135,4,D,800,CC,100
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,ST,WCXY,101,AN,133,135,4,D,800,CC,101
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,ST,WCXY,102,AN,133,135,4,D,800,CC,102
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,ST,WCXY,103,AN,133,135,4,D,800,CC,103
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,0,SPD2,2,ST,WCXY,104,AN,133,135,4,D,800,CC,104
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		

;screen 30
		dc.w	NXTSCRN
		dc.w	NB,14,PD,275,4,PW
		dc.w	NXTSP
		dc.w	SPD,1,P,42,48,XYD,20,SCX,100,101,0
		dc.w	AN,178,181,4,ST,AD,0,R,358,AD,1,L,42,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,358,792,XYD,20,SCX,102,103,0
		dc.w	AN,178,181,4,ST,AD,1,L,42,AD,0,R,358,JST
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,100,AN,173,177,2,SD,10,3,D,800,CC,100
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,101,AN,173,177,2,SD,10,3,D,800,CC,101
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,102,AN,173,177,2,SD,10,3,U,0,CC,102
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,103,AN,173,177,2,SD,10,3,U,0,CC,103
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,P,42,48,XYD,20,SCY,104,105,0
		dc.w	AN,169,172,4,ST,AD,0,D,792,AD,1,U,48,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,358,792,XYD,20,SCY,106,107,0
		dc.w	AN,169,172,4,ST,AD,1,U,48,AD,0,D,792,JST
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,104,AN,173,177,2,SD,10,3,R,360,CC,104
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,105,AN,173,177,2,R,SD,10,3,360,CC,105
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,106,AN,173,177,2,L,SD,10,3,35,CC,106
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,1,ST,WCXY,107,AN,173,177,2,L,SD,10,3,35,CC,107
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	SPD,0,ST,W,80,P,306,0,AN,109,112,4,D,144,SD,9,2,L,116
		dc.w	D,272,AD,1,SD,9,2,R,300,D,400,FP,109,AD,0,SD,9,2,L,116
		dc.w	D,528,SD,9,2,AN,109,112,4,AD,1,R,300,D,656,AD,0,L,196
		dc.w	D,800,P,0,0,JST
		dc.w	NXTSP


;screen 31
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,760,PW
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,ST,AN,0,9,2,W,50,R,346,SKCS,22,FP,204
		dc.w	LP,4,W,10,FP,203,SD,21,2,W,10,FP,204,JLP
		dc.w	SC,22,FP,0,W,80,AN,0,9,2,SKIP,R,370,CC,22,W,80	
		dc.w	LP,4,SD,21,2,W,20,JLP,SC,22,W,80,AN,10,19,2,L,350
		dc.w	CC,22,L,30,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 32
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,760,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,ST,AN,0,9,2,W,50,SKCS,22,LP,4,SD,21,3
		dc.w	W,20,JLP,SC,22,W,80,SKIP,R,60,CC,22,R,346,SKCS,23
		dc.w	FP,204,LP,4,W,10,FP,203,SD,21,2,W,10,FP,204,JLP
		dc.w	SC,23,FP,0,W,80,AN,0,9,2,SKIP,R,370,CC,23,W,50	
		dc.w	LP,4,SD,21,2,W,20,JLP,SC,23,W,80,AN,10,19,2,L,350
		dc.w	CC,23,L,52,SKCS,22,FP,205,LP,4,W,10,FP,206,SD,21,3,W,10
		dc.w	FP,207,JLP,SC,22,W,80,AN,10,19,2,SKIP,L,30,CC,22,JST
		dc.w	NXTSP
		dc.w	P,130,768,ST,SPD2,2,AN,148,150,8,AD,2,W,180,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,210,768,ST,SPD2,2,AN,148,150,8,AD,2,W,100,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 33
		dc.w	NXTSCRN
		dc.w	NB,8,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,0,P,30,768,ST,AN,0,9,2,W,50,SKCS,23,LP,4,SD,21,3
		dc.w	W,20,JLP,SC,23,W,80,SKIP,R,60,CC,23,R,346,SKCS,24
		dc.w	FP,204,LP,4,W,10,FP,203,SD,21,2,W,10,FP,204,JLP
		dc.w	SC,24,FP,0,W,80,AN,0,9,2,SKIP,R,370,CC,24,W,50	
		dc.w	LP,4,SD,21,2,W,20,JLP,SC,24,W,80,AN,10,19,2,L,350
		dc.w	CC,24,L,52,SKCS,23,FP,205,LP,4,W,10,FP,206,SD,21,3,W,10
		dc.w	FP,207,JLP,SC,23,W,80,AN,10,19,2,SKIP,L,30,CC,23,JST
		dc.w	NXTSP
		dc.w	P,130,768,ST,SPD2,2,AN,148,150,8,AD,2,W,260,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,170,768,ST,SPD2,2,AN,148,150,8,AD,2,W,220,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,210,768,ST,SPD2,2,AN,148,150,8,AD,2,W,180,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,250,768,ST,SPD2,2,AN,148,150,8,AD,2,W,140,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP
		dc.w	P,290,768,ST,SPD2,2,AN,148,150,8,AD,2,W,100,AW,150,AD,0
		dc.w	AN,151,153,8,AW,153,FP,153,SD,1,3,U,40,SLDN,1,U,0,SLDN,0
		dc.w	SPD2,3,D,768,AN,153,151,8,AD,1,AW,151,JST
		dc.w	NXTSP

;screen 34
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,1,P,80,24,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,128,24,AN,169,172,4,ST,W,50,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,176,24,AN,169,172,4,ST,W,50,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,224,24,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP

;screen 35
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,1,P,80,348,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,128,68,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,W,50,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,176,68,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,W,50,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,224,348,AN,169,172,4,ST,AD,0,D,852,AD,1,U,24,JST
		dc.w	NXTSP

;screen 36
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,1,P,224,728,AN,169,172,4,ST,AD,1,U,24,AD,0,D,728,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,176,472,AN,169,172,4,ST,AD,1,U,24,AD,0,D,472,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,256,696,AN,162,165,8,AD,2,ST,D,760,R,352,U,696
		dc.w	L,256,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 37
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	P,247,136,ST,LP,4,FP,98,W,50,ANC,1,AD,2,AN,98,100,10
		dc.w	AW,98,W,100,JLP,SD,18,3,W,50,AN,101,104,5,ANC,0,SPD,1,D,772
		dc.w	LP,3,FP,98,W,50,ANC,1,AD,2,AN,98,100,10,AW,98
		dc.w	W,100,JLP,SD,18,3,W,50,AN,87,90,4,ANC,0,SPD,0,R,368,W,100
		dc.w	SPD,1,P,31,136,SD,18,3,R,211,AN,101,104,4,R,247,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,256,148,AN,58,59,15,ST,WC,19,D,768,W,50,U,148,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,W,20,ST,L,47,R,368,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,W,130,ST,L,47,R,368,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,W,240,ST,L,47,R,368,JST
		dc.w	NXTSP

;screen 38
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,ST,L,30,R,368,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,W,110,ST,L,30,R,368,JST
		dc.w	NXTSP
		dc.w	SPD,0,P,368,752,AN,162,165,6,AD,2,W,220,ST,L,30,R,368,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP


;screen 39
		dc.w	NXTSCRN
		dc.w	NB,8,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	SPD,1,P,297,192,AN,113,115,5,AD,2,ST,R,357,L,297,JST
		dc.w	NXTSP
		dc.w	SPD,1,P,357,768,AN,113,115,5,AD,2,ST,L,297,R,357,JST
		dc.w	NXTSP
		dc.w	FP,113,P,363,768,XYD,20,SCY,100,0,PW
		dc.w	NXTSP
		dc.w	SPD,0,ST,WCXY,100,AN,119,121,4,AD,2,SD,14,3,L,30,CC,100
		dc.w 	P,0,0,JST	
		dc.w	NXTSP
		dc.w	FP,113,P,10,64,XYD,20,SCY,101,0,PW
		dc.w	NXTSP
		dc.w	SPD,0,ST,WCXY,101,AN,119,121,4,AD,2,SD,14,3,R,363,CC,101
		dc.w 	P,0,0,JST	
		dc.w	NXTSP

;screen 40
		dc.w	NXTSCRN
		dc.w	NB,6,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	P,164,704,ST,AN,136,137,14,W,100,AN,137,142,8,AW,142,JST
		dc.w	NXTSP
		dc.w	ST,P,234,704,AN,136,137,14,W,120,P,223,704,AN,143,147,8
		dc.w	AW,147,JST
		dc.w	NXTSP
		dc.w	P,297,704,ST,AN,136,137,14,W,130,AN,137,142,8,AW,142,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 41
		dc.w	NXTSCRN
		dc.w	NB,6,PD,60,180,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,125,292,AN,58,59,15,ST,R,205,W,10,L,125,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,322,292,AN,58,59,15,ST,L,242,W,10,R,322,JST
		dc.w	NXTSP
		dc.w	LFT,SPD,1,P,346,800,AN,197,201,2,ST,U,64,SPD,2,AS,4
		dc.w	L,328,AS,6,D,192,AS,4,L,168,R,346,SPD,1,AS,6,D,292,W,50
		dc.w	D,580,SPD,0,AS,4,L,167,SPD,2,AS,6,D,800,AS,2,U,580
		dc.w	SPD,0,AS,4,R,346,SPD,1,AS,6,D,800,AS,2,JST
		dc.w	NXTSP
		dc.w	P,0,0,PW,NXTSP

;screen 42
		dc.w	NXTSCRN
		dc.w	NB,7,PD,275,4,PW
		dc.w	NXTSP
		dc.w	FP,80,P,190,180,PW	
		dc.w	NXTSP
		dc.w	LFT,SPD,0,AN,197,201,4,XYD,20,SCX,100,0,P,30,512,WC,21
		dc.w	R,240,WC,100,SC,101,L,114,PW
		dc.w	NXTSP
		dc.w	LFT,SPD,0,AN,197,201,4,P,30,512,WC,21,R,224,WC,101,L,98,PW
		dc.w	NXTSP
		dc.w	LFT,SPD,0,AN,197,201,4,P,30,512,WC,21,R,208,WC,101,L,82,PW
		dc.w	NXTSP
		dc.w	LFT,SPD,0,AN,197,201,4,P,30,512,W,600,R,192,WC,101,L,66,PW
		dc.w	NXTSP
		dc.w	LFT,SPD,0,AN,197,201,4,P,30,512,W,800,R,176,WC,101,L,50,PW
		dc.w	NXTSP



		dc.w	LSTSCRN	


;___________________________________________________________



;****** Sound FX Player Tables *******


;sample address in chip ram

sampptr	dc.l	0	    ;boing		1	
	dc.l	0	        ;church bell	2
	dc.l	0	        ;collect		3
	dc.l	0	        ;doorslide	4
	dc.l	0	        ;drip		5
	dc.l	0	        ;fanfare		6
	dc.l	0	        ;flames		7
	dc.l	0	        ;hu		    8
	dc.l	0	        ;thud		9
	dc.l	0	        ;laser		10
	dc.l	0	        ;pick		11
	dc.l	0	        ;quack		12
	dc.l	0	        ;slide		13
	dc.l	0	        ;big laser	14
	dc.l	0	        ;walk		15
	dc.l	0	        ;robot walk	16
	dc.l	0	        ;wau		17	
	dc.l	0	        ;bird		18	
	dc.l	0	        ;wooden creak	19	
	dc.l	0	        ;swallow		20
	dc.l	0	        ;knock		21
	dc.l	0	        ;argh		22
	

;dc.w	length,vol,period/rate

sampdef	dc.w	1394,64,570
sampde	dc.w	3379,64,428	2	
	dc.w	2389,64,350
	dc.w	1860,64,400	4
	dc.w	524,64,380
	dc.w	2976,64,400	6
	dc.w	2062,24,700
	dc.w	580,64,300	8
	dc.w	1514,64,400
	dc.w	431,64,900	10
	dc.w	324,64,700
	dc.w	1156,64,428	12
	dc.w	926,64,400
	dc.w	2987,64,380	14
	dc.w	868,32,170
	dc.w	1718,64,428	16
	dc.w	4013,64,428
	dc.w	1210,64,428	18
	dc.w	1900,64,600
	dc.w	1203,64,500	20
	dc.w	1514,64,240
	dc.w	1623,64,500	22

	dc.w	LASTSAMP	

SAMPDEFLENGTH	=	sampde-sampdef	

plrsampch0	dc.b	0	        ;requested sample for each channel
collectsampch1	dc.b	0
alien1sampch2	dc.b	0
alien2sampch3	dc.b	0

samppriority
	dc.b	5
	dc.b	5
	dc.b	5
	dc.b	5
	dc.b	5
	dc.b	5
	dc.b	5
	dc.b	5
	

;*************************************


;bob data storage area
	
	even
scrnctr		dc.w	0	
bobd		dc.l	0
maskd		dc.l	0	
bmod		dc.w	scwidth-bwidth4
xyptr		dc.l	0
Newscpos	dc.w	0
tmp1		dc.w	0
tmp1w		dc.w	0
tmp2w		dc.w	0	
tmp3w		dc.w	0
tmp1L		dc.l	0,0
tmp2L		dc.l	0
tmp3L		dc.l	0
dptrtmp		dc.l	0

	
num_bobs	dc.w	1
activ_bobs	dc.l	%00000000000000000000011111111111


tmp1B		dc.b	0
tmp2B		dc.b	0
flag		dc.b	0
scrnflag	dc.b	0		
collisionflag	dc.b	0
entryflag	dc.b	0
scoredelay	dc.b	0
pause		dc.b	0
newscreenflag	dc.b	0

condtab		dcb.b	maxcond
		even

objectctr	dc.w	0		    ;num objects needed to goto nxt scrn
runflag		dc.w	0
testnum1	dc.w	0
destscptr	dc.l	0
visibleptr	dc.l	0
crunchscptr	dc.l	0		    ;points to start of cur crunchscreen
bobxyptr	dc.l	0		    ;pointer to cur xy table
Binfotabsize	dc.l	0		;size (bytes) of entire table
binfoscrnptr	dc.l	0		;points to 1st scrnpos of cur scrn
binfoendptr	dc.l	0		    ;points to $ffff/end of table marker
binfotabptr	dc.l	0		    ;start of cur infotab screen


;___________________________________________________________
;screen & bob"gfx data



dmastore	dc.w	0
intenastore	dc.w	0
adkonstore	dc.w	0
copper1store	dc.l	0
copper2store	dc.l	0

bnumconvert	dcb.l	blockWd*16		;cvrts blk nos to blockscrn addrs
plotoffset	dcb.l	NumBlocks		;block nums to plot pos'n offsets
fnumconvert	dcb.l	Maxframe		;cvrts bob frame numbers to memory addresses

;scrnnum,bcomtype,scpos,Hi+st,anistat,Spd,Locn,newscrn,mask,score,repl,btype


entryscreen	dcb.b	newentrylength*numblocks


btypescreentab	dcb.b	numblocks*2
;-format = btype,combtype,btype,combtype,.....

btabB		dcb.w	(numblocks*2)+4
btabMoB		dcb.w	(numblocks*2)+4
btabMcB		dcb.w	(numblocks*2)+4
banitabI	dcb.w	(numblocks*2)+4
banitabB	dcb.w	(numblocks*2)+4
banitabLMoI	dcb.w	(numblocks*2)+4
banitabLMcI	dcb.w	(numblocks*2)+4
banitabLMoB	dcb.w	(numblocks*2)+4
banitabLMcB	dcb.w	(numblocks*2)+4
banitabLCoI	dcb.w	(numblocks*2)+4
banitabLCcI	dcb.w	(numblocks*2)+4
banitabLCoB	dcb.w	(numblocks*2)+4
banitabLCcB	dcb.w	(numblocks*2)+4



btabBptr	dc.l	0	
btabMoBptr	dc.l	0
btabMcBptr	dc.l	0
banitabIptr	dc.l	0
banitabBptr	dc.l	0
banitabLMoIptr	dc.l	0
banitabLMcIptr	dc.l	0
banitabLMoBptr	dc.l	0
banitabLMcBptr	dc.l	0
banitabLCoIptr	dc.l	0
banitabLCcIptr	dc.l	0
banitabLCoBptr	dc.l	0
banitabLCcBptr	dc.l	0

colbarspst	dc.w	0
colbarf		dc.w	0
intfdflag	dc.w	0
testword	dc.w	0


crunchscrns	incbin	"PCProg:Data/PlatGFX/crunchscreens.lev_1"
colloffset	incbin	"PCProg:Data/PlatGFX/collisiontable"
font		incbin	"PCProg:Data/PlatGFX/gamefont.R"
blocktypetab	incbin	"PCProg:Data/PlatGFX/blocktypetab"
blockinfotab	incbin	"PCProg:Data/PlatGFX/blockinfotab"	

;crunchscrns	incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/crunchscreens.lev_1"
;colloffset	incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/collisiontable"
;font		incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/gamefont.R"
;blocktypetab	incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/blocktypetab"
;blockinfotab	incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/blockinfotab"	

alphabet	dc.b	"ABCDEFGHIJKLMNOPQRSTUVWXYZ: 0123456789"
scoretxt	dc.b	"SCORE:",0
livestxt	dc.b	"LIVES:",0
introtxt1	dc.b	"TRIAL  BY  ENDURANCE ",0
introtxt2	dc.b	"PRESS FIRE TO PLAY",0

screennumtxt	dc.b	"SCRN:",0

	even

;******************************************************
;***** The following data is located in CHIP ram ****** 
;******************************************************

;	SECTION gfxetc,data_C


introcop	;intro screen copper list :-
	dc.w	$1fc,0		    ;AGA sprites reset
;	dc.w	$106,$c000	    ;AGA sprts,pallet,dplfld reset
;	dc.w	$10c,$11

intcol	dc.w	$180,0,$182,0,$184,0,$186,0,$188,0
	dc.w 	$18a,0,$18c,0,$18e,0,$190,0,$192,0
	dc.w 	$194,0,$196,0,$198,0,$19a,0,$19c,0
	dc.w 	$19e,0

spt0ptr dc.w	$120,0,$122,0	    ;hardware sprite pointers
	dc.w	$124,0,$126,0
	dc.w	$128,0,$12a,0
	dc.w	$12c,0,$12e,0
	dc.w	$130,0,$132,0
	dc.w	$134,0,$136,0
	dc.w	$138,0,$13a,0
	dc.w	$13c,0,$13e,0

;*** Display Backdrop ***

	dc.w	diwstrt,$0581
	dc.w	bplcon0,$0200
	dc.w	bplcon2,0
	dc.w	diwstop,$40c1
	dc.w	ddfstrt,$0038
	dc.w	ddfstop,$00d0
	dc.w	bplcon1,0
	dc.w	bpl1mod,(scwidth*PLANES)-40
	dc.w	bpl2mod,(scwidth*PLANES)-40
 
intpl1	dc.w 	$e0,0,$e2,0
intpl2	dc.w	$e4,0,$e6,0
intpl3	dc.w	$e8,0,$ea,0
intpl4	dc.w	$ec,0,$ee,0

	dc.w	$280f,$fffe
	dc.w	bplcon0,$4200
	dc.w	$ff0f,$fffe
	dc.w	bplcon0,$0200
	dc.w	$ffff,$fffe		    ;end of intro copper list

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;In-game copper list :-

gamecop	
	dc.w	$1fc,0		    ;AGA sprites reset
;	dc.w	$106,$c000	    ;AGA sprts,pallet,dplfld reset
;	dc.w	$10c,$11
gamecol	dc.w	$180,0,$182,0,$184,0,$186,0,$188,0	colour regs
	dc.w 	$18a,0,$18c,0,$18e,0,$190,0,$192,0
	dc.w 	$194,0,$196,0,$198,0,$19a,0,$19c,0
	dc.w 	$19e,0
sp0ptr	dc.w	$120,0,$122,0	    ;hardware sprite pointers
	dc.w	$124,0,$126,0
	dc.w	$128,0,$12a,0
	dc.w	$12c,0,$12e,0
	dc.w	$130,0,$132,0
	dc.w	$134,0,$136,0
	dc.w	$138,0,$13a,0
	dc.w	$13c,0,$13e,0


;*** Display Backdrop ***

	dc.w	diwstrt,$0581
	dc.w	bplcon0,$0200
	dc.w	bplcon2,$0000
	dc.w	diwstop,$40c1
	dc.w	ddfstrt,$0038
	dc.w	ddfstop,$00d0
	dc.w	bplcon1,0
	dc.w	bpl1mod,(scwidth*PLANES)-40
	dc.w	bpl2mod,(scwidth*PLANES)-40
 
Plane1	dc.w 	$e0,0,$e2,0
plane2	dc.w	$e4,0,$e6,0
plane3	dc.w	$e8,0,$ea,0
plane4	dc.w	$ec,0,$ee,0

	dc.w	$280f,$fffe
	dc.w	bplcon0,$4200
	dc.w	$ffdf,$fffe

	dc.w	$140f,$fffe
	dc.w	$180
colbar	dc.w	0	
	dc.w	$1b0f,$fffe
	dc.w	$180,0
	dc.w	$200f,$fffe
	dc.w	$180
coltest	dc.w	$0			;debug colour bar on
	dc.w	$240f,$fffe
	dc.w	$180,0			;debug colour bar off

	dc.w	bplcon0,$0200
	dc.w	$ffff,$fffe		;end of copper list

copend

blanksfx	dc.l	0

CLISTSIZE	=	copend-gamecop

;screen1+screen2+restscreen = 159744 bytes (52*256*4 * 3 screens)
;1 bob frame screen+mask screen=81,920 bytes (40*256*4*2)
;block data gfx = 40960 bytes (40*256*4)
;gfx font =38 bytes wide * 8 * 4 =1216 bytes
;TOTAL CHIP MEMORY USED = 283,840 bytes (excl. sound + copper list)



screen1		dcb.b	scwidth*scheight*planes
screen2		dcb.b	scwidth*scheight*planes
restscreen	dcb.b	scwidth*scheight*planes

scsize	=	screen2-screen1

maskbuffer	dcb.w	16*4



bobdata		incbin	"PCProg:Data/PlatGFX/bobs_16col_.ilbm"
mask		incbin	"PCProg:Data/PlatGFX/bobs_16col_.m"
blockdata	incbin	"PCProg:Data/PlatGFX/blockscreen.ILBM"
sfx			incbin	"PCProg:Data/PlatSFX/sfx"
		
;bobdata		incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/bobs_16col_.ilbm"
;mask		incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/bobs_16col_.m"
;blockdata	incbin	"J:\A1200 WinUAE Folder/Data/PlatGFX/blockscreen.ILBM"
;sfx			incbin	"J:\A1200 WinUAE Folder/Data/PlatSFX/sfx"


