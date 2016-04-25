\	vdp.f
\	SEGA Saturn VDP 1 & 2 constants and functions


\ VDP1 memory and registers 
0x25C00000 constant #VDP1_VRAM   
0x25C80000 constant #VDP1_FB     
0x25D00000 constant #TVMR        
0x25D00002 constant #FBCR        
0x25D00004 constant #PTMR       
0x25D00006 constant #EWDR        
0x25D00008 constant #EWLR        
0x25D0000A constant #EWRR       
0x25D0000C constant #ENDR       

0x25D00010 constant #EDSR       
0x25D00012 constant #LOPR       
0x25D00014 constant #COPR       
0x25D00016 constant #MODR       


\ VDP2 memory and registers 
0x25E00000 constant #VDP2_VRAM   
0x25F00000 constant #VDP2_CRAM   
0x25F80000 constant #VDP2_REG   

0x25F80000 constant #TVMD       
0x25F80002 constant #EXTEN       
0x25F80004 constant #TVSTAT     
0x25F80006 constant #VRSIZE     
0x25F80008 constant #HCNT       
0x25F8000A constant #VCNT       
0x25F8000E constant #RAMCTL     

0x25F80010 constant #CYCA0L     
0x25F80012 constant #CYCA0U      
0x25F80014 constant #CYCA1L     
0x25F80016 constant #CYCA1U      
0x25F80018 constant #CYCB0L     
0x25F8001A constant #CYCB0U     
0x25F8001C constant #CYCB1L      
0x25F8001E constant #CYCB1U     

0x25F80020 constant #BGON        
0x25F80022 constant #MZCTL      
0x25F80024 constant #SFSEL      
0x25F80026 constant #SFCODE     
0x25F80028 constant #CHCTLA      
0x25F8002A constant #CHCTLB     
0x25F8002C constant #BMPNA       
0x25F8002E constant #BMPNB      

0x25F80030 constant #PNCN0      
0x25F80032 constant #PNCN1      
0x25F80034 constant #PNCN2      
0x25F80036 constant #PNCN3      
0x25F80038 constant #PNCR       
0x25F8003A constant #PLSZ        
0x25F8003C constant #MPOFN      
0x25F8003E constant #MPOFR      

0x25F80040 constant #MPABN0      
0x25F80042 constant #MPCDN0     
0x25F80044 constant #MPABN1      
0x25F80046 constant #MPCDN1      
0x25F80048 constant #MPABN2      
0x25F8004A constant #MPCDN2      
0x25F8004C constant #MPABN3    
0x25F8004E constant #MPCDN3     

0x25F80050 constant #MPABRA     
0x25F80052 constant #MPCDRA     
0x25F80054 constant #MPEFRA     
0x25F80056 constant #MPGHRA    
0x25F80058 constant #MPIJRA      
0x25F8005A constant #MPKLRA     
0x25F8005C constant #MPMNRA     
0x25F8005E constant #MPOPRA      

0x25F80060 constant #MPABRB     
0x25F80062 constant #MPCDRB     
0x25F80064 constant #MPEFRB      
0x25F80066 constant #MPGHRB     
0x25F80068 constant #MPIJRB      
0x25F8006A constant #MPKLRB      
0x25F8006C constant #MPMNRB     
0x25F8006E constant #MPOPRB     

0x25F80070 constant #SCXIN0     
0x25F80072 constant #SCXDN0      
0x25F80074 constant #SCYIN0     
0x25F80076 constant #SCYDN0     
0x25F80078 constant #ZMXIN0      
0x25F8007A constant #ZMXDN0    
0x25F8007C constant #ZMYIN0     
0x25F8007E constant #ZMYDN0     

0x25F80080 constant #SCXIN1      
0x25F80082 constant #SCXDN1     
0x25F80084 constant #SCYIN1      
0x25F80086 constant #SCYDN1     
0x25F80088 constant #ZMXIN1      
0x25F8008A constant #ZMXDN1    
0x25F8008C constant #ZMYIN1     
0x25F8008E constant #ZMYDN1      

0x25F80090 constant #SCXN2      
0x25F80092 constant #SCYN2      
0x25F80094 constant #SCXN3      
0x25F80096 constant #SCYN3      
0x25F80098 constant #ZMCTL      
0x25F8009A constant #SCRCTL     
0x25F8009C constant #VCSTAU     
0x25F8009E constant #VCSTAL      

0x25F800A0 constant #LSTA0U     
0x25F800A2 constant #LSTA0L     
0x25F800A4 constant #LSTA1U    
0x25F800A6 constant #LSTA1L     
0x25F800A8 constant #LCTAU      
0x25F800AA constant #LCTAL      
0x25F800AC constant #BKTAU     
0x25F800AE constant #BKTAL     

0x25F800B0 constant #RPMD      
0x25F800B2 constant #RPRCTL     
0x25F800B4 constant #KTCTL    
0x25F800B6 constant #KTAOF     
0x25F800B8 constant #OVPNRA     
0x25F800BA constant #OVPNRB     
0x25F800BC constant #RPTAU      
0x25F800BE constant #RPTAL      

0x25F800C0 constant #WPSX0       
0x25F800C2 constant #WPSY0       
0x25F800C4 constant #WPEX0      
0x25F800C6 constant #WPEY0      
0x25F800C8 constant #WPSX1       
0x25F800CA constant #WPSY1      
0x25F800CC constant #WPEX1       
0x25F800CE constant #WPEY1      

0x25F800D0 constant #WCTLA      
0x25F800D2 constant #WCTLB      
0x25F800D4 constant #WCTLC      
0x25F800D6 constant #WCTLD      
0x25F800D8 constant #LWTA0U     
0x25F800DA constant #LWTA0L      
0x25F800DC constant #LWTA1U     
0x25F800DE constant #LWTA1L     

0x25F800E0 constant #SPCTL       
0x25F800E2 constant #SDCTL     
0x25F800E4 constant #CRAOFA     
0x25F800E6 constant #CRAOFB      
0x25F800E8 constant #LNCLEN     
0x25F800EA constant #SFPRMD      
0x25F800EC constant #CCCTL       
0x25F800EE constant #SFCCMD     

0x25F800F0 constant #PRISA     
0x25F800F2 constant #PRISB      
0x25F800F4 constant #PRISC       
0x25F800F6 constant #PRISD     
0x25F800F8 constant #PRINA       
0x25F800FA constant #PRINB     
0x25F800FC constant #PRIR     
0x25F800FE constant #RESERVE    

0x25F80100 constant #CCRSA      
0x25F80102 constant #CCRSB       
0x25F80104 constant #CCRSC      
0x25F80106 constant #CCRSD       
0x25F80108 constant #CCRNA       
0x25F8010A constant #CCRNB       
0x25F8010C constant #CCRR       
0x25F8010E constant #CCRLB       


\ Init VDP2
: vdp-init ( -- )
	0 	#TVMD w!
	#RAMCTL dup w@ 0xCFFF and swap w!
	0 	#MPOFN w!
	0x1A 	#CHCTLA w!
	
	0 	#SCXIN0 w!
	0 	#SCXDN0 w!
	0 	#SCYIN0 w!

	0x0001 	#BGON w!
	
	\ Clear VRAM
	0x40000 0 do
		0x0000 #VDP2_VRAM i wa+ w! loop
	
	\ Clear CRAM
	0x8000 0 do
		0x0000 #VDP2_CRAM i wa+ w! loop ;

		
: vdp-shutdown ( -- )	
	0 #TVMD w!
	
	0x100 0 do
		0 #VDP2_REG i wa+ w! loop
	
	0x40000 0 do
		0 #VDP2_VRAM i wa+ w! loop
	
	0x800 0 do
		0 #VDP2_CRAM i wa+ w! loop ;



: wait-hblank-in ( -- )
	begin #TVSTAT w@ 4 and 0<> until ;


: wait-hblank-out ( -- )
	begin #TVSTAT w@ 4 and 0= until ;


: wait-vblank-in ( -- )
	begin #TVSTAT w@ 8 and 0<> until ;


: wait-vblank-out ( -- )
	begin #TVSTAT w@ 8 and 0= until ;

	
: wait-vblank ( -- )
	begin #TVSTAT w@ 8 and 0<> until 
	begin #TVSTAT w@ 8 and 0= until ;


: wait-hblank ( -- )
	begin #TVSTAT w@ 4 and 0<> until 
	begin #TVSTAT w@ 4 and 0= until ;

