(
	Frequently used Windows constants
	Part of the ForthEC package
	/Mic, 2004
)


0 	constant DLL_PROCESS_DETACH                   
1 	constant DLL_PROCESS_ATTACH                   
2 	constant DLL_THREAD_ATTACH                    
3 	constant DLL_THREAD_DETACH                    

0	constant MB_OK
1 	constant MB_OKCANCEL                          
2 	constant MB_ABORTRETRYIGNORE                  
3 	constant MB_YESNOCANCEL                       
4 	constant MB_YESNO                             
5 	constant MB_RETRYCANCEL                     
0x10	constant MB_ICONHAND
MB_ICONHAND constant MB_ICONERROR                         

32512	constant IDC_ARROW                    
32513	constant IDI_IBEAM                            
32514	constant IDI_WAIT                       
32515	constant IDI_CROSS 

32512	constant IDI_APPLICATION                    
32513	constant IDI_HAND                            
32514	constant IDI_QUESTION                       
32515	constant IDI_EXCLAMATION                    
32516	constant IDI_ASTERISK                         
32517	constant IDI_WINLOGO         			  
IDI_EXCLAMATION constant IDI_WARNING     			
IDI_HAND constant IDI_ERROR       				 
IDI_ASTERISK constant IDI_INFORMATION

1 	constant IDOK                               
2 	constant IDCANCEL                            
3 	constant IDABORT                           
4 	constant IDRETRY                             
5 	constant IDIGNORE                            
6 	constant IDYES                               
7 	constant IDNO                               
8 	constant IDCLOSE                            
9 	constant IDHELP                              

0 	constant WM_NULL
1 	constant WM_CREATE
2 	constant WM_DESTROY
3 	constant WM_MOVE
5 	constant WM_SIZE
0x0f 	constant WM_PAINT
16 	constant WM_CLOSE
0x12 	constant WM_QUIT
0x100 	constant WM_KEYDOWN
0x110 	constant WM_INITDIALOG
0x111	constant WM_COMMAND
0x113	constant WM_TIMER

27 	constant VK_ESCAPE

1	constant CS_VREDRAW                          
2 	constant CS_HREDRAW                           
4 	constant CS_KEYCVTWINDOW                      
8 	constant CS_DBLCLKS                          
0x20	constant CS_OWNDC                             
0x40 	constant CS_CLASSDC                          
0x80 	constant CS_PARENTDC                          
0x100 	constant CS_NOKEYCVT                         
0x200 	constant CS_NOCLOSE                          

4	constant COLOR_MENU        
5	constant COLOR_WINDOW      
6	constant COLOR_WINDOWFRAME   
7	constant COLOR_MENUTEXT      
8	constant COLOR_WINDOWTEXT    
15	constant COLOR_BTNFACE                        
16	constant COLOR_BTNSHADOW                      
17	constant COLOR_GRAYTEXT                       
18	constant COLOR_BTNTEXT                        

0x80000000 constant CW_USEDEFAULT

0x4000000 constant WS_CLIPSIBLINGS
0x2000000 constant WS_CLIPCHILDREN
0xcf0000  constant WS_OVERLAPPEDWINDOW
0xCE0000  constant WS_OVERLAPPED_NOMAX		    

0x143	constant CB_ADDSTRING
0x147	constant CB_GETCURSEL
0x14E	constant CB_SETCURSEL

10 	constant SW_SHOWDEFAULT

0	constant PM_NOREMOVE


0 constant OUT_DEFAULT_PRECIS                   
1 constant OUT_STRING_PRECIS                    
2 constant OUT_CHARACTER_PRECIS                
3 constant OUT_STROKE_PRECIS                    
4 constant OUT_TT_PRECIS                        
5 constant OUT_DEVICE_PRECIS                    
6 constant OUT_RASTER_PRECIS                    
7 constant OUT_TT_ONLY_PRECIS                   
8 constant OUT_OUTLINE_PRECIS                   
0 constant CLIP_DEFAULT_PRECIS                  
1 constant CLIP_CHARACTER_PRECIS                
2 constant CLIP_STROKE_PRECIS                   
0xF constant CLIP_MASK                          
16 constant CLIP_LH_ANGLES                      
32 constant CLIP_TT_ALWAYS                      
128 constant CLIP_EMBEDDED                      
0 constant DEFAULT_QUALITY                      
1 constant DRAFT_QUALITY                        
2 constant PROOF_QUALITY
4 constant ANTIALIASED_QUALITY
0 constant DEFAULT_PITCH                        
1 constant FIXED_PITCH                          
2 constant VARIABLE_PITCH                       
0 constant ANSI_CHARSET                         
1 constant DEFAULT_CHARSET                      

0 constant FF_DONTCARE                         
16 constant FF_ROMAN                             
32 constant FF_SWISS                             
48 constant FF_MODERN                            
64 constant FF_SCRIPT                            
80 constant FF_DECORATIVE                        
0 constant FW_DONTCARE                          
100 constant FW_THIN                              
200 constant FW_EXTRALIGHT                        
300 constant FW_LIGHT                             
400 constant FW_NORMAL                            
500 constant FW_MEDIUM                            
600 constant FW_SEMIBOLD                          
700 constant FW_BOLD                              
800 constant FW_EXTRABOLD                         
900 constant FW_HEAVY                             


0 constant DIB_RGB_COLORS                       
1 constant DIB_PAL_COLORS                       
2 constant DIB_PAL_INDICES                      
2 constant DIB_PAL_PHYSINDICES                  
4 constant DIB_PAL_LOGINDICES                   

0 constant BI_RGB                              
1 constant BI_RLE8                             
2 constant BI_RLE4                              
3 constant BI_BITFIELDS                         

0x0CC0020 constant SRCCOPY                              
0x0EE0086 constant SRCPAINT                            


0x40 constant LMEM_ZEROINIT
