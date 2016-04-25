(
	dialog.f
	Example of loading a dialog
	/Mic, 2004
)


include ..\..\include\windef.f

variable done
variable hctrl
variable buffer 64 allot

101 constant IDD_DIALOG1                     
102 constant IDR_MENU1                       
1000 constant IDC_EDIT1                       
1001 constant IDC_CHECK1                      
1002 constant IDC_COMBO1



: dlg-func { hwndDlg uMsg wParam lParam -- }
	uMsg WM_COMMAND = if
		wParam 0xffff and case
			IDOK of
				z" You entered " buffer 12 cmove
				IDC_EDIT1 hwndDlg call GetDlgItem a@ hctrl !
				32 buffer 12 + hctrl @ call GetWindowText
				MB_OK z" Finished" buffer 0 call MessageBox
				1 done !
				0 hwndDlg call EndDialog
				1 exit
				endof
			IDCANCEL of
				1 done !
				1 hwndDlg call EndDialog
				0 exit
				endof
			IDCLOSE of
				1 done !
				1 hwndDlg call EndDialog
				0 exit
				endof
		endcase
	else uMsg WM_INITDIALOG = if
		IDC_COMBO1 hwndDlg call GetDlgItem a@ hctrl !
		z" Item 1" 0 CB_ADDSTRING hctrl @ call SendMessage
		z" Item 2" 0 CB_ADDSTRING hctrl @ call SendMessage
		z" Item 3" 0 CB_ADDSTRING hctrl @ call SendMessage
		0 0 CB_SETCURSEL hctrl @ call SendMessage
		
		IDC_EDIT1 hwndDlg call GetDlgItem a@ hctrl !
		z" Edit" hctrl @ call SetWindowText
	then
	then
	drop drop drop drop 0 ;@
	


: win-main { hInst -- } 
	0 done !
	NULL ' dlg-func NULL IDD_DIALOG1 hInst call DialogBoxParam 
	begin 0 done = while
		100 call Sleep repeat drop ;

	

NULL call GetModuleHandle a@ win-main
bye 

