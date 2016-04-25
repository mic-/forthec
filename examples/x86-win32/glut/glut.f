(
	glut.f
 	A simple example of using GLUT with ForthEC
	/Mic, 2004
)


include ..\..\include\opengl32.f	\ Include some OpenGL constants

notouch				\ Tell the compiler not to touch the code (ie. just pass it on
				\ to the assembler).
include glut-masm\gl.inc
include glut-masm\glu.inc
include glut-masm\glut.inc

includelib opengl32.lib
includelib glu32.lib
includelib glut\glut-masm\glut32.lib

touch				\ Enable compilation again.



0 constant GLUT_RGB
2 constant GLUT_DOUBLE
z" GLUT/Forth. Press [Esc] to quit" constant #caption


variable argc
variable argv
fvariable theta
fvariable d-theta

	

\ Glut callbacks use the C calling convention, otherwise it would be necessary to use
\ drop to normalize the stack before returning.

: reshape { w h -- }
	h w 0 0 call glViewport ;


: keyfunc { keycode x y -- }
	\ Exit if the user hit the [Esc] key
	keycode 255 and 27 = if
		bye
	then ;
	
	
: display ( -- )
	GL_COLOR_BUFFER_BIT call glClear
	call glPushMatrix
	
	\ fpops pops an 8-byte floating point value from the floating point stack
	\ and stores it on the regular stack as a 4-byte float.
	1.0e0 fpops 0.0e0 fpops 0.0e0 fpops theta f@ fpops call glRotatef
	
	GL_QUADS call glBegin
		 0.1e0 fpops 0.1e0 fpops 1.0e0 fpops	call glColor3f
		-0.6e0 fpops	-0.6e0 fpops		call glVertex2f

		 1.0e0 fpops 0.1e0 fpops 0.1e0 fpops	call glColor3f
		-0.6e0 fpops	0.6e0 fpops		call glVertex2f

		 0.1e0 fpops 1.0e0 fpops 0.1e0 fpops	call glColor3f
		0.6e0 fpops	0.6e0 fpops		call glVertex2f

		 0.1e0 fpops 1.0e0 fpops 1.0e0 fpops	call glColor3f
		0.6e0 fpops	-0.6e0 fpops		call glVertex2f
	call glEnd
	call glPopMatrix
	
	call glutSwapBuffers
	
	\ Increase angle.
	theta f@ d-theta f@ f+ theta f! ;
	


0.0e0 theta f!
0.2e0 d-theta f!

call GetCommandLine a@ argv !
argv argc call glutInit

GLUT_DOUBLE GLUT_RGB or call glutInitDisplayMode
288 352 call glutInitWindowSize

#caption call glutCreateWindow

\ Set up the callbacks
' display call glutDisplayFunc
' display call glutIdleFunc
' reshape call glutReshapeFunc
' keyfunc call glutKeyboardFunc

call glutMainLoop


