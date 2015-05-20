; Pacman
BITS 16

segment stack stack
			resb 100h
			
segment data data

segment code code

..start:
	mov ah,00h			;Set Video Mode
	mov al,13h			;320x200 Graphics, 256 colors, 1 page
	int 10h				;Video reference interrupt 
	


.end