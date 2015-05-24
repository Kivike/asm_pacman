;Pacman
BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h

; starting address of video memory

videobase				EQU 0a000h

; some colors

black						EQU 0
green						EQU 00110000b
blue						EQU 00001001b
red							EQU 00000100b
white						EQU 00001111b
grey						EQU 00000111b
yellow					EQU 00001110b
transparent			EQU 11111111b

scrwidth EQU 320

;;;;;;;;;;;;;;
; DATA AND STACK SEGMENTS
;;;;;;;;;;;;;;

segment memscreen data
	resb 320*200

segment background data
	resb 320*200
		
segment mystack stack
	resb stacksize
stacktop:	
	
segment mydata data
	oldintseg resw 1
	oldintoff resw 1
	oldvideomode resw 1
	pressesc resw 1
	foo	resw	1
	

;;;;;;;;;;;;;;
; The code segment - YOUR CODE HERE
;;;;;;;;;;;;;;

segment mycode code

KeybInt:
        push    ds              ; push the value of ds and ax to safety
        push    ax              
        mov     ax,mydata       ; Re-initialisation of 
        mov     ds,ax           ; the data segment
        cli                     ; Disable other interrupts
	                            ; during this one

.getstatus:
        in      al, 64h
        test	al, 02h
        loopnz	.getstatus      ; wait until the port is ready
        in      al,60h          ; Get the scan code of the pressed/released key

	    ; here begins the actual key scanning
        cmp     al, 01h		    ; 1 is the 'make code' for ESC
        jne     .cplus		    ; if ESC was not pressed, continue
        mov     word [pressesc], 1
	    jmp    .kbread
.cplus:
	    cmp 	al, 04Eh	; 4E is the 'make code' for keypad plus
	    jne 	.cminus
	    ;something here later
	    jmp     .kbread
.cminus:
	    cmp 	al, 04Ah	; 4A is the 'make code' for keypad minus
	    jne	.kbread
	    ;something here later

.kbread:
        in      al,61h        ; Send acknowledgment without
        or      al,10000000b  ; modifying the other bits.
        out     61h,al        ;                            
        and     al,01111111b  ;                            
        out     61h,al        ;                            
        mov     al,20h        ; Send End-of-Interrupt signal 
        out     20h,al        ;                              
        sti                   ; Enable interrupts again
        pop     ax		
   	    pop 	ds       				; Regain the ds,ax from stack
        iret	                ; Return from interrupt
		



; takeInput:
 
	; mov ah, 9                    ;DOS: print string
	; int 21h
 
	; mov	ah, 1					;DOS: get character
 	; int	21h
 	; or	al, 20h					;to lowercase
 
 ; ; Check keypresses and change the right text
	; cmp al, 'w'					
		; je Up					

 	; cmp	al, 'a'					
		; je Left					
	
 	; cmp	al, 's'					
		; je Down
	
	; cmp al, 'd'					
		; je Right		

 	; jnz	takeInput
 
 
; Up: 
 ; mov dx, up
 ; je  takeInput

; Left:
 ; mov dx, left
 ; je  takeInput

; Down:
 ; mov dx, down
 ; je  takeInput
 
; Right:
 ; mov dx, right
 ; je  takeInput
 
copybackground:
	push ds
	pusha
	;Pointers
	mov word si, 0
	mov word di, 0
	mov cx,64000

	;Segment registers to correct locations
	mov ax,memscreen						; Destination segment
	mov es,ax
	mov ax,background						; Source segment
	mov ds,ax
	
	;REPEAT COPY!
	rep movsb										; Move byte at address ds:si to address es:di
	popa
	pop ds
	ret
	
drawPacman:
	ret
	
copymemscreen:
	push ds
	pusha
	;Pointers
	mov word si, 0
	mov word di, 0
	mov cx,64000

	;Segment registers to correct locations
	mov ax,videobase					; Destination segment
	mov es,ax
	mov ax,background						; Source segment
	mov ds,ax
	
	;REPEAT COPY!
	rep movsb										; Move byte at address ds:si to address es:di
	popa
	pop ds
	ret

initbackground:
	mov cx,0
	mov byte[background],blue
	mov byte[background+1],blue
	ret
	
drawrow:
	cmp cx,150
	je drawrowdone

drawpixel:
	mov bx,background
	add bx,cx
	mov byte[bx],blue
	
	jmp drawpixeldone
	
	
drawpixeldone:
  inc cx
	jmp drawrow
	
drawrowdone:
	ret
	

draw:
	;call copybackground
	call drawPacman
	call copymemscreen
	ret
	
drawsinglepixel:
	mov ax,videobase
	mov es,ax										;move video memory address to ESC
	mov di,0										;move the desired offset address to DI
	mov byte[es:di],blue				;move the constant 'blue' to the video memroy at offset DI
	inc di											;inc offset
	mov byte[es:di],white				;paint another pixel
	mov cl,[background+1]
	mov byte[es:0],cl
	ret
	
..start:
		
	mov ax, mydata
	mov ds, ax
	mov ax, mystack
	mov ss, ax
	mov sp, stacktop
	
	mov ah,35h
	mov al,9
	int 21h											;Vanhat arvot talteen -> es:bx
	mov [oldintseg],es
	mov [oldintoff],bx
	
	push ds
	mov dx,KeybInt							; Oman keyboard interruptin alkuaddress
	mov bx,mycode								
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h											;Asetetaan oma keyboard interrupt
	pop ds
	
	mov ah,0fh
	int 10h											;Haetaan nykyinen videomode
	mov [oldvideomode],ah
	
	mov ah,00h
	mov al,13h
	int 10h											;Asetetaan uusi videomode
	
	call initbackground
	
.mainloop:

	call draw
	call drawsinglepixel
	;mov ax,videobase
	;mov es,ax										;move video memory address to ESC
	;mov di,0										;move the desired offset address to DI
	;mov byte[es:di],blue				;move the constant 'blue' to the video memroy at offset DI
	;inc di											;inc offset
	;mov byte[es:di],white				;paint another pixel
	;mov cl,[background+1]
	;mov byte[es:0],cl
	
	
	
	cmp word [pressesc],1
	jne .mainloop

	
.dosexit:
	mov word dx, [oldintoff]
	mov word bx, [oldintseg]
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h											;Vanhat arvot takas
	
	mov word dx, [oldvideomode]
	mov ah,00h
	mov al,13h
	int 10h											;Vanha videomode takas
	
	mov	al, 0
	mov     ah, 4ch
	int     21h

.end
