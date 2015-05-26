;Pacman
BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h

; starting address of video memory

videobase				EQU 0a000h

; some colors

black				EQU 0
green				EQU 00110000b
blue				EQU 00001001b
red					EQU 00000100b
white				EQU 00001111b
grey				EQU 00000111b
yellow				EQU 00001110b
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

segment bitmaps data
	Ghost1 db transparent,transparent,red,red,red,red,red,red,transparent,transparent,transparent,red,red,red,red,red,red,red,red,transparent,red,red,red,red,red,red,red,red,red,red,red,red,white,blue,red,red,blue,white,red,red,red,red,white,black,red,red,black,white,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,transparent,red,red,red,red,transparent,red,red,red,transparent,transparent,transparent,red,red,transparent,transparent,transparent,red

	CoinBlock db transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,yellow,yellow,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,yellow,yellow,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent
	
	BlueBlock db blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue

	Pacman db yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow
	
	MapRow1 db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	MapRow2 db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,1
	MapRow3 db 1,2,1,2,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow4 db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow5 db 1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	MapRow6 db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow7 db 1,2,1,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,1,2,1
	MapRow8 db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow9 db 1,2,2,2,1,2,2,2,1,2,1,1,1,2,2,2,1,2,2,2,1
	MapRow10 db 1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1
	MapRow11 db 1,2,2,2,1,2,2,2,1,2,1,1,1,2,2,2,1,2,2,2,1
	MapRow12 db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow13 db 1,2,1,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,1,2,1
	MapRow14 db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow15 db 1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	MapRow16 db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow17 db 1,2,1,2,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow18 db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,1
	MapRow19 db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	
segment mydata data
	oldintseg resw 1
	oldintoff resw 1
	oldvideomode resw 1
	pressesc resw 1
	movedir resw 1
    pacmanloc resw 1

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
	    cmp 	al, 04Eh		; 4E is the 'make code' for keypad plus
	    jne 	.cminus
	    ;something here later
	    jmp     .kbread
.cminus:
	    cmp 	al, 04Ah		; 4A is the 'make code' for keypad minus
	    jne	.kbread
	    ;something here later

.kbread:
        in      al,61h          ; Send acknowledgment without
        or      al,10000000b    ; modifying the other bits.
        out     61h,al                                    
        and     al,01111111b                           
        out     61h,al                                   
        mov     al,20h          ; Send End-of-Interrupt signal 
        out     20h,al          ;                              
        sti                     ; Enable interrupts again
        pop     ax		
   	    pop 	ds       		; Regain the ds,ax from stack
        iret	                ; Return from interrupt
		
takeInput:
 
		mov ah, 9               ;DOS: print string
		int 21h
 
		mov	ah, 1				;DOS: get character
 		int	21h
 		or	al, 20h				;to lowercase
 		
		cmp al, 'w'				; Check keypresses and call the subroutine
			je Up					

	 	cmp	al, 'a'					
			je Left					
		
	 	cmp	al, 's'					
			je Down
		
		cmp al, 'd'					
			je Right		

	 	ret
 
 
 Up: 
		je  takeInput
 
 Left:
		je  takeInput
 
 Down:
		je  takeInput
	 
 Right:
		je  takeInput


; takeInput:
 
	; mov ah, 9				;DOS: print string
	; int 21h
 
	; mov	ah, 1			;DOS: get character
 	; int	21h
 	; or	al, 20h			;to lowercase
 
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
	mov ax,memscreen		; Destination segment
	mov es,ax
	mov ax,background		; Source segment
	mov ds,ax

	;REPEAT COPY!
	rep movsb				; Move byte at address ds:si to address es:di
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
	mov ax,videobase		; Destination segment
	mov es,ax
	mov ax,memscreen		; Source segment
	mov ds,ax
	;REPEAT COPY!
	rep movsb				; Move byte at address ds:si to address es:di
	popa
	pop ds
	ret

initbackground:
	push ds
	pusha
	mov ax,bitmaps				
	mov ds,ax					; ds = bitmaps = source segment
	mov di,0
	mov si,MapRow1				; si = current map block
	mov ax,background
	mov es,ax					; es = background = target segment
	mov dx,21					; columns = 21
	mov cx,19					; rows = 19
	
	.drawblockrow:
		push cx
		mov cx,dx
	.drawblockcolumn:
		push cx
		push dx
		mov cx,10
		mov dx,10
		
		mov byte bl,[ds:si]		; current mapblock
		push si
		cmp bl,1				; if mapblock is a wall
		je .setblueblock
		cmp bl,2
		je .setcoinblock
		cmp bl,0
		je .skip
		
		.setblueblock:
			mov bx,BlueBlock
			mov si,BlueBlock
			jmp .drawblock
		.setcoinblock:
			mov bx,CoinBlock
			mov si,CoinBlock
		.drawblock:
			call copybitmap
		
		.skip:
			pop si
			pop dx
			pop cx
			
			inc si
			add di,10
			loop .drawblockcolumn
		pop cx
		add di,2990			; Move to next row
		loop .drawblockrow
	popa
	pop ds
	ret

checkcollision:
	
	mov cx, 0             ;Boolean collision is false

	mov ax,pacmanloc      ;Check pacmans next movement
	add ax, movdir

	mov bx, [videobase + ax] ;Check collision with blue in the corners
	cmp bx, blue
	je collision

	mov bx, [videobase + ax + 10]
	cmp bx, blue
	je collision

	mov bx, [videobase + ax + 3200]
	cmp bx, blue
	je collision

	mov bx, [videobase + ax + 3210]
	cmp bx, blue
	je collision

	ret                  ;Return if no collision


collision:
	mov cx, 1           ;Boolean collision is true
	ret

checkIfEatCoins:			;Might not work with 90% chance
	
	mov ax,mydata			
	mov ds, ax				;Change datasegment
	mov bh,pacmanloc
	mov bl, [bh / 10]		;Get pacman location in tiles (divided by 10 (can you even do this?))
	mov ax,bitmaps						
	mov ds, ax				;Change datasegment to bitmaps
	mov ax, MapRow1 		;Get first map row				
	cmp [ax + bl], 2        ;Check If there is a coin at pacmans tile
	je eat                  ;Eat
	ret

eat:
	mov [ax + bl], 0
	ret


copybitmap:
	;PARAMETERS
    ;   SI contains the offset address of the bitmap
    ;   DI contains the target coordinate 
    ;   ES contains the target segment
    ;   CX contains the bitmap row count
    ;   DX contains the bitmap col count
	push ds
	pusha

	mov ax,bitmaps
	mov ds,ax

	.rowloop:
		push cx
		push di
		mov cx,dx
		.colloop:
			mov byte bl,[ds:si]
			cmp byte bl,transparent
			je .skip
			mov byte[es:di],bl
			.skip:
			inc di
			inc si
			loop .colloop
		pop di
		add di,320
		pop cx
		loop .rowloop
	popa
	pop ds
	ret
	
	
draw:
	; Creates the image by layering
	call copybackground				; Draw bg layer
	;call drawPacman
	call copymemscreen				; Show image
	ret

..start:
	mov ax, mydata
	mov ds, ax
	mov ax, mystack
	mov ss, ax
	mov sp, stacktop
	
	mov ah,35h
	mov al,9
	int 21h					;Vanhat arvot talteen -> es:bx
	mov [oldintseg],es
	mov [oldintoff],bx
	
	push ds
	mov dx,KeybInt			;Oman keyboard interruptin alkuaddress
	mov bx,mycode								
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h					;Asetetaan oma keyboard interrupt
	pop ds
	
	mov ah,0fh
	int 10h					;Haetaan nykyinen videomode
	mov [oldvideomode],ah
	
	mov ah,00h
	mov al,13h
	int 10h					;Asetetaan uusi videomode

	call initbackground
	
.mainloop:
	call draw
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
	mov ah, 4ch
	int 21h
.end
