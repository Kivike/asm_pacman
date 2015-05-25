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
	CoinBlock db transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,yellow,yellow,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,yellow,yellow,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent,transparent
	BlueBlock db blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue
	RedBlock db red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red,red
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
	
	CoinRow1 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    CoinRow2 db 0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0
    CoinRow3 db 0,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0
    CoinRow4 db 0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0
    CoinRow5 db 0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
    CoinRow6 db 0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0
    CoinRow7 db 0,1,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,0,1,0
    CoinRow8 db 0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0
    CoinRow9 db 0,1,1,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0
    CoinRow10 db 0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0
    CoinRow11 db 0,1,1,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0
    CoinRow12 db 0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0
    CoinRow13 db 0,1,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,0,1,0
    CoinRow14 db 0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0
    CoinRow15 db 0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
	CoinRow16 db 0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0
    CoinRow17 db 0,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0
    CoinRow18 db 0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0
    CoinRow19 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	;map db 11111111b,11111111b,11110000b,00001000b,00000110b,11011101b,01110110b,11000000b,00000000b,00011011b,01011111b,01011011b,00001000b,10001000b,01111101b,11010111b,01111111b,10100000b,00101111b,11110101b,11110101b,11111110b,00111110b,00111111b,11010111b,11010111b,11111010b,00000010b,11111111b,01011111b,01011111b,00000000b,10000000b,01101101b,11010111b,01101100b,10000000b,00001001b,11010101b,11110101b,01110000b,10001000b,10000110b,11111101b,01111110b,11000000b,00000000b,00011111b,11111111b,11111110b
	
segment mydata data
	oldintseg resw 1
	oldintoff resw 1
	oldvideomode resw 1
	pressesc resw 1


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
	
copycoinmap:

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
	
initcoinmap:
	push ds
	pusha
	mov ax,bitmaps				
	mov ds,ax					; ds = bitmaps = source segment
	mov di,0
	mov si,CoinRow1				; si = current map block
	mov ax,background
	mov es,ax					; es = background = target segment
	mov dx,21					; columns = 21
	mov cx,19					; rows = 19
	
	.drawblockrow:
		push cx
		mov cx,dx
	.drawblockcolumn:
		mov byte bl,[ds:si]		; current mapblock
		push si
		cmp bl,1				; if mapblock is a wall
		jne .skip
		mov bx,CoinBlock
		mov si,CoinBlock
		push cx
		push dx
		mov cx,10
		mov dx,10
		call copybitmap
		pop dx
		pop cx
		.skip:
			pop si
			inc si
			add di,10
			loop .drawblockcolumn
		pop cx
		add di,2990			; Move to next row
		loop .drawblockrow
	popa
	pop ds
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
	call copycoinmap				; Draw coin layer
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
	call initcoinmap
	call draw
	
.mainloop:
	;call draw
	;call draw
	;call drawsinglepixel
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
	mov ah, 4ch
	int 21h

.end
