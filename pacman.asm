;Pacman
BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h

; starting address of video memory

videobase		EQU 0a000h

; some colors

M		EQU 0
G		EQU 00110000b
B		EQU 00001001b
R		EQU 00000100b
W		EQU 00001111b
H		EQU 00000111b
Y		EQU 00001110b
T		EQU 11111111b

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
	Ghost1 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,B,R,R,B,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R
	Ghost2 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,B,R,R,B,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R
	Ghost3 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,B,R,R,B,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R

	CoinBlock 	db T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T
	
	BlueBlock 	db B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B

	Pacman 		db Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y
	
	MapRow1 	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	MapRow2 	db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,1
	MapRow3 	db 1,2,1,2,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow4 	db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow5 	db 1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	MapRow6 	db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow7 	db 1,2,1,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,1,2,1
	MapRow8 	db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow9 	db 1,2,2,2,1,2,2,2,1,2,1,1,1,2,2,2,1,2,2,2,1
	MapRow10 	db 1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1
	MapRow11 	db 1,2,2,2,1,2,2,2,1,2,1,1,1,2,2,2,1,2,2,2,1
	MapRow12 	db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow13 	db 1,2,1,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,1,2,1
	MapRow14 	db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow15 	db 1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	MapRow16 	db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow17 	db 1,2,1,2,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow18 	db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,1
	MapRow19 	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

segment mydata data
	oldintseg resw 1
	oldintoff resw 1
	oldvideomode resw 1
	pressesc resw 1
	movedir resw 1
    pacmanloc resw 1
	ghost1loc resw 1
	ghost2loc resw 1
	ghost3loc resw 1
	dotseaten resw 1
	
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
	    jne	.w
	    ;something here later

.w:
		cmp		al,11h
		jne .a
		
		mov bx,[movedir]
		push bx
		mov bx,320
		neg bx
		mov [movedir],bx		; Set new move direction initially
		
		call checkcollision		; Check if there is collision with the new direction
		pop bx					; Pop bx to the original move direction
		cmp dx,1
		jne .kbread				; If no collision, the direction remains
		.undow:					
			mov [movedir],bx	; Else, set back the original direction
		
.a:		cmp		al,1Eh
		jne .s
		
		mov bx,[movedir]
		push bx
		mov bx,1
		neg bx
		mov [movedir],bx	
		
		call checkcollision
		pop bx
		cmp dx,1
		jne .kbread
		.undoa:
			mov [movedir],bx

.s		cmp		al,1Fh
		jne .d
		
		mov bx,[movedir]
		push bx
		mov bx,320
		mov [movedir],bx
		
		call checkcollision
		pop bx
		cmp dx,1	
		jne .kbread
		.undos:
			mov [movedir],bx
;
.d		cmp		al,20h
		jne .kbread
		
		mov bx,[movedir]
		push bx
		mov bx,1
		mov [movedir],bx
		
		call checkcollision
		pop bx
		cmp dx,1
		jne .kbread
		.undod:
			mov [movedir],bx

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
	
drawPacman:
	pusha
	push cx
	push dx
	mov ax,Pacman
	mov si,ax
	mov ax,memscreen
	mov es,ax
	mov cx,10
	mov dx,10
	mov di,[pacmanloc]
	call copybitmap
	pop dx
	pop cx
	popa
	ret
	
drawGhost1:
	pusha
	push cx
	push dx
	mov ax,Ghost1
	mov si,ax
	mov ax,memscreen
	mov es,ax
	mov cx,10
	mov dx,10
	mov di,[ghost1loc]
	call copybitmap
	pop dx
	pop cx
	popa
	ret

drawGhost2:
	pusha
	push cx
	push dx
	mov ax,Ghost2
	mov si,ax
	mov ax,memscreen
	mov es,ax
	mov cx,10
	mov dx,10
	mov di,[ghost2loc]
	call copybitmap
	pop dx
	pop cx
	popa
	ret
	
drawGhost3:
	pusha
	push cx
	push dx
	mov ax,Ghost3
	mov si,ax
	mov ax,memscreen
	mov es,ax
	mov cx,10
	mov dx,10
	mov di,[ghost3loc]
	call copybitmap
	pop dx
	pop cx
	popa
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
		je .setBlueBlock
		cmp bl,2
		je .setcoinblock
		cmp bl,0
		je .skip
		
		.setBlueBlock:
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

 checkIfEatCoins:			;Might not work with 90% chance
	; mov ax,mydata			
	; mov ds, ax				;Change datasegment
	; mov bh,pacmanloc
	; mov bl, [bh / 10]		;Get pacman location in tiles (divided by 10 (can you even do this?))
	; mov ax,bitmaps						
	; mov ds, ax				;Change datasegment to bitmaps
	; mov ax, MapRow1 		;Get first map row				
	; cmp [ax + bl], 2        ;Check If there is a coin at pacmans tile
	; je eat                  ;Eat
	; ret

; eat:
	; mov [ax + bl], 0
	; ret


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
			cmp byte bl,T
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
	
movePacman:
	pusha
	mov ax,50h
	mov bl,2
	div bl
	call checkcollision
	cmp dx,1
	je .collision
	mov ax,[pacmanloc]
	add ax,[movedir]
	mov [pacmanloc],ax
	jmp .skip
	.collision:
		mov word[movedir],0
	.skip:
		popa
		ret

checkcollision:
	; Checks for collision
	; Returns boolean to bx
	push bx
	push cx
	
	mov bx, [pacmanloc]      ;Check pacmans next movement
	add bx, [movedir]
	mov di,bx
	mov cx,memscreen
	mov es,cx
	
	mov bl,B
	mov cx,10
	mov dx,10
	
	call iscolour
	
	mov dx,0
	cmp ax,0
	je .return

	.collision:
		mov dx, 1
		
	.return:
		pop cx
		pop bx
		ret
		
; https://wiki.oulu.fi/pages/viewpage.action?title=PACMAN+-+Part+2
iscolour:
	; DI = Rect coordinate
	; CX, DX = Rect width, height
	; ES = Data segment to check in
	; BL = Color to check
	; AX = Return as pixel count
    PUSH DI
    PUSH CX
    PUSH DX
    MOV AX, 0
    .rowloop:
        PUSH CX
        PUSH DI
        MOV CX, DX
        .colloop:
            cmp byte [ES:DI], BL
            jne .ok
                INC AX
            .ok:
            INC DI
            LOOP .colloop
        POP DI
        ADD DI, 320
        POP CX
        LOOP .rowloop
    POP DX
    POP CX
    POP DI
    RET
	
draw:
	; Creates the image by layering
	call copybackground				; Draw bg layer
	call drawPacman					; Draw pacman
	call drawGhost1
	call drawGhost2
	call drawGhost3
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
	mov word[pacmanloc],3210
	mov word[ghost1loc],3350
	mov word[ghost2loc],3370
	mov word[ghost3loc],3390
	
.mainloop:
	call movePacman
	call draw
	cmp word [pressesc],1
	jne .mainloop
	
.dosexit:
	mov word dx, [oldintoff]
	mov word bx, [oldintseg]
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h				;Vanhat arvot takas
	
	mov word dx, [oldvideomode]
	mov ah,00h
	mov al,13h
	int 10h				;Vanha videomode takas
	
	mov	al, 0
	mov ah, 4ch
	int 21h
