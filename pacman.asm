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
	EmptyBlock 	db M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M

	BlueBlock 	db B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B

	Pacman 		db T,T,T,Y,Y,Y,Y,T,T,T,T,T,Y,Y,Y,Y,Y,Y,T,T,T,Y,Y,Y,T,T,Y,Y,Y,Y,Y,Y,Y,T,W,W,T,Y,Y,Y,Y,Y,T,W,T,R,W,T,Y,Y,Y,Y,T,W,T,T,W,T,Y,Y,Y,Y,Y,T,W,W,T,Y,Y,Y,T,Y,Y,Y,T,T,Y,Y,Y,T,T,T,Y,Y,Y,Y,Y,Y,T,T,T,T,T,Y,Y,Y,Y,T,T,T

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
	pressw resw 1
	pressa resw 1
	presss resw 1
	pressd resw 1
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
	    jne	.wDown
	    ;something here later
		jmp 	.kbread
		
.wDown:	cmp		al,11h
		jne .aDown
		mov word[pressw],1
		jmp .kbread
		
.aDown:	cmp		al,1Eh
		jne .sDown
		mov word[pressa],1
		jmp .kbread

.sDown	cmp		al,1Fh
		jne .dDown
		mov word[presss],1
		jmp .kbread
		
.dDown	cmp		al,20h
		jne .wUp
		mov word[pressd],1
		jmp .kbread
		
.wUp:	cmp		al,91h
		jne .aUp
		mov word[pressw],0
		jmp .kbread
		
.aUp:	cmp		al,9Eh
		jne .sUp
		mov word[pressa],0
		jmp .kbread

.sUp:	cmp		al,9Fh
		jne .dUp
		mov word[presss],0
		jmp .kbread
		
.dUp:	cmp		al,0A0h
		jne .kbread
		mov word[pressd],0

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
	mov si,ax			; Source bitmap
	
	mov ax,memscreen
	mov es,ax			; Target segment
	
	mov cx,10			; Size
	mov dx,10
	
	mov di,[pacmanloc]	; Target offset
	
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
		je .setemptyblock
		
		.setBlueBlock:
			mov bx,BlueBlock
			mov si,BlueBlock
			jmp .drawblock
		.setcoinblock:
			mov bx,CoinBlock
			mov si,CoinBlock
			jmp .drawblock
		.setemptyblock:
			mov bx,EmptyBlock
			mov si,EmptyBlock
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

checkIfEatCoins:	
	push ds
	pusha
	mov ax, [pacmanloc]
	
	;Get pacman's row
	mov dx, 0
	mov bx, 3200            
	div bx
	mov cx, ax 				;Put pacman's row to cx

	mov ax, dx 				;Get modulo to ax
	mov dx, 0
	mov bx, 320             ;Get rows off from next modulo
	div bx					; dx:ax/bx
	mov ax, dx

	
	;Get pacman's column
	mov dx, 0 				;Wont work without :D
	mov bx, 10 				
	div bx
	mov bx, ax 				;Put pacman's column to bx

	push bx
	push cx
	
	mov ax, cx 				;move rows to ax
	mov cx, 21
	mul cx                  ;multiply by columns in one row


	add ax, bx				;and add columns to get final point

	mov bx,bitmaps
	mov ds,bx
	mov es,bx
	add ax, MapRow1	
	mov di,ax

	mov ah,byte[es:di]
	cmp ah, 2      ;Check If there is a coin at pacmans tile
	jne .skipeat            ;Eat
	mov cx,7
	mov fs,cx
	mov ah, 0
	mov byte[es:di],0

	pop cx
	pop bx
	
	mov ax,cx
	mov cx,3200
	mul cx
	mov cx,ax
	
	mov ax,bx
	mov bx,10
	mul bx
	mov bx,ax
	
	; set parameters for copybitmap
	mov dx,0
	add dx,bx
	add dx,cx
	mov di,dx
	
	mov si,EmptyBlock
	mov dx,background
	mov es,dx
	mov cx,10
	mov dx,10
	call copybitmap
	
	jmp .return
	
	.skipeat:
	pop cx
	pop bx

	
	.return:
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
	
checkInput:
	push ax
	push dx
	
	cmp word[pressw],1
	je .pressedW
	
	cmp word[pressa],1
	je .pressedA
	
	cmp word[presss],1
	je .pressedS
	
	cmp word[pressd],1
	je .pressedD
	
	jmp .return
	
	.pressedW:
		mov ax,640
		neg ax
		jmp .checkifok
		
	.pressedA:
		mov ax,2
		neg ax
		jmp .checkifok
		
	.pressedS:
		mov ax,640
		jmp .checkifok
		
	.pressedD:
		mov ax,2

	.checkifok:
		call checkcollision
		cmp dx,1
		je .return				; not OK
		mov [movedir],ax		; OK
		
	.return:
		pop dx
		pop ax
		ret

movePacman:
	pusha
	push dx
	call checkIfEatCoins
	mov ax,[movedir]
	
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
		pop dx
		popa
		ret

checkcollision:
	; Checks for collision
	; Returns boolean to bx
	push ax
	push bx
	push cx
	
	mov bx, [pacmanloc]      ;Check pacmans next movement
	add bx,ax ;[movedir]
	mov di,bx	;bx
	
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
		pop ax
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
	;call drawGhost1
	;call drawGhost2
	;call drawGhost3
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
	mov word[pressw],0
	mov word[pressa],0
	mov word[presss],0
	mov word[pressd],0
	
.mainloop:
	call checkInput
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
