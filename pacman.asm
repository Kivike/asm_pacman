;Pacman
BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h

; starting address of video memory

videobase		EQU 0a000h

; some colors

M		EQU 0				;Musta
G		EQU 00110000b		;Green
B		EQU 00001001b		;Blue
R		EQU 00000100b		;Red
W		EQU 00001111b		;White
H		EQU 00000111b		;Harmaa
Y		EQU 00001110b		;Yellow
T		EQU 11111111b		;Transparent
J		EQU 11111110b		;Junction

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
	Ghost1 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,G,R,R,G,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R
	Ghost2 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,G,R,R,G,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R
	Ghost3 		db T,T,R,R,R,R,R,R,T,T,T,R,R,R,R,R,R,R,R,T,R,R,R,R,R,R,R,R,R,R,R,R,W,G,R,R,G,W,R,R,R,R,W,M,R,R,M,W,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,T,R,R,R,R,T,R,R,R,T,T,T,R,R,T,T,T,R

	CoinBlock 	db T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T
	EmptyBlock 	db M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M
	
	CoinBlockJunction 	db J,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,Y,Y,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T
	EmptyBlockJunction 	db J,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M,M

	BlueBlock 	db B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B

	Pacman 		db T,T,T,Y,Y,Y,Y,T,T,T,T,T,Y,Y,Y,Y,Y,Y,T,T,T,Y,Y,Y,M,M,Y,Y,Y,T,Y,Y,Y,M,W,W,M,Y,Y,Y,Y,Y,M,W,M,Y,W,M,Y,Y,Y,Y,M,W,M,M,W,M,Y,Y,Y,Y,Y,M,W,W,M,Y,Y,Y,T,Y,Y,Y,M,M,Y,Y,Y,T,T,T,Y,Y,Y,Y,Y,Y,T,T,T,T,T,Y,Y,Y,Y,T,T,T
	
	MapRow1 	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	MapRow2 	db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,4,2,2,1
	MapRow3 	db 1,2,1,4,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow4 	db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow5 	db 1,2,1,2,2,4,2,4,2,2,2,4,2,2,2,4,2,4,2,4,1
	MapRow6 	db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow7 	db 1,2,1,2,2,4,1,4,2,4,2,4,2,2,1,2,2,4,1,2,1
	MapRow8 	db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow9 	db 1,4,2,2,1,4,2,2,1,2,1,1,1,4,2,2,1,4,2,2,1
	MapRow10 	db 1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1
	MapRow11 	db 1,4,2,2,1,4,2,2,1,2,1,1,1,4,2,2,1,4,2,2,1
	MapRow12 	db 1,2,1,2,1,2,1,2,1,2,1,1,1,2,1,2,1,2,1,2,1
	MapRow13 	db 1,2,1,2,2,4,1,4,2,4,2,4,2,2,1,2,2,4,1,2,1
	MapRow14 	db 1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,2,1
	MapRow15 	db 1,2,1,2,2,4,2,4,2,2,2,4,2,2,2,4,2,4,2,4,1
	MapRow16 	db 1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow17 	db 1,2,1,4,2,2,1,2,1,1,1,1,1,1,1,2,1,2,1,2,1
	MapRow18 	db 1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,2,2,4,2,2,1
	MapRow19 	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

segment mydata data
	delay resw 1
	oldintseg resw 1
	oldintoff resw 1
	oldvideomode resw 1
	pressesc resw 1
	pressplus resw 1
	pressminus resw 1
	pressw resw 1
	pressa resw 1
	presss resw 1
	pressd resw 1
	movedir resw 1
    pacmanloc resw 1

	ghost1loc resw 1
	ghost1movedir resw 1
	ghost2loc resw 1
	ghost2movedir resw 1
	ghost3loc resw 1
	ghost3movedir resw 1
	
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
	    jne 	.cplusup
	    mov 	word [pressplus], 1
	    jmp     .kbread
.cplusup:
		cmp		al, 0CEh
		jne 	.cminus
		mov		word [pressplus], 0
		jmp 	.kbread
		
.cminus:
	    cmp 	al, 04Ah		; 4A is the 'make code' for keypad minus
	    jne		.cminusup
	    mov		word [pressminus], 1
		jmp 	.kbread
		
.cminusup:
		cmp		al, 0CAh
		jne .wDown
		mov word [pressminus], 0
		jmp .kbread
		
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
	
	mov ax,Pacman
	mov si,ax			; Source bitmap
	
	mov ax,memscreen
	mov es,ax			; Target segment
	mov cx,10			; Size
	mov dx,10			; Size
	
	mov di,[pacmanloc]	; Target offset
	
	call copybitmap
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
;Called only at the start of the game (uses a lot of cpu cycles)
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
		cmp bl, 3
		je .setemptyblockJunction
		cmp bl, 4
		je .setcoinblockJunction
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
		.setcoinblockJunction:
			mov bx,CoinBlockJunction
			mov si,CoinBlockJunction
			jmp .drawblock
		.setemptyblockJunction:
			mov bx,EmptyBlockJunction
			mov si,EmptyBlockJunction
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
		add di,2990			; Move to next row, 9*320+140
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
	pop cx
	pop bx

	cmp ah, 2 		;Check if there is a coin at pacmans line
	je .dofortwo
	cmp ah, 4
	jne .return

	;;;DO JUNCTION
	
	mov ah, 0
	mov byte[es:di],3

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
	
	mov si,EmptyBlockJunction
	mov dx,background
	mov es,dx
	mov cx,10
	mov dx,10
	call copybitmap
	jmp .return


	;;;DO STRAIGTH
	.dofortwo:           ;Eat

	mov ah, 0
	mov byte[es:di],0

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
	
	.checkW:
		cmp word[pressw],1
		jne .checkA
		mov ax,320
		neg ax
		jmp .checkifok
	.checkA:
		cmp word[pressa],1
		jne .checkS
		mov ax,1
		neg ax
		jmp .checkifok
	.checkS:
		cmp word[presss],1
		jne .checkD
		mov ax,320
		jmp .checkifok
	.checkD:
		cmp word[pressd],1
		jne .return
		mov ax,1

	.checkifok:
		call checkcollision
		cmp dx,1
		je .return				; COLLISION! ABORT ABORT
		mov [movedir],ax		; OK, set new direction
		
	.return:
		pop dx
		pop ax
		ret

movePacman:
	push ax
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
		pop ax
		ret

moveGhosts:
    ; Move all ghosts
    push si
    push di

	mov si, ghost1loc
    mov di, ghost1movedir
    call moveGhost

    mov si, ghost2loc
    mov di, ghost2movedir
    call moveGhost

    mov si, ghost3loc
    mov di, ghost3movedir
    call moveGhost

    pop di
    pop si
    ret


moveGhost:
    ; si = ghost location's memory address
    ; di = ghost direction's memory address
    ; Moves ghost towards its direction. If ghost collides, new direction is randomized.

    push ax
    push dx
    mov ax, cx;

    call checkjunctionghost
    cmp dx, 0                 ;if no junction hit
    je .nojunction

    call randomMovement

    .nojunction:
        call checkcollisionghost
        cmp dx,1
        je .collision
        mov ax,[si]

        push dx
        mov dx, [di]
        add ax, dx
        mov [si],ax
        pop dx
        jmp .skip
    .collision:
        call randomMovement
    .skip:
        pop dx
        pop ax
        ret

randomMovement:
    ; Select random movement direction to a ghost
    ; di = ghost move direction memory address

    pusha

	mov ah, 2CH 	;get system time
	int 21h

	mov ax, 0
	mov al, dl
	mov bl, 4
	div bl 			;get dividend (ah)

	cmp ah, 0
	je .moveup
	cmp ah, 1
	je .movedown
	cmp ah, 2
	je .moveleft
	cmp ah, 3
	je .moveright

	jmp .return

	.moveleft:
		mov ax, 1					;Checks so it won't go back where it came from
		cmp [di], ax
		je .moveright

		mov ax, -1					;Changes direction
		mov word[di], ax
		jmp .return
	.moveright:
		mov ax, -1
		cmp [di], ax
		je .moveleft

		mov ax, 1
		mov word[di], ax
		jmp .return
	.movedown:
		mov ax, -320
		cmp [di], ax
		je .moveup

		mov ax, 320
		mov word[di], ax
		jmp .return
	.moveup:
		mov ax, 320
		cmp [di], ax
		je .movedown

		mov ax, -320
		mov word[di], ax
		jmp .return

    .return:
        popa
        ret


checkcollision:
	; Checks for collision
	; Returns boolean to dx
	push ax
	push bx
	push cx
	
	mov bx, [pacmanloc]      ;Check pacmans next movement
	add bx,ax 				 ;Add move direction
	mov di,bx
	
	mov cx,memscreen 
	mov es,cx
	
	mov bl,B
	mov cx,10
	mov dx,10
	
	call iscolour
	
	mov dx,0
	cmp ax,0
	je .checkforgreen

	mov dx, 1
	jmp .return
		
    ; check collision with ghosts (colors green and red)
    .checkforgreen:
    mov cx,10
    mov dx,10
    mov bl,G
    call iscolour
    cmp ax,0
    je .checkforred
    call dieloop

    .checkforred:
    mov bl,R
    call iscolour
    cmp ax,0
    je .return
    call dieloop

	 
	.return:
		pop cx
		pop bx
		pop ax
		ret


checkcollisionghost:
	; Checks for collision
    ; si = location to check
	; Returns boolean to dx
	push ax
	push bx
	push cx
    push di
	
	mov bx, [si]      ;Save ghosts location
    mov ax, [di]
	add bx,ax
	mov di,bx
	
	mov cx,memscreen
	mov es,cx
	
	mov cx,10
	mov dx,10
	
	mov bl,B
	call iscolour
	
	mov dx,0		; default = no collision
	cmp ax,0
	je .return

	.collision:
		mov dx, 1	; collision

	.return:
        pop di
		pop cx
		pop bx
		pop ax
		ret

checkjunctionghost:
	; Checks for collision
	; Returns boolean to dx
	push ax
	push bx
	push cx
    push di
	
	mov bx, [si]      ;Save ghosts location
	mov di,bx				 ;Save ghosts location as offset
	
	mov cx,memscreen
	mov es,cx

	mov al, J
	cmp byte[es:di], al 	;If ghost at junction
	je .collision
	
	mov dx,0		; default = no collision
	jmp .return

	.collision:
		mov dx, 1	; collision
		mov ax, 10
		mov fs, ax
		
	.return:
        pop di
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
    push di
    push cx
    push dx
    mov ax, 0
    .rowloop:
        push cx
        push di
        mov cx,dx
        .colloop:
            cmp byte [es:di],bl
            jne .ok
                inc ax
            .ok:
            inc di
            loop .colloop
        pop di
        add di, 320
        pop cx
        loop .rowloop
    pop dx
    pop cx
    pop di
    ret
	
draw:
	; Creates the image to be shown on screen
	call copybackground				; Draw bg layer
	call drawPacman					; Draw pacman
	call drawGhost1
	call drawGhost2
	call drawGhost3
	
	; Send image to screen
	call copymemscreen
	ret

delayGame:
	mov word dx,[delay]
	.checkforinc
		cmp word[pressplus],1
		jne .checkfordec
		inc dx
		mov word[delay],dx
		jmp .pause1
		
	.checkfordec
		cmp word[pressminus],1
		jne .pause1
		cmp dx,1
		je .pause1
		dec dx
		mov word[delay],dx
	
	.pause1:
		mov cx,65535
	.pause2:
		dec cx
		jne .pause2
		dec dx
		jne .pause1
	ret

dieloop:
    inc ax
    call delayGame
    jmp dieloop

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

	mov word[delay],5
	mov word[ghost1movedir], 320
    mov word[ghost2movedir], 320
    mov word[ghost3movedir], 320
	
.mainloop:
	call delayGame
	call checkInput
	call movePacman
	call moveGhosts
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
