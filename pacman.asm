;Pacman
BITS 16

;;;;;;;;;;;;;;
; CONSTANTS
;;;;;;;;;;;;;;
stacksize       EQU 0200h


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
        in      al,61h          ; Send acknowledgment without
        or      al,10000000b    ; modifying the other bits.
        out     61h,al          ;                            
        and     al,01111111b    ;                            
        out     61h,al          ;                            
        mov     al,20h          ; Send End-of-Interrupt signal 
        out     20h,al          ;                              
        sti                     ; Enable interrupts again
        pop     ax		
   	    pop 	ds       		; Regain the ds,ax from stack
        iret	                ; Return from interrupt
		

..start:
		
	mov ah,35h
	mov al,9
	int 21h								;Vanhat arvot talteen -> es:bx
	mov [oldintseg], es
	mov [oldintoff], bx
	
	push ds
	mov dx,KeybInt
	mov bx,mycode
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h								;Asetetaan oma keyboard interrupt
	pop ds
	
.mainloop:
	cmp word [pressesc],1
	jne .mainloop
	
	
.dosexit:
	mov word dx, [oldintoff]
	mov word bx, [oldintseg]
	mov ds,bx
	mov al,9
	mov ah,25h
	int 21h								;Vanhat arvot takas
	
	mov	al, 0
	mov     ah, 4ch
	int     21h

.end
