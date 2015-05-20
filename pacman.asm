segment data data

up DB 'Up$'
down DB 'Down$'
left DB 'Left$'
right DB 'Right$'
message DB 'asdasd $'

segment code code

..start:
  mov	ax,data
  mov	ds,ax                   ;set DS to point to the data segment

  mov dx, message
  mov cx, 1
  je takeInput


takeInput:
 
 mov	ah, 9                    ;DOS: print string
 int	21h
 
 mov	ah, 1					;DOS: get character
 int	21h
 or		al, 20h					;to lowercase
 
 ; Check keypresses and change the right text
 cmp	al, 'w'					
	je	Up					

 cmp	al, 'a'					
	je	Left					
	
 cmp	al, 's'					
	je	Down
	
 cmp	al, 'd'					
	je 	Right		

; Quit program on X
 cmp 	al, 'x'
 	je	QuitProgram
	
 jnz	takeInput
 
 
 Up: 
 mov dx, up
 je takeInput
 
 Left:
 mov dx, left
 je takeInput
 
 Down:
 mov dx, down
 je takeInput
 
 Right:
 mov dx, right
 je takeInput
 
QuitProgram:

  mov	al,0					;return code will be 0
  mov	ah,4ch					;DOS: terminate program
  int	21h
.end