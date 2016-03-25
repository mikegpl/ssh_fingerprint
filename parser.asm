;----------------------------------- RSA key fingerprint to ASCII art ----------------------------------- 
;-----------------------------------  Michał Grabowski 'mkgrabowski'  ----------------------------------- 
;-----------------------------------    Informatyka IEiT, grupa 7     ----------------------------------- 



;-----------------------------------              Data                ----------------------------------- 
data segment 
 	
	buffer	            		db 128 	dup('$')	; parse input to buffer
	argsindex					db 128 	dup('0')	; index of each arg in buffer
	argctr 						db 49,'$'			; number of arguments
	nothing 					db 10,13, '$' 
	arglen						db 0				; length of parsed (with NULLs) arguments 
	bufferlen					db 0, "$"

 	debug  						db "*$"
 	flaga						db 0

 	
	error0 						db "Error 0: no arguments$"
	error1 						db "Error 1: first argument passed in must be either 0 or 1$"
	error2 						db "Error 2: second argument must be of length 32$"
	error3 						db "Error 3: second argument should be a string consisting of a-f letters and 0-9 numbers$"
	error4 						db "Error 4: invalid number of arguments$"
data ends 
 



;-----------------------------------              Code                ----------------------------------- 
code1 segment 

main:
	;stack initialization 
   	mov ax, seg stack_top 
  	mov ss, ax 
  	mov sp, offset stack_top
  	
	call loadtobuffer				; load arguments from buffer
	;call checknumber				; check if number of args = 2
	;call buffertoarg				; if number of args is right write to arg1, arg2

    ;print arguments
	mov dx, offset buffer			
	call print
    ;print new line
    mov dx, offset nothing
    call print
    ;print number of arguments
    mov dx, offset argctr
    call print

    

    ;end the program end exit to command prompt
    call endsection

;###################################        Procedures section        ################################### 
loadtobuffer:
	push ax
	push bx
	push cx
	push dx

	mov ax, seg data 				; load data segment
	mov es, ax 						; using extra segment for data

	xor cx, cx 						; set cx to 0 for loop
	mov cl, byte ptr ds:[080h]		; set cl to length of arguments
	cmp cl, 0d						
	JE errorhandler0				; if no arguments jump to errorhandler0

	; input length of args
	xor bx,bx
	mov bx, offset bufferlen
	mov byte ptr es:[bx], cl

	; if there are arguments 
	mov si, 082h 					; move PSP offset to SI (source index)
	mov di, offset buffer 			; move buffer offset to DI (destination index)
 	
 	xor dh, dh 						; set dh to 0 for counting arg length
 	
 	; input loop
	input:		
		call whitespacedestroyerultra5000		
		mov al, byte ptr ds:[si] 	; read byte from source index to al
		mov byte ptr es:[di], al	; write byte from al to destination index
		inc dh
		inc si 						; si+=1 -> now points to next byte
		inc di 						; di+=1 -> now points to next byte
	loop input 						; cx-=1, jump to input

	mov byte ptr es:[arglen], dh	; set arglen to dh (number of characters written to buffer)
									; debugging : sets arglen to right value
	xor dh,dh 

 	pop dx
 	pop cx
 	pop bx
 	pop ax
ret

whitespacedestroyerultra5000:

	mov al, byte ptr ds:[si]		; load next byte to al 

	cmp al, 09h						; check if space
	JE skipwhitespaces	

	cmp al, 20h						; check if tab
	JE skipwhitespaces

	mov ah, es:[flaga]				; check if insert separator
	cmp ah, 1						; if any whitespaces were skipped
	JE insert
ret

skipwhitespaces:					; skipping whitespaces 
	inc si 							; si+=1 
	dec cl 							; loop counter-=1
	mov byte ptr es:[flaga], 1		; set flag to high

	call whitespacedestroyerultra5000
ret


insert:								; insert separator to argstring, also counts arguments
	cmp di, 0 						; if insert to first buffer character
	JE noinsert

	;########### inserting NULL 	;
	;mov byte ptr es:[di], 0		; set next character to NULL
	;inc es:[argctr]				; increment argument counter
	;inc di 						; set di to next character
 	;inc dh							; in dh we store length of parsed arguments
	;########### /inserting NULL

	; printing new line
	mov byte ptr es:[di], 10		; set next char to new line
	inc es:[argctr] 				; increment argument counter
	inc di 							; set di to next char
	inc dh 							; increment length of parsed arguments
	mov byte ptr es:[di], 13 		; set next char to carriage return
	inc di 							; set di to next char
	inc dh  						; increment length of parsed arguments
    ; added printing new line

	noinsert:						;	
	xor ah,ah 						; set ah to 0
	mov byte ptr es:[flaga],0  		; flag=0
ret 

checknumber:						; 
	push ax
	mov ax, seg data
	mov ds, ax
	cmp ds:[argctr], 50
	JNE errorhandler4
	pop ax
ret

buffertoarg:
	push ax
	push bx
	push cx
	push dx


	pop dx
	pop cx
	pop bx
	pop ax
ret



;###################################          Error handling          ################################### 
errorhandler0:
	mov dx, offset error0
	call print
	call endsection
ret
errorhandler1:
	mov dx, offset error1
	call print
	call endsection
ret
errorhandler2:
	mov dx, offset error2
	call print
	call endsection
ret
errorhandler3:
	mov dx, offset error3
	call print
	call endsection
ret
errorhandler4:
	mov dx, offset error4
	call print
	call endsection
ret


;###################################          Print and exit          ################################### 
print: 								;print variable from data segment
	mov ax, seg data 				
    mov ds, ax 						; move data segment adress to ds 
    mov ah, 9h 						; interruption argument 9h (write)
    int 21h 						
ret 

endsection:							; end 
	mov ah, 04ch 					; iterruption argument 4ch (quit)
    int 21h 
ret
 
code1 ends			


;-----------------------------------              Stack               ----------------------------------- 
stos1 segment stack 
    dw  200 dup(?) 
    stack_top dw  ? 
stos1 ends 
 
end main 