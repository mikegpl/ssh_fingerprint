;----------------------------------- RSA key fingerprint to ASCII art ----------------------------------- 
;-----------------------------------  Michał Grabowski 'mkgrabowski'  ----------------------------------- 
;-----------------------------------    Informatyka IEiT, grupa 7     ----------------------------------- 



;-----------------------------------              Data                ----------------------------------- 
data segment 
 	
 	; parser section
	bufferlen					db 0				; length of PSP buffer
	flag						db 0				; flag for parsing
	buffer	            		db 128 	dup('$')	; parse input to buffer
	argctr 						db 0,'$'			; number of arguments
	arglen						db 0, '$'			; length of parsed (with NULLs) arguments (working)
	legalchars					db "0123456789abcdef"; list of legal chars for argument2
    ;/parser section

    ; arguments section
    argslength					db 128  dup(0)		; length of each argument
    argument1					db 128  dup('$')	; argument number 1
    argument2 					db 128  dup('$') 	; argument number 2
    ;/arguments section

    ; debugging args
	newline 					db 10,13, '$' 
 	debug  						db "*$"
 	tmp							db 0, '$'
 	;/debugging args
 	
 	; drunken bishop section
 	chartable					db 153 	dup(0)
 	bytesequence0				db 16  	dup(0)
 	bytesequence1				db 17  	dup(0)
 	;/drunken bishop section

 	; errors
	error0 						db "Error 0: no arguments$"
	error1						db "Error 1: first argument must be of length 1$"
	error2 						db "Error 2: first argument passed in must be either 0 or 1$"
	error3 						db "Error 3: second argument must be of length 32$"
	error4 						db "Error 4: second argument should be a string consisting of a-f letters and 0-9 numbers$"
	error5 						db "Error 5: invalid number of arguments$"
	;/errors
data ends 
 



;-----------------------------------              Code                ----------------------------------- 
code1 segment 

main:
	;stack initialization 
   	mov ax, seg stack_top 
  	mov ss, ax 
  	mov sp, offset stack_top

  	call parse
  	mov dx, offset newline
  	call print
    ;end the program end exit to command prompt
    call endsection

;###################################              Parser              ################################### 


parse: 								; parse arguments, calls proper procedures

	call loadtobuffer				; load arguments from buffer
	call checknumber				; check if number of args = 2
	call setarglengths
	call buffertoargs
	call validateargs
	;print arguments
	mov dx, offset buffer			
	call print
ret

loadtobuffer:						; load from PSP buffer to var buffer in loop

	push ax
	push bx
	push cx
	push dx

	mov ax, seg data 				; load data segment
	mov es, ax 						; using extra segment for data

	xor cx, cx 						; set cx to 0 for loop
	mov cl, byte ptr ds:[080h]		; set cl to length of arguments
	cmp cl, 0d						
	JNE loadargs					; if no arguments do error0out

	error0out:						
	mov dx, offset error0
	call errorhandler


	loadargs:						; if there are arguments
	; input length of args
	xor bx,bx
	mov bx, offset bufferlen
	mov byte ptr es:[bx], cl

	mov si, 082h 					; move PSP offset to SI (source index)
	mov di, offset buffer 			; move buffer offset to DI (destination index)
 	
 	xor dh, dh 						; set dh to 0 for counting arg length
 	; input loop
 	mov al, byte ptr ds:[si]
 	cmp al, 21h
 	
	input:		
		call eatwhitespaces		
		mov al, byte ptr ds:[si] 	; read byte from source index to al
		mov byte ptr es:[di], al	; write byte from al to destination index
		inc dh
		inc si 						; si+=1 -> now si is pointing to next byte
		inc di 						; di+=1 -> now di is pointing to next byte
	loop input 						; cx-=1, jump to input

	mov byte ptr es:[arglen], dh	; set arglen to dh (number of characters written to buffer)
									; TEST : sets arglen to right value (ok)

	dec di 							; set last arg char to NULL (for next iteration)
	mov byte ptr es:[di], 0

								
 	pop dx
 	pop cx
 	pop bx
 	pop ax 			
ret
 	
eatwhitespaces:						; eat white spaces, insert separator after each argument

	mov al, byte ptr ds:[si]		; load next byte to al 

	cmp al, 09h						; check if space
	JNE checktab

 	skipwhitespaces:				; skipping whitespaces 
	inc si 							; si+=1 
	dec cl 							; loop counter-=1
	mov byte ptr es:[flag], 1		; set flag to high
	JMP eatwhitespaces
	
	checktab:
	cmp al, 20h						; check if tab
	JNE separator

	skipwhitespaces2:				; skipping whitespaces 
	inc si 							; si+=1 
	dec cl 							; loop counter-=1
	mov byte ptr es:[flag], 1		; set flag to high
	JMP eatwhitespaces

 	separator:
	mov ah, es:[flag]				; check if insert separator
	cmp ah, 1						; if any whitespaces were skipped
	JNE noinsert

	insert:							; insert separator to argstring, also counts arguments
	cmp di, offset buffer			; if insert to first buffer character
	JE noinsert

	;########### inserting NULL 	;
	mov byte ptr es:[di], 0		    ; set next character to NULL
	inc es:[argctr]					; increment argument counter
	inc di 							; set di to next character
 	inc dh							; in dh we store length of parsed arguments
	;########### /inserting NULL
	
	noinsert:						;	
	xor ah,ah 						; set ah to 0
	mov byte ptr es:[flag], 0  		; flag=0	
ret

checknumber:						; check number of aruments

	push ax
	mov ax, seg data
	mov ds, ax
	cmp ds:[argctr], 1				; by default argctr==0 and it is incremented when inserting separator, thus argctr==arg number-1
	JE rightnumber

	error5out:
	mov dx, offset error5
	call errorhandler

	rightnumber:					; if number of args == 2, return
	pop ax
ret

setarglengths:						; fills argslength[] tab, checks for length errors
	
	push ax
	push cx

	mov ax, seg data
	mov ds, ax

	xor ax, ax 						; ax=0
	xor si, si
	mov si, offset buffer 			; set si (source index) to first character of parsed arguments
	xor di, di 						; set di (destination index) to 0
	xor cx, cx 						; cx=0
	mov cl, byte ptr ds:[arglen]	; now cl = length of parsed arguments

	stlen: 							; loop for writing to argslength[] tab
		mov al, byte ptr ds:[si]	; load next character to al
		cmp al, 0 					; check if al==NULL
		JE isnull

		inc ds:[argslength+di] 		; current arg length ++
		JMP notnull

		isnull:						; if current char is null
		inc di 						; edit next (argslength[di]) length

		notnull:					; current character is not null
		inc si 						; si+=1, point to next char
	loop stlen

	call checkarglen 				; check length of arguments 

	pop cx
	pop ax
ret

checkarglen:						; upgrade - store proper lengths in one tab, store error offsets in another tab for faster validation

	xor ax, ax
	mov al, byte ptr ds:[argslength] 
	cmp al, 1						; check length of first argument	
	JNE error1out 					

	mov al, byte ptr ds:[argslength+1]  
	cmp al, 32						; check length of second argument
	JNE error3out					; if argslength[1]!=32 then print error3 and exit to command prompt

	JMP noerrors					; if lengths are right

	error1out:
	mov dx, offset error1
	call errorhandler

	error3out:
	mov dx, offset error3
	call errorhandler

	noerrors:
ret

buffertoargs: 					 	; upgrade - store offsets of args in buffer in a tab

	push ax
	push bx
	push dx

	mov ax, seg data
	mov ds, ax

	xor bx, bx						; bx=0 
	mov bx, offset argument1		; bx is pointing to argument1, for writing from buffer to argument1
	mov al, byte ptr ds:[argslength]; store in al length of arg1
	mov si, 0 						; start index of arg1 in buffer
	call toarg 						

	xor bx, bx 						; bx=0
	mov bx, offset argument2 		; bx is pointing to argument2, for writing from buffer to argument2
	mov al, byte ptr ds:[argslength+1]; store in al length of arg2
	xor dx, dx 						; prepare si for arg2
	add dl, byte ptr ds:[argslength]; si=len(arg1)+1
	inc dl
	xor si, si
	add si, dx
	call toarg

	pop dx
	pop bx
	pop ax
ret

toarg: 								; bx - offset to argument#nr, al - length of argument, si - number of chars before argument in buffer
	
	push cx 						
	xor cx, cx 					 	; cx=0
	mov cl, al  					; set counter to length of argument
	xor al, al

	writetoarg:						; write from buffer to argument#nr
		mov al, byte ptr ds:[buffer+si]
		mov byte ptr ds:[bx], al
		inc si 						; now si is pointing to next char from buffer
		inc bx 						; now bx is pointing to next char in argument#nr
	loop writetoarg
	pop cx
ret

validateargs: 						; check if args consist of legal characters

	push ax
	push cx

	mov ax, seg data
	mov ds, ax
	xor ax, ax

	mov al, byte ptr ds:[argument1]	
	cmp al, 48d						; check if first arg is '0'
	JE checksecond

	cmp al, 49d						; check if first arg is '1'
	JE checksecond

	error2out: 						; if first arg is neither '0' nor '1'
	mov dx, offset error2 
	call errorhandler 				; print error2

	checksecond:	 				; validate second argument
	xor ax, ax
	xor cx, cx
	mov cl, byte ptr ds:[argslength+1]; set counter for length(argument2)
	mov si, offset argument2 		; set si (source index) to first char of argument2
	validate: 						; iterate over argument2 chars and check if they are in legalchars tab
		mov al, byte ptr ds:[si] 	; load next character to al
		call testchar 				; testchar(al), returns if character is legal in ah
		cmp ah, 0 					; if character isn't in legalchars tab
		JE error4out 				
		inc si 						; si+=1, now si is pointing to next char
	loop validate
	JMP noerrors2 					; if there were no errors, end procedure

	error4out: 						; if second arg consists of illegal chars
	mov dx, offset error4
	call errorhandler

	noerrors2:

	pop cx
	pop ax
ret

testchar:							; check if character is in legalchars tab

	push cx 					 	; push cx, i will be using it for next loop
	push dx 

	xor ah, ah
	xor cx, cx
	xor dx, dx
	mov cl, 16d 					; cl=16, length of legalchars tab
	mov di, offset legalchars 		; di is pointing to first char of legalchars tab
	chartab:
		mov dl, byte ptr ds:[di] 	; load next legal character to dl
		cmp al, dl 					
		JE setflag 					; if verified character has matching char in legalchars tab
		inc di 						; di+=1, now di is pointing to next char in legalchars tab
	loop chartab
	JMP nocharfound 				; if no matches were found, return ah=0

	setflag: 						; if character is correct, return ah=1
	mov ah, 1

	nocharfound:
	pop dx
	pop cx
ret


;###################################          Error handling          ################################### 

errorhandler: 						; args - error offset in dx

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