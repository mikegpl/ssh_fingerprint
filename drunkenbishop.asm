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
 	topframe					db "+--( RSA 1024)----+$"
 	bottomframe					db "+-----------------+$"
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
  	call drunkenbishop
   
    call endsection

;###################################              Parser              ################################### 

parse: 								; parse arguments, calls proper procedures

	call loadtobuffer				; load arguments from buffer
	call checknumber				; check if number of args = 2
	call setarglengths
	call buffertoargs
	call validateargs
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
	xor bx,bx
	mov bx, offset bufferlen		; input length of args
	mov byte ptr es:[bx], cl

	mov si, 082h 					; move PSP offset to SI (source index)
	mov di, offset buffer 			; move buffer offset to DI (destination index)
 	
 	xor dh, dh 						; set dh to 0 for counting arg length
 	; input loop
 	mov al, byte ptr ds:[si]
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

									;########### inserting NULL 
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
	push si

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
	pop si
	pop cx
	pop ax
ret

testchar:							; check if character is in legalchars tab

	push cx 					 	; push cx, i will be using it for next loop
	push dx 
	push di

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
	pop di
	pop dx
	pop cx
ret

;###################################          Drunken bishop          ################################### 

drunkenbishop:						; translate to binary, make moves

	call arg2tobytes
	call modification
	call makeallmoves
	call makefingerprint
	call printfingerprint
ret

arg2tobytes: 						; translate hexadecimal bytes to binary

	push dx
	push cx
	push ax
	push si
	push di

	xor ax, ax
	mov ax, seg data
	mov ds, ax
	xor ax, ax
	xor dx, dx

	mov si, offset argument2		; si (source index) will iterate over arg2
	mov di, offset bytesequence0
	mov cx, 16 						; we will iterate through 16 bytes (32 ascii chars) of second argument

	charstobyte:
		mov ax, word ptr ds:[si] 	; load two characters
		call asciitobyte 			; translate two characters to one byte, returned in dl
		mov byte ptr ds:[di], dl 	; write next byte from dl to bytesequence0
		inc di 						; di+=1 - di is pointing to next byte in bytesequence0
		add si, 2					; si+=2 - si is pointing to next pair of chars
	loop charstobyte

	pop di
	pop si
	pop ax
	pop cx
	pop dx
ret

asciitobyte:						; translate two ascii chars to a byte, chars passed in ax, return byte in dl, confirmed working

	push cx 						; cx used for shifting to right half of byte
	xor dl, dl 						; return result in dl

	cmp al, 'a' 					; determine if al is number or letter
	JL alnumber 					
	JMP alletter

	alnumber: 						; if al is number
	sub al, 48 						; al-=48 (al - '0')
	JMP handleah 					; translate ah

	alletter: 						; if al is letter
	sub al, 87 						; al-=87 (al - 'a' + 10)
	JMP handleah 					; translate ah

	handleah: 						
	cmp ah, 'a' 					; determine if ah is number or letter
	JL ahnumber
	JMP ahletter

	ahnumber: 						; if ah is number
	sub ah, 48 						; ah-=48
	JMP asciitobyteend 				; proceed to end

	ahletter: 						; if ah is letter
	sub ah, 87						; ah-=87
	JMP asciitobyteend 				; proceed to end

	asciitobyteend:
	mov dl, al 						; dl=al
	mov cl, 4 						
	shl dl, cl 						; multiply dl by 4 (shift left 4 bits)
	add dl, ah 						; add lower half (al)

	pop cx
ret

modification:						; check if make modified asciiart, and prepare bytesequence0 for it #confirmed working

	push ax
	push cx
	push si
	push di

	xor ax, ax
	mov ax, seg data
	mov ds, ax
	xor ax, ax

	mov al, byte ptr ds:[argument1]
	cmp al, 49						; check if argument1=='1'
	JNE nomod						; if argument1!='1', proceed to making moves

	
	writesequence0to1:				; write bytes from bytesequence0 to bytesequence1 for easier xor later
	xor ax, ax						; ax=0
	xor cx, cx 						; cx=0
	mov al, byte ptr ds:[bytesequence0 +15]; copy last bytesequence0 char to first index in bytesequence1
	mov byte ptr ds:[bytesequence1], al
	xor al, al
	mov si, offset bytesequence0 	; now si (source index) is pointing to first char of bytesequence0
	mov di, offset bytesequence1 	
	inc di 							; now di (destination index) is pointing to second char of bytesequence1 (first is already copied)
	mov cl, 16d 					; set loop counter for 16 (length of bytesequence0)
	rewrite: 						
		mov al, byte ptr ds:[si] 	; load next byte from bytesequence0
		mov byte ptr ds:[di], al 	; write it to next place in bytesequence1
		inc si 						; si+=1
		inc di 						; di+=1
	loop rewrite


	doxor: 						  	; iterate through bytesequence1, xor every byte with the folllowing byte, store result in bytesequence0
	xor ax, ax  					; ax=0
	xor cx, cx 						; cx=0
	mov cl, 16d  					; set loop counter for 16
	mov si, offset bytesequence1 	; set si for first byte of bytesequence1
	mov di, offset bytesequence0 	; set di for first byte of bytesequence0
	xorbytes:
		mov al, byte ptr ds:[si] 	; read byte from bytesequence1
		mov ah, byte ptr ds:[si+1] 	; read following byte
		xor al, ah 					; xor al and ah, result is stored in al
		mov byte ptr ds:[di], al 	; write al to next byte in bytesequence0
		xor ax, ax 					; ax=0
		inc si 						; si+=1
		inc di  					; di+=1
	loop xorbytes


	nomod:
	pop di
	pop si
	pop cx
	pop ax
ret

makeallmoves: 						; return in dx position after all moves
	push ax
	push bx
	push cx

	mov ax, seg data
	mov ds, ax
	xor ax, ax

	xor dx, dx
	mov dx, 76							; in dx return position after all moves, by default - center (76)
	mov cx, 16							; loop over 16 bytes of bytesequence0
	mov bx, offset bytesequence0
	makemoves:
		mov al, byte ptr ds:[bx]
		call makemovesbyte
		inc bx
	loop makemoves
	pop cx
	pop bx
	pop ax
ret

makemovesbyte:						; input - byte in al, store in dx position after moves
	
	push bx
	push cx
	xor cx, cx
	mov cl, 4 						; set loop counter to four iterations

	makemove:
		; make vertical moves
		verticalmove:
		mov bl, al
		and bl, 00000010b
		cmp bl, 00000010b 			; check second bit, if equals 1 make move down, else - make move up
		JE makemovedown	

		makemoveup:		 
		call moveup 				; try to make move up, return target position in dx, don't modify chartable
		JMP horizontalmove

		makemovedown:				; same here, try to make move, return target position in dx
		call movedown
		JMP horizontalmove

		horizontalmove:
		mov bl, al
		and bl, 00000001b 			; check first bit, if equals 1 make move right, else - make move left
		cmp bl, 00000001b
		JE makemoveright

		makemoveleft:				; actual movement, try to make move to left(up/down), and if possible - move
		call moveleft
		JMP continue

		makemoveright:				; actual movement, try to make move to right(up/down), and if possible - move
		call moveright
		JMP continue

		continue: 					; shift byte 2 bits right
		push cx
		mov cl, 2
		shr al, cl 
		pop cx
	loop makemove 					

	pop cx
	pop bx
ret

moveup:								; try to make move up, return potential position in dx, takes dx as argument

	cmp dx, 16						; move one row up (dx-=17), if bishop is not in first row (dx>16 to make move)
	JLE nomoveup					; if dx <=16 don't move
	sub dx, 17						; if dx > 16 make move, return new position in dx

	nomoveup:
ret

movedown:							; try to make move down, return potential position in dx, takes dx as argument

	cmp dx, 136 					; move one row down (dx+=17), if bishop is not in last row (dx<136 to make move)
	JGE nomovedown 					; if dx >=136 don't move
	add dx, 17						; if dx < 136 make move, return new position in dx

	nomovedown:
ret

moveleft: 							; try to make move left and modify chartable if any movement possible, input dx - current pos, output dx - pos after move
	
	push ax
	push bx
	push cx

	mov ax, dx 						; make copy of dx in ax for checking dx % 17 
	mov cl, 17						; set the divisor to 17
	div cl
	cmp ah, 0 						; al=dx/17, ah=dx%17
	JE moveleftend 					; if dx%17!=0 we can move to the left (dx-=1), otherwise it means bishop is in 1st column and we can't make move to the left
	dec dx

	moveleftend:					; modify appropriate element in chartable
	mov bx, dx 						
	inc byte ptr ds:[chartable+bx]  ; chartable[dx]+=1

	pop cx
	pop bx
	pop ax
ret

moveright:							; try to make move right and modify chartable if any movement possible, input dx - current pos, outpot dx - pos after move

	push ax
	push bx
	push cx

	mov ax, dx 						; make copy of dx in ax for checking dx % 17
	mov cl, 17 						; set the divisor to 17
	div cl
	cmp ah, 16 						; check dx%17
	JE moverightend 				; if dx%17!=16 we can move to the right (dx+=1), otherwise it means bishop is in last column and we can't make move to the right
	inc dx 

	moverightend: 					; modify appropriate element in chartable
	mov bx, dx
	inc byte ptr ds:[chartable+bx]	; chartable[dx]+=1

	pop cx
	pop bx
	pop ax
ret

makefingerprint:					; for arg takes dx - last position of bishop, makes the fingerprint
	push ax
	push bx
	push cx
	push di

	mov ax, seg data
	mov ds, ax
	xor ax, ax

	mov bx, offset chartable
	mov di, 0
	mov cx, 153
	makefp:
		cmp di, 76
		JE changeS

		cmp di, dx
		JE changeE
		JMP otherchars

		changeS:
		mov byte ptr ds:[bx+di], 'S'
		JMP nextchar

		changeE:
		mov byte ptr ds:[bx+di], 'E'
		JMP nextchar

		otherchars:
		mov al, byte ptr ds:[bx+di]
		cmp al, 0
		JE space

		cmp al, 14
		JGE dash

		call findrightchar
		JMP nextchar
		
		dash:
		mov byte ptr ds:[bx+di], '^'
		JMP nextchar

		space:
		mov byte ptr ds:[bx+di], ' '
		JMP nextchar

		nextchar:
		inc di
	loop makefp

	pop di
	pop cx
	pop bx
	pop ax
ret

findrightchar:						; in al passed current number of visits, in bx passed offset of chartable, in di index of char
	
	char13:
	cmp al, 13
	JL char12
	mov byte ptr ds:[bx+di], '/'
	JMP endfind

	char12:
	cmp al, 12
	JL char11
	mov byte ptr ds:[bx+di], '#'
	JMP endfind

	char11:
	cmp al, 11
	JL char10
	mov byte ptr ds:[bx+di], '&'
	JMP endfind

	char10:
	cmp al, 10
	JL char9
	mov byte ptr ds:[bx+di], '%'
	JMP endfind

	char9:
	cmp al, 9
	JL char8
	mov byte ptr ds:[bx+di], '@'
	JMP endfind

	char8:
	cmp al, 8
	JL char7
	mov byte ptr ds:[bx+di], 'X'
	JMP endfind

	char7:
	cmp al, 7
	JL char6
	mov byte ptr ds:[bx+di], 'O'
	JMP endfind

	char6:
	cmp al, 6
	JL char5
	mov byte ptr ds:[bx+di], 'B'
	JMP endfind

	char5:
	cmp al, 5
	JL char4
	mov byte ptr ds:[bx+di], '*'
	JMP endfind

	char4:
	cmp al, 4
	JL char3
	mov byte ptr ds:[bx+di], '='
	JMP endfind

	char3:
	cmp al, 3
	JL char2
	mov byte ptr ds:[bx+di], '+'
	JMP endfind

	char2:
	cmp al, 2
	JL char1
	mov byte ptr ds:[bx+di], 'o'
	JMP endfind

	char1:
	mov byte ptr ds:[bx+di], '.'
	JMP endfind

	
	endfind:
ret

printfingerprint:					; print ascii art of fingerprint
	push bx
	push cx
	push dx
	push si
	push di
	
	mov di, offset chartable		; load to di chartable offset
		
	mov dx, offset topframe			; print top frame part
	call print
	mov dx, offset newline
	call print

	mov cx, 9						; load to cx 9 (9 rows)
	print_rows:

		push cx						; push cx to stack, i will be using cx to print 17 chars in each row
	
		mov dl, '|' 				; print element of frame
		mov ah, 6 
		int 21h 
		
		mov cx, 17					; load to cx 17 (for 17 columns/chars in row)
		print_element:
			mov dl, byte ptr ds:[di]
			mov ah, 6h 				; print char at di offset
			int 21h
				
			inc di					; di+=1, for printing next char
		loop print_element
				
		mov dl, '|' 				; print element of frame
		mov ah, 6h 
		int 21h
			
		mov dx, offset newline 		; print new line
		call print 

		pop cx 						; pop cx for outer loop
	loop print_rows
		
	mov dx, offset bottomframe		; print bottom frame
	call print

	pop di	
	pop si
	pop dx
	pop cx
	pop bx
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