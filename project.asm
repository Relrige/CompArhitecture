.model small
.stack 100h

ASCNull         EQU     0               ; ASCII null
ASCcr           EQU     13              ; ASCII carriage return
ASClf           EQU     10              ; ASCII line feed 
TailLen EQU 080h 
CommandTail EQU 081h

.data
    oneChar db ?
    params db 100 dup('$') 
    VAR DB 100*255 DUP('$')
    mybyte db " $"
    numparams dw ?
    keys DB 100h DUP('$')
    values DB 100h DUP('$')

.code
ORG 0100h
main proc
    MOV AX,@DATA
    MOV eS,AX

    call GetParams
    call readFile

    mov ax,es
    mov ds,ax

    ;call outPutPSP

    mov si, offset params
    mov di, offset VAR
    
    call PrintAllCountSubStr
    
    mov si, offset keys
    mov di, offset values
    ;call mergeSort
    call bubbleSort
    call PrintAll

exit2:
    MOV AH,4CH      ;end    
    INT 21H 
main endp

bubbleSort PROC
    push di
    push si
    call StrLength
    dec cx  ; count-1
    dec cx
outerLoop:
    push cx
    lea si, values
    lea di, keys
innerLoop:
    mov al, byte ptr[si]
    cmp al, byte ptr[si+1]
    jl nextStep
    xchg byte ptr[si+1], al
    mov byte ptr[si], al

    mov bl, byte ptr[di]
    xchg byte ptr[di+1], bl
    mov byte ptr[di], bl
nextStep:
    inc si
    inc di
    loop innerLoop
    pop cx
    loop outerLoop
    pop si 
    pop di
    ret                     
bubbleSort ENDP

PrintAll PROC
    push di
    push si 
    call StrLength
    dec cx
printallLoop:
    cmp cx, 0
    jle goOut
    xor ax, ax 
    mov al, byte ptr [di]
    cmp al, 0
    je nextLoop
    call numberOutPut2
    call outPutSpace
    xor ax,ax
    mov al, byte ptr [si]
    cmp al,'$'
    je goOut
    call numberOutPut2
    call outPutCRLF
nextLoop:
    inc si
    inc di
    dec cx
    jmp printallLoop
goOut:
    pop si 
    pop di
    ret                     
PrintAll ENDP
outPutSpace PROC
   mov ah, 02h
   mov dl, ' '
   int 21h
ret
outPutSpace ENDP
outPutCRLF PROC
   mov ah, 02h
   mov dl, ASCcr
   int 21h
   mov ah, 02h
   mov dl, ASClf
   int 21h
ret
outPutCRLF ENDP
inputKeysValues PROC
    push di
    push si
    push ax 
    mov di, offset keys
    mov si, offset values
    mov [di+byte ptr bx], bx
    mov [si+byte ptr bx], ax
    mov dx, [di] 
    inc bx
    pop ax
    pop si
    pop di
ret
inputKeysValues ENDP
PrintAllCountSubStr PROC
    xor bx,bx
startloop:
    call CountOccurrences
    call inputKeysValues
    ;call numberOutPut2
    call moveTONextLine
    cmp dx, 0ffffh
    je loopend
    jmp startloop
loopend:      
    ret                     ; Return to caller
PrintAllCountSubStr ENDP
moveTONextLine PROC
diLoop:
    cmp byte ptr [di], "$" 
    je EOFExit
    cmp byte ptr [di], ASCcr  
    je outFunc
    cmp byte ptr [di], ASClf 
    je outFunc
    inc di
    jmp diLoop
EOFExit:
    mov dx, 0ffffh
    ret
outFunc:
    inc di
    inc di
    ret                     ; Return to caller
moveTONextLine ENDP

CountOccurrences PROC
    push    bx
    push    cx
    push    si
    xor     cx, cx          ; Initialize count to 0
CountLoop:
    xor     dx, dx
    call    StrPos          ; Call StrPos to find the next occurrence
    cmp     dx, 0ffffh      ; Check if substring found (dx=-1 means not found)
    je      Done            ; If not found, exit loop
    inc     cx              ; Increment count
    add     di,dx             ; Move to the next character
    jmp     CountLoop       ; Repeat until end of string
Done:
    mov     ax, cx  
    pop     si              ; Restore registers
    pop     cx
    pop     bx
    ret                     ; Return to caller
CountOccurrences ENDP
GetParams PROC
;-----  Initialize counter (cx) and index registers (si,di)
        xor     ch, ch                  ; Zero upper half of cx
        mov     cl, [ds:TailLen]        ; cx = length of parameters
        inc     cx                      ; Include cr at end
        mov     si, CommandTail         ; Address parameters with si
        mov     di, offset params       ; Address destination with di
qw10:
        call    Separators              ; Skip leading blanks & tabs
        jne     qw20                    ; Jump if not a blank or tab
        inc     si                      ; Skip this character
        loop    qw10                    ; Loop until done or cx=0
qw20:
        push    cx                      ; Save cx for later
        jcxz    qw30                    ; Skip movsb if count = 0
        cld                             ; Auto-increment si and di
        rep     movsb                   ; copy cx bytes from ds:si to es:di
qw30:
        push    es                      ; Push es onto stack
        pop     ds                      ; Make ds = es
        pop     cx                      ; Restore length to cx
        xor     bx, bx                  ; Initialize parameter count
        jcxz    qw60                    ; Skip loop if length = 0
        mov     si, offset params       ; Address parameters with si
qw40:
        call    Separators              ; Check for blank, tab, or cr
        jne     qw50                    ; Jump if not a separator
        mov     [byte ptr si], 0ffffh        ; Change separator to null
        inc     bx                      ; Count number of parameters
qw50:
        inc     si                      ; Point to next character
        loop    qw40                    ; Loop until cx equals 0
qw60:
        dec si
        mov [byte ptr si], '$'
        mov     [numParams], bx         ; Save number of parameters
        ret                             ; Return to caller
GetParams ENDP
Separators PROC 
        mov al, [si] ; Get character at ds:si 
        cmp al, 20h ; Is char a blank? 
        je a10 ; Jump if yes 
        cmp al, 09h ; Is char a tab? 
        je a10 ; Jump if yes 
        cmp al, 00Dh ; Is char a cr? : 
        a10: 
        ret ; Return to caller : 
Separators ENDP 
numberOutPut2 PROC
    push ax
    push bx
    push cx
    cmp ax, 0ffffh
    je theEnd
    mov bl,100
    div bl
    add al, 48
    mov cl,al
    mov ch,ah
    mov mybyte, cl
    lea dx, mybyte
    cmp mybyte,'0'
    je skip
    call outDx
skip:
    xor ax,ax
    mov al,ch
    mov bl,10
    div bl
    add al, 48
    add ah, 48
    mov cl,al
    mov ch, ah 
    mov mybyte, cl
    lea dx, mybyte
    cmp mybyte,'0'
    je skip1
    call outDx
skip1:
    mov mybyte, ch
    lea dx, mybyte
    call outDx
theEnd:
   pop cx
   pop bx
   pop ax
   ret                    
numberOutPut2 ENDP

StrPos PROC
        push    ax              ; Save modified registers
        push    bx
        push    cx
        push    di
        push    si
   
        call    StrLength       ; Find length of target string
        mov     ax, cx          ; Save length(s2) in ax
        xchg    si, di          ; Swap si and di
        call    StrLength       ; Find length of substring
        mov     bx, cx          ; Save length(s1) in bx
        xchg    si, di          ; Restore si and di
        sub     ax, bx          ; ax = last possible index
        jb      q30            ; Exit if len target < len substring
        mov     dx, 0ffffh      ; Initialize dx to -1
q11:
        pop si
        push si
q10:
        inc     dx              ; For i = 0 TO last possible index
        mov     cl, BYTE PTR[di]      ; Save char at s[bx] in cl
        mov     ch, BYTE PTR[si]      ; Save char at s[bx] in cl
        cmp ch,"$"
        je q20
        cmp cl,"$"
        je q30
        cmp cl,ASCcr
        je q30
        cmp cl,ASClf
        je q30
        cmp ch, cl
        je q12
        cmp dx,ax
        je q30
        inc di
        jne q11
q12:
        inc di
        inc si
        jmp q10
q30:
        mov dx, 0ffffh
q20:
        ;sub dx,bx
        pop     si
        pop     di              ; Restore registers
        pop     cx
        pop     bx
        pop     ax
        ret                     ; Return to caller
StrPos ENDP

StrLength PROC
     push di
     push si
     xor     cx, cx          ; Set cx to 0 for counting
count_loop:
    cmp     byte ptr [di], '$' ; Check for null terminator
    je      end_count       ; If null terminator found, exit loop
    inc     cx              ; Increment count
    inc     di              ; Move to the next character
    jmp     count_loop      ; Repeat until null terminator found
end_count: 
   pop si
   pop di
   ret                     ; Return to caller
StrLength ENDP

outDx PROC   
    MOV AH,9                
    INT 21H
    ret
outDx ENDP

outPutPSP PROC
    mov dx, offset VAR     ;outPut input
    call outDx

    call outPutSpace   ; space output

    mov dx, offset params     ;outPut subStr
    call outDx
    ret
outPutPSP ENDP

readFile PROC
    mov si, offset VAR
read_next:
    mov ah, 3Fh
    mov bx, 0h               ; stdin handle
    mov cx, 1                ; 1 byte to read
    mov dx, offset oneChar   ; read to ds:dx 
    int 21h                  ;  ax = number of bytes read
    or ax,ax
    jz exit
    mov bl, oneChar
    mov es:[si], bl
    inc si
    jmp read_next
exit:
    ret
readFile ENDP
end main