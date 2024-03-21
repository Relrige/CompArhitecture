.MODEL SMALL
.STACK 100H

BufSize         EQU     255             ; Maximum string size (<=255)
ASCNull         EQU     0               ; ASCII null
ASCcr           EQU     13              ; ASCII carriage return
ASClf           EQU     10              ; ASCII line feed 
TailLen EQU 080h 
CommandTail EQU 081h

.DATA 
    oneChar db ?
    params db 100h dup('$') 
    VAR DB 100h DUP('$')
    mybyte db " $"
    numparams dw ?

.CODE
MAIN PROC
MOV AX,@DATA
MOV DS,AX

LEA DX,MSG1             ;load msg1 into dx
MOV AH,9                ;set ah to printing string
INT 21H                 ;displaying msg1

MOV SI, OFFSET VAR   ;sets si to var

COME:   
MOV AH,1                ;Read Character from Standard Input
INT 21H                 ;Trigger interrupt 21H to wait for user input and store it in AL.

CMP AL,13         ;check for enter
JE HERE           ;jump to output (here)
MOV [SI],AL       ;stroing data at SI
INC SI            ;   
JMP COME          ;returning to input


HERE:
MOV AH,2                 
INT 21H                    
MOV DL,0DH                  
MOV DL,0AH                  
INT 21H                     

LEA DX,MSG2        
MOV AH,9            
INT 21H             


mov si, offset MSG3 
mov di, OFFSET VAR

call StrPos

mov si, offset answer
mov [si], "A"
inc si
add dx,30h
mov [si], dx
MOV dx, OFFSET answer       
MOV AH,9                
INT 21H         ;output

MOV AH,4CH      ;end    
INT 21H         
 
MAIN ENDP
CountOccurrences PROC
    push    bx
    push    cx
    push    di
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
    pop     di
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
        mov     [byte ptr si], 0        ; Change separator to null
        inc     bx                      ; Count number of parameters
qw50:
        inc     si                      ; Point to next character
        loop    qw40                    ; Loop until cx equals 0
qw60:
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
    mov bl,100
    div bl
    add al, 48
    mov cl,al
    mov ch,ah
    mov mybyte, cl
    lea dx, mybyte
    call outDx
    
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
    call outDx
    mov mybyte, ch
    lea dx, mybyte
    call outDx
   ret                     ; Return to caller
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
        mov si, offset params
q10:
        inc     dx              ; For i = 0 TO last possible index
        mov     cl, BYTE PTR[di]      ; Save char at s[bx] in cl
        mov     ch, BYTE PTR[si]      ; Save char at s[bx] in cl
        cmp ch,"$"
        je q20
        cmp cl,"$"
        je q30
        cmp ax,dx
        je q30
        cmp ch, cl
        je qwer
        inc di
        jne q11
qwer:
        inc di
        inc si
        jmp q10
q30:
        mov dx, 0ffffh
        xor bx,bx
q20:
        sub dx,bx
        pop     si
        pop     di              ; Restore registers
        pop     cx
        pop     bx
        pop     ax
        ret                     ; Return to caller
StrPos ENDP

StrCompare PROC
        push    ax              ; Save modified registers
        push    di
        push    si
        cld                     ; Auto-increment si
@@10:
        lodsb                   ; al <- [si], si <- si + 1
        scasb                   ; Compare al and [di]; di <- di + 1
        jne     @@20            ; Exit if non-equal chars found
        or      al, al          ; Is al=0? (i.e. at end of s1)
        jne     @@10            ; If no jump, else exit
@@20:
        pop     si              ; Restore registers
        pop     di
        pop     ax
        ret                     ; Return flags to caller
StrCompare ENDP

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
   ;dec cx
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
    mov dx, offset VAR     
    MOV AH,9                
    INT 21H 

    mov dx, offset params     
    MOV AH,9                
    INT 21H
    ret
outPutPSP ENDP

readFile PROC
    mov si, offset VAR
read_next:
    mov ah, 3Fh
    mov bx, 0h  ; stdin handle
    mov cx, 1   ; 1 byte to read
    mov dx, offset oneChar   ; read to ds:dx 
    int 21h                  ;  ax = number of bytes read
    mov bl, oneChar
    cmp bl,"$"
    je exit
    cmp bl,ASClf
    je exit
    cmp bl,ASCcr
    je exit
    cmp bl,ASCNull
    je exit
    mov es:[si], bl
    inc si
    or ax,ax
    jnz read_next
    jmp exit
exit:
    ret
readFile ENDP

END MAIN