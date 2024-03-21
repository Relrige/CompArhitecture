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


END MAIN