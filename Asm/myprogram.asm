.MODEL SMALL
.STACK 100H
.DATA 

MSG1 DB "Enter your string: $"
MSG2 DB "Output: $" 
VAR  DB 100 DUP('$')

.CODE
MAIN PROC
MOV AX,@DATA
MOV DS,AX
MOV SI,OFFSET VAR   

LEA DX,MSG1             ;load msg1 into dx
MOV AH,9                ;set ah to printing string
INT 21H                 ;displaying msg

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







MOV Dl,OFFSET VAR       
MOV AH,9                
INT 21H         ;output 
MOV AH,4CH      ;end    
INT 21H         
 
MAIN ENDP
END MAIN 