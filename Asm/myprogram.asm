.model small
.model small
.stack 100h

.code
main PROC
    ; ds = PSP
    ; copy param
    xor ch,ch
    mov cl, ds:[80h]   ; at offset 80h length of "args"
write_char:
    test cl, cl
    jz write_end
    mov si, 81h        ; at offest 81h first char of "args"
    add si, cx
    mov ah, 02h
    mov dl, ds:[si]
    int 21h
    dec cl
    jmp write_char
write_end:
main ENDP
END main