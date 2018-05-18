; MULTILINE MACROS

setbool macro field, flag                       ; sets a certain bit-field in b_array
    mov bl, flag
    maskflag b_array, not mask field            ; clear the field we want to set, so we can OR it
    setfield field b_array, bl
endm

cmpbool macro field, flag                       ; saves the value of a bit-field in b_array in BL and compares with the flag
    getfield field bl, b_array
    cmp bl, flag
endm

print macro arg
    lea dx, arg
    mov ah, 09h
    int 21h
endm

write macro arg, arg2                           ; WRITE TO FILE, arg = irasymo baitu skaicius, arg2 = irasymo buferis
    mov ah, 40h
    mov bx, fhandle2
    mov cx, arg
    lea dx, arg2
    int 21h
endm

write_tag macro arg, arg2                       ; WRITE TO BUFFER, arg -> irasymo baitu skaicius, arg2 -> tago buferis
    local @@loop
    push cx
    push ax
    push bx
    mov ax, arg
    mov cx, ax
    xor bx, bx
@@loop:
    mov al, arg2+bx
    mov source_code_modified+di, al
    inc di
    inc bx
    loop @@loop
    pop bx
    pop ax
    pop cx
endm

write_part macro arg                            ; write part of input buffer to output buffer, arg := amount of bytes to write
    local @@loop
    push cx
    mov cx, arg
@@loop:
    mov dl, source_code+si
    mov source_code_modified+di, dl
    inc si
    inc di
    loop @@loop
    pop cx
    sub cx, arg
endm

reset_registers macro
    xor si, si
    xor di, di
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
endm

reset_scanner_variables macro
    mov next_line_si, 0
    mov comment_si, 0
    mov label_si, 0
    mov immediate_si, 0
    mov continuation_si, 0
    mov quote, 0

    setbool b_label, 0
    setbool b_comment, 0
    setbool b_immediate, 0
    setbool b_continuation, 0
    setbool b_quotes_parity, 0
    setbool b_si_zero, 0
    setbool b_datatype_found, 0
    setbool b_instruction, 0
endm

reset_scanner_variables_limited macro
    mov next_line_si, 0
    mov comment_si, 0
    mov label_si, 0
    mov immediate_si, 0
    mov continuation_si, 0

    setbool b_si_zero, 0
endm

push_registers macro
    push ax
    push bx
    push cx
    push dx
    push si
    push di
endm

pop_registers macro
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm
