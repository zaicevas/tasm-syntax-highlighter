; tasyn - TASM Syntax highlighter
; Program takes TASM source code and turns it into .htm file with highlighted core elements:
; registers, instructions, comments, immediate operands, data types, labels
; USAGE: tasyn <input_file> {-w} {-h} {-o <output_file}
; AUTHOR: Tomas Zaicevas <tozaicevas@gmail.com>
; 1.0 2018-05-17

.model small
.stack 2048d
.data

JUMPS                                   ; makes conditionals for far jumps
LOCALS @@                               ; all labels with prefix @@ will be local

category ENUM none, datatype, register, instruction, pointr
match category none

reserved struc                          ; reserved words, which we'll highlight
    _length db ?                        ; length of a string
    _string dw ?                        ; pointer to reserved string
    _type category ?
ends

include tags.inc                        ; ALL needed html tags
include reserved.inc                    ; Reserved words we'll scan for
include m_macros.inc                    ; multiline macros

; macros
newline_macro EQU 10
cr_macro EQU 13                         ; carriage return
bytes_read EQU 1024                     ; amount of bytes we read in each iteration
input_buffer_length EQU 2048
reserved_words EQU 143                  ; amount of reserved words we'll check for
; the bigger difference between input_buffer_length and bytes_read,
; the longer lines we can process

; FILE NEEDS
    file_name db 1, 13 dup('$')         ; input file name
    file_name2 db 1, 13 dup('$')        ; output file name
    file_name_length dw ?
    fhandle1 dw ?
    fhandle2 dw ?
    extension db "HTM", 0

; USER MESSAGES
    help_message db "Usage: tasyn <input_file> [options]", 13, 10, 13, 10, "OPTIONS:", 13, 10,\
     "-h ", 9, 9, 9, "Print the help message", 13, 10,\
     "-o <output_file>", 9, "Explicitly provide and output file name", 13, 10,\
     "-w ", 9, 9, 9, "White theme", 13, 10, '$'
    endl db 13, 10

; EXCEPTION USER MESSAGES
    err_wrong_filename db "Input/output file name is too long.", 13, 10,\
    "At max 8 chars for filename, 3 for extension", 13, 10, '$'
    err_line_too_long db "ERROR: At least one line is too long.", 13, 10, '$'
    err_open_file db "ERROR: File $"
    err_open_file_2 db "couldn't be opened.", 13, 10, '$'
    err_create_output db "ERROR: Htm file couldn't be created.", 13, 10, '$'
    err_read_input db "ERROR: Data from input file couldn't be read.", 13, 10, '$'
    err_close_file db "ERROR: File(s) couldn't be closed.", 13, 10, '$'

; SYNTAX ANALYSIS
    source_code db input_buffer_length dup (0)              ; buffer for input
    source_code_modified db 2048d dup (0)                   ; buffer for output

    bool RECORD b_immediate:1, b_comment:1, b_label:1, {
        b_last_chunk:1,                 ; have we read max amount of bytes?
        b_quotes_parity:1               ; is there an odd number of quotes in a line?
        b_continuation:1,               ; has line continuation symbol '\' been used?
        b_si_zero:1,                    ; do we have a newline when si == 0?
        b_datatype_found:1              ; have we found a data type in current line?
        b_white:1                       ; white theme?
        b_output:1                      ; has -o option been used?
        b_continuation_prev:1           ; did we have a continuation char in previous line?
        b_initial_quotes_parity:1       ; used in continuation checks
        b_instruction:1                 ; have we found an instruction in current line?
    }
    b_array bool <0, 0, 0, 0, 0, 0, 0, 0>

    quote db 0                          ; first spotted quote (single/double)

; pre-analysis
    bytes_process dw 0                  ; amount of bytes we'll process
    pushed_bytes dw 0                   ; amount of pushed bytes (a line which is split between two reads)

; SPECIAL CHARACTERS (to differentiate inappropriate label/word)
    sch_label db "?@_$&"
    sch_label_length EQU ($ - sch_label)
    sch_reg_left db "[:,+-"             ; these can go before registers
    sch_reg_left_length EQU ($ - sch_reg_left)
    sch_reg_right db "]:,+"
    sch_reg_right_length EQU ($ - sch_reg_right)

; scanner
    next_line_si dw 0                   ; the SI offset of the next newline, 0 otherwise
    comment_si dw 0                     ; SI source_code offset where comment begins (';')
    label_si dw 0                       ; SI source_code offset where label ends (':')
    immediate_si dw 0                   ; SI source_code offset where immediate operand starts
    continuation_si dw 0                ; SI source_code offset where line continuation begins ('\')

; syntax analyzer
    a_ip dw 0                           ; IP of syntax_analyzer return call
    word_length dw 0                    ; length of a word for searching pattern
    start_si dw 0                       ; needed in search_string

; html escape codes
    tag_left db "&lt;"
    tag_right db "&gt;"
.code

start:
    mov dx, @data
    mov ds, dx

    xor cx, cx
    mov cl, es:[80h]                    ; location of command line arguments
    cmp cx, 0
    jne input_cmd                       ; jump if we have any arguments
    call ex_wrong_arguments
input_cmd:
    call parse_cmd
    lea dx, file_name
open_input_file:
    call open_r                         ; open file in read-only mode
    mov fhandle1, ax                    ; save file descriptor
    call add_htm                        ; add .htm extension to file_name2
create_file:
    call create_f
open_output_file:
    call open_w                         ; open output file for writing
    mov fhandle2, ax
    call write_base                     ; write down the base structure of .htm file
read_input:
    call read_data                      ; read bytes specificied by bytes_read
    cmp ax, 0                           ; ax := amount of bytes read
    jne pre_analysis
    ; no data read
    cmp pushed_bytes, 0
    je close_file
pre_analysis:
    call pre_analyze
    cmpbool b_continuation_prev, 1
    je @@soft_reset
    reset_scanner_variables
    jmp syntax_analysis
@@soft_reset:
    reset_scanner_variables_limited
syntax_analysis:
    call syntax_analyzer
write_output_chunk:
    write di, source_code_modified
    jmp read_input
close_file:
    ; we have to add the last line there
    write ending_length, ending
    mov bx, fhandle1
    call close_f
    mov bx, fhandle2
    call close_f

    call exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INPUT procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

parse_cmd PROC NEAR                        ; get input file name through the cmd
    mov si, 0082h
    lea di, file_name

iterate:
    mov al, es:[si]
    cmp al, ' '
    jbe further
    cmp al, '-'
    je check_option
    jmp get_input_file

check_option:
    inc si
    dec cx
    mov al, es:[si]
    cmp al, 'h'
    je help_option
    cmp al, 'o'
    je output_option
    cmp al, 'w'
    je white_option
    call ex_wrong_arguments
output_option:
    setbool b_output, 1
    add si, 2
    sub cx, 2
    xor di, di
skip_white:
    mov al, es:[si]
    cmp al, ' '
    ja lets_go
    inc si
    dec cx
    jmp skip_white
lets_go:
    mov al, es:[si]
    cmp al, 0Dh
    je escape
    cmp al, ' '
    jbe iterate
    cmp al, '.'
    je ye
    cmp al, 'Z'
    jbe ye
    sub al, 20h
ye:
    cmp di, 12
    je ex_wrong_filename
    mov file_name2+di, al
    inc si
    inc di
    dec cx
    jmp lets_go
white_option:
    setbool b_white, 1
    jmp further
get_input_file:
    cmp file_name, 1
    jne ex_wrong_arguments
    xor di, di
@@iterate:
    mov al, es:[si]
    cmp al, ' '
    jbe further_input
    cmp di, 12
    je ex_wrong_filename
    mov file_name+di, al
    cmpbool b_output, 1
    je avoid_output
    cmp al, '.'
    je write_upper
    cmp al, 'Z'
    jbe write_upper
    sub al, 20h
write_upper:
    mov file_name2+di, al
avoid_output:
    inc si
    inc di
    dec cx
    jmp @@iterate
further_input:
    mov file_name+di, 0
further:
    inc si
    cmp cx, 0
    je escape
    loop iterate
escape:
    ret
help_option:
    call ex_wrong_arguments
parse_cmd endp

add_htm PROC NEAR                           ; adds .htm extension to output file name
    xor si, si
    lea di, file_name2
find_dot:
    cmp [di], byte ptr '.'
    je found_dot
    inc di
    inc si
    cmp si, 14                              ; in case there's -o without a dot
    je ex_wrong_arguments
    jmp find_dot
found_dot:
    xor si, si
    mov cx, 4d
add_extension:
    inc di
    mov dl, extension+si
    mov [di], dl
    inc si
    loop add_extension
    lea bx, file_name2
    sub di, bx
    mov file_name_length, di
    ret
add_htm endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

open_r PROC NEAR                            ; open file in read-only mode
    mov ah, 3dh
    mov al, 00                              ; read only const
    int 21h
    jc @@exception
    ret
@@exception:
    call ex_open_input
open_r endp

open_w PROC NEAR                            ; open file in write-only mode
    mov ah, 3dh
    mov al, 01h
    lea dx, file_name2
    int 21h
    jc @@exception
    ret
@@exception:
    call ex_open_output
open_w endp

create_f PROC NEAR                          ; create file with name of file_name2
    mov ah, 3ch
    xor cx, cx
    lea dx, file_name2
    int 21h
    jnc @@return
    call ex_create_output
@@return:
    ret
create_f endp

read_data PROC NEAR                         ; reads bytes specified by bytes_read
    mov si, pushed_bytes
    mov ah, 3fh
    mov bx, fhandle1
    mov cx,    bytes_read
    lea dx, source_code
    add dx, si
    int 21h
    jc @@exception                          ; read error
    ret
@@exception:
    call ex_read_input
read_data endp

close_f PROC NEAR                           ; close file
    mov ah, 3eh
    int 21h
    jc @@exception
    ret
@@exception:
    call ex_close_file
close_f endp

pre_analyze PROC NEAR                       ; makes the source_code buffer ready before syntax analysis:
; sets b_last_chunk, pushed_bytes, bytes_process variables
; pops an unprocessed line from the last iteration (if there was such line)
; calls exception if one of lines is too long
    pop di                                  ; IP
    cmp ax, bytes_read
    je not_last_chunk
    setbool b_last_chunk, 1
not_last_chunk:
    call pop_line
    add ax, pushed_bytes
    push ax
    push di
    reset_registers
    mov pushed_bytes, 0
    pop di
    pop cx

appropriate_amount:
    mov bytes_process, cx
    push di                                 ; push di (so we can make return)
    xor di, di
    ret
pre_analyze endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SYNTAX ANALYSIS procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

line_scanner PROC NEAR                      ; gather information about the line
    push si
    push dx
    push ax
    push next_line_si
    cmpbool b_continuation_prev, 1
    je soft_reset
    reset_scanner_variables
    jmp reset
soft_reset:
    getfield b_initial_quotes_parity al, b_array
    setbool b_quotes_parity, al
    reset_scanner_variables_limited
reset:
    pop next_line_si
    mov ax, input_buffer_length
    sub ax, bytes_read                      ; max size of a line
loopas:
    cmp si, ax
    ja validate_length                      ; the line possibly has more than bytes_read bytes
length_is_ok:
    cmp si, bytes_process                   ; check if we hit the last+1 byte of the input buffer
    je last_line
    mov dh, source_code+si
    cmp dh, cr_macro
    je set_next
    cmp dh, newline_macro
    je set_next
    cmpbool b_comment, 1
    je inc_si
    ; any of these checks will work only if we're not in comment
    cmp dh, ';'
    je found_semicolon
    cmp dh, 39d                             ; '
    je found_quote
    cmp dh, '"'
    je found_quote
    cmp dh, ':'
    je found_colon
    cmp dh, ','
    je found_comma
    cmp dh, '\'
    je found_continuation
inc_si:
    inc si
    jmp loopas
set_next:
    cmp si, 0
    jne not_zero
    setbool b_si_zero, 1
not_zero:
    mov next_line_si, si
    cmpbool b_continuation, 0
    je no_escape_found
    setbool b_continuation_prev, 1
    setbool b_continuation, 0
    getfield b_quotes_parity al, b_array
    setbool b_initial_quotes_parity, al
    jmp @@return
no_escape_found:
    setbool b_continuation_prev, 0
    jmp @@return
found_semicolon:
    call validate_comment
    jmp inc_si
found_quote:
    call validate_quote
    jmp inc_si
found_colon:
    call validate_label
    jmp inc_si
found_comma:
    cmpbool b_immediate, 1
    je inc_si
    call validate_immediate
    jmp inc_si
found_continuation:
    call validate_continuation
    jmp inc_si
validate_length:
    cmp next_line_si, 0
    jne length_is_ok
    call ex_line_too_long
last_line:                                  ; it's the last line of a chunk of data
    mov next_line_si, 0
@@return:
    pop ax
    pop dx
    pop si
    ret
line_scanner endp

push_line PROC NEAR                         ; push a line, which is split between two blocks of input (chunks of data)
    pop bx                                  ; pop the instruction pointer, which is pushed after we call the function
    mov ax, bytes_process
    mov pushed_bytes, ax
    sub pushed_bytes, si
    xor ax, ax
    mov cx, pushed_bytes
    mov si, bytes_process
    dec si                                  ; 1 less, since bytes_process is amount, not a last index
push_it:                                    ; we push in a reverse order
    mov al, source_code+si
    push ax
    dec si
    loop push_it
    push bx                                 ; push IP
    ret
push_line endp

pop_line PROC NEAR
    pop bx
    xor si, si
    xor dx, dx
    cmp pushed_bytes, 0
    je @@return
    mov cx, pushed_bytes
pop_it:
    pop dx
    mov source_code+si, dl
    inc si
    loop pop_it
@@return:
    push bx
    ret
pop_line endp

syntax_analyzer    PROC NEAR                ; analyze input source code byte-by-byte and write tags to output buffer
    pop a_ip                                ; save pushed IP so we can later return there
    jmp hook_first_symbol                   ; when si = 0, assume it's first symbol after newline
byte_loop:                                  ; loop through every byte of input file
    ; RESERVED REGISTERS:
    ; SI := offset of source_code (reading buffer) we're currently processing
    ; DI := offset of source_code_modified (writing buffer) we're currently on
    ; CX := loop counter
    ; DX := DL for current byte, DH for looping further and scanner
    ; AX := push line, AL for b_array bit-fields
    cmpbool b_si_zero, 1
    je si_zero
    cmp next_line_si, 0                     ; 0 if it's the last line of a chunk/file (depends on b_last_chunk)
    je hook_last_line
    cmp si, next_line_si
    jb analysis                             ; any symbols in a line (before \n)
    cmp si, next_line_si
    ja hook_first_symbol                    ; si > next_line_si (will happen only once, when si - next_line_si = 1)
    jmp end_of_line

si_zero:                                    ; this happens only if the input file starts with newline
    mov dl, source_code
    mov source_code_modified+di, dl
    inc si
    inc di
    dec cx
    cmp cx, 0
    je dont_write_char
    jmp hook_first_symbol

end_of_line:
    call check_comment                      ; possible </g>
    jmp write_char
hook_first_symbol:
    ; si > next_line_si, so IT'S THE FIRST SYMBOL AFTER A NEWLINE
    call line_scanner
    cmp next_line_si, 0
    je byte_loop
    call check_label                        ; possible </o>
    jmp byte_loop                           ; we need to recheck if next_line_si == 0
hook_last_line:
; if it's not the last chunk, then we've got to push it onto the stack
; otherwise we process it, write the chunk and finish the program
    cmpbool b_last_chunk, 1
    je analysis
; it's the start of a line, which has its end at the next block of data
; so we push it onto the stack
    call push_line
    jmp @@return

analysis:
; if we got there, it means we have symbols inside a line that we can work with
    mov dl, source_code+si
    cmp dl, '<'
    jne check_right
    write_tag 4, tag_left
    dec di
    jmp dont_write_char
check_right:
    cmp dl, '>'
    jne no_escape
    write_tag 4, tag_right
    dec di
    jmp dont_write_char
no_escape:
    ; Check if we hit end of label (':') and possibly write a </o>
    call e_label
    jmp is_it_comment
is_it_comment:
    ; Check if we hit start of comment (';') and possibly write a <g>
    cmpbool b_comment, 0
    je is_it_continuation
    cmp si, comment_si
    jb is_it_continuation
    cmp si, comment_si
    ja write_char
    ; we've found a semicolon, which is a start of a comment
    write_tag 3, gtag
    jmp write_char
is_it_continuation:                         ; everything after \ is ignored by tasm
    cmpbool b_continuation, 0
    je is_it_immediate
    cmp si, continuation_si
    jae write_char

is_it_immediate:
    cmpbool b_immediate, 0
    je continue
    cmp si, immediate_si
    jne continue
    cmpbool b_instruction, 0
    je continue
    write_tag 4, putag
    call write_immediate
    write_tag 5, putagoff
    cmp cx, 0
    je dont_write_char
    jmp byte_loop

continue:
    ; differentiate a word an check if it matches pattern (instruction/datatype/register)
    mov dl, source_code+si
    cmp dl, ' '
    jbe write_char                          ; write any whitespace character

    call differentiate_word                 ; pridet side case kur paskutinis simbolis
    cmp word_length, 0
    je write_char
    call search_string                      ; check if we've matched a string and change MATCH variable
    call check_match                        ; put according tags, if we've found something
    cmp cx, 0
    je dont_write_char
    jmp analysis

write_char:
    mov dl, source_code+si
    mov source_code_modified+di, dl
dont_write_char:
    cmp cx, 0
    je @@return
    inc si
    inc di
    loop byte_loop
@@return:
    push a_ip                               ; push IP so ret can pop and jump there
    ret
syntax_analyzer endp

write_base PROC NEAR                        ; WRITE STARTING TAGS, depending on chosen theme
    cmpbool b_white, 0
    je write_dark
    write base_tags_length, base_tags
    write white_theme_length, white_theme
    write 4, h1_tag
    write file_name_length, file_name2
    write ending_tags2_length, ending_tags2
    jmp @@return
write_dark:
    write base_tags_length, base_tags
    write dark_theme_length, dark_theme
    write dark_background_length, dark_background
    write 4, h1_tag
    write file_name_length, file_name2
    write ending_tags2_length, ending_tags2
@@return:
    ret
write_base endp

; "PRIVATE" (only for implementation of other procedures) PROCEDURES:

validate_comment PROC NEAR                  ; checks whether the semicolon
; we've found in line scanner is a legit start of a comment
    cmp quote, 0
    je legit_comment
    cmpbool b_quotes_parity, 1
; if there's a odd number of firstly-matched quotes BEFORE the semicolon, then it's not a comment
    je @@return
legit_comment:
    setbool b_comment, 1
    mov comment_si, si
@@return:
    ret
validate_comment endp

validate_quote PROC NEAR                    ; increments variables after we've found a quote
    cmp quote, 0
    je first_quote
    cmp quote, dh
    jne @@return
    cmpbool b_quotes_parity, 1
    je to_zero
    setbool b_quotes_parity, 1
    jmp @@return
to_zero:
    setbool b_quotes_parity, 0
    jmp @@return
first_quote:
    mov quote, dh
    setbool b_quotes_parity, 1
@@return:
    ret
validate_quote endp

validate_immediate PROC NEAR                ; check whether we have an immediate operand
; gets called after we've found an instruction or comma
    push si
skip_whitespaces:
    inc si
    cmp si, bytes_process
    je @@return
    cmp si, comment_si
    je @@return
    mov dh, source_code+si
    cmp dh, cr_macro
    je @@return
    cmp dh, newline_macro
    je @@return
    cmp dh, '-'
    je found_imm
    cmp dh, 39d                             ; '
    je found_imm
    cmp dh, '"'
    je found_imm
    cmp dh, 33d
    jb skip_whitespaces
    ; not a whitespace
    cmp dh, '0'
    jb @@return
    cmp dh, '9'
    ja @@return
found_imm:
    setbool b_immediate, 1
    mov immediate_si, si
@@return:
    pop si
    ret
validate_immediate endp

validate_label PROC NEAR                    ; checks whether the ':'
; we've found in line scanner is semantically a label
; to do this, we need to check if there's only one word in our line
    push di
    push bx
    push dx
    mov di, next_line_si
    ; firstly we need to skip all whitespaces at the start of a line(they're correct syntaxically)
@@skip_whitespaces:
    mov dh, source_code+di
    cmp dh, ' '
    ja repeat_loop
    inc di
    jmp @@skip_whitespaces
repeat_loop_inc:
    inc di
repeat_loop:                                ; we search for any symbol that is not a
; letter, number or sch, if we found such char, its not a label
    cmp di, si
    je legit_label
    cmp di, si
    ja @@return                             ; e.g. "       :"
    mov dh, source_code+di
    xor bx, bx
special_ch_loop:
    cmp dh, sch_label+bx
    je repeat_loop_inc
    inc bx
    cmp bx, sch_label_length
    jne special_ch_loop
    cmp dh, '0'
    jb @@return
    cmp dh, 'z'
    ja @@return
    cmp dh, '9'
    ja not_number
    jmp repeat_loop_inc
not_number:
    cmp dh, 'A'
    jb @@return
    cmp dh, '['
    jb repeat_loop_inc
    cmp dh, 'a'
    jb @@return
    jmp repeat_loop_inc
legit_label:
    mov dh, source_code+di+1                ; e.g. record "b_last_chunk:1"
    cmp dh, ' '
    ja @@return
    setbool b_label, 1
    mov label_si, si
@@return:
    pop dx
    pop bx
    pop di
    ret
validate_label endp

validate_continuation PROC NEAR             ; checks whether the '\' we've found is not inside quotes
    cmpbool b_quotes_parity, 1
    je @@return
    setbool b_continuation, 1
    mov continuation_si, si
@@return:
    ret
validate_continuation endp

check_label PROC NEAR                       ; checks whether we have a label and if yes - writes a <o> tag
    cmpbool b_label, 0
    je @@return
    write_tag 3, otag                       ; write a LABEL tag if we'll have one in this line
@@return:
    ret
check_label endp

check_comment PROC NEAR                     ; checks whether we have a comment and if yes - writes a </g> tag
    cmpbool b_comment, 0                    ; si == next_line_si
    je @@return
    write_tag 4, gtagoff
@@return:
    ret
check_comment endp

e_label PROC NEAR                           ; is it an end of label?
    add sp, 2
    cmpbool b_label, 0
    je is_it_comment
    cmp si, label_si
    jne is_it_comment
    write_tag 4, otagoff
    jmp write_char
e_label endp

differentiate_word PROC NEAR
    push si
    push ax
    push cx
    mov word_length, 0
    mov ax, si
check_letter:
    mov dl, source_code+si
    cmp dl, 'A'
    jb end_of_a_word
    cmp dl, 'z'
    ja end_of_a_word
    cmp dl, 96d
    ja loop_further                         ; lower letter
    cmp dl, 91d
    jae end_of_a_word
loop_further:
    inc si
    loop check_letter
; if we're there -> cx == 0 and we're out of symbols
; (means we're at the very last line and there's no empty line at the end)
end_of_a_word:
    sub si, ax
    mov word_length, si
@@return:
    pop cx
    pop ax
    pop si
    ret
differentiate_word endp

search_string PROC NEAR
;    AX := counter, BX := pointer to struct,  CX := loop,    DX := cmp bytes
    mov match, none
    push_registers
    xor ax, ax
    mov bx, offset _strings
    mov start_si, si
repeat:
    mov dx, word_length
    cmp dl, [bx]._length
    jne not_matched
    cmp dh, 0
    ja repeat
    xor di, di
    xor cx, cx
    mov si, start_si
    mov cl, [bx]._length
    mov di, [bx]._string
looperz:
    mov dl, source_code+si
    cmp dl, 'a'
    jae lower_case
    add dl, 20h                             ; if its upper case, we store the same letter, but in lower case
lower_case:
    cmp dl, byte ptr [di]
    jne not_matched
    inc si
    inc di
    loop looperz
    ;we've found a matching string
    mov dh, [bx]._type
    mov match, dh
    jmp @@return
not_matched:
    inc ax
    cmp ax, reserved_words
    je @@return
    add bx, size _strings
    jmp repeat
@@return:
    pop_registers
    ret
search_string endp

check_match PROC NEAR
    push ax
    push bx
    cmp match, none
    je not_matched
    cmpbool b_datatype_found, 1
    je not_matched
    cmpbool b_label, 1
    je not_matched
    cmp match, instruction
    je f_instruction
    cmp match, register
    je f_register
    cmp match, pointr
    je f_ptr
    ; data type
    setbool b_datatype_found, 1
    write_tag 3, ttag
    write_part word_length
    write_tag 4, ttagoff
    jmp @@return
f_instruction:
    cmp si, 0
    je legit_right
    mov dl, source_code+si-1
    cmp dl, ' '
    jbe legit_right
    jmp not_matched
legit_right:
    mov ax, bytes_process
    cmp ax, si                              ; last byte, we can't check the next byte
    je legit_instruction
    add si, word_length                     ; word + 1 char
    mov dl, source_code+si
    cmp dl, ' '
    ja invalid_instruction
    sub si, word_length
    jmp legit_instruction
invalid_instruction:
    sub si, word_length
    jmp not_matched
legit_instruction:
    setbool b_instruction, 1
    write_tag 3, btag
    write_part word_length
    write_tag 4, btagoff
    cmpbool b_immediate, 1                  ; if it's after comma, we've found it in scanner
    je @@return
    call validate_immediate
    cmpbool b_immediate, 0
    je @@return

    mov ax, si
    mov bx, immediate_si
    sub bx, ax
    write_part bx
    write_tag 4, putag
    call write_immediate
    write_tag 5, putagoff
    setbool b_immediate, 0                  ; safeguard
    jmp @@return

f_register:
    mov ax, di
    mov bx, si
    push cx
start_of_a_word:
    cmp si, 0
    je legit_start
    mov dl, source_code+si-1
    xor di, di
    cmp dl, ' '
    jbe legit_start                         ; it's either whitespace before a start of a word, or it's special char
    mov cx, sch_reg_left_length
special_reg_characters:
    mov dh, sch_reg_left+di
    cmp dl, dh
    je legit_start
    inc di
    loop special_reg_characters
    jmp not_legit_register
legit_start:                                ; we'll now find the end of a word
    xor di, di
    add si, word_length
    mov dl, source_code+si
    cmp dl, ' '
    jbe legit_register
    mov cx, sch_reg_right_length
special_reg_ch_right:
    mov dh, sch_reg_right+di
    cmp dl, dh
    je legit_register
    inc di
    loop special_reg_ch_right

not_legit_register:
    pop cx
    mov di, ax
    mov si, bx
    jmp not_matched

legit_register:
    pop cx
    mov di, ax
    mov si, bx
    write_tag 3, rtag
    write_part word_length
    write_tag 4, rtagoff
    jmp @@return

f_ptr:
    write_part word_length
    cmpbool b_instruction, 0
    je @@return
    cmpbool b_immediate, 1
    je @@return
    call validate_immediate
    cmpbool b_immediate, 0
    je @@return
    mov ax, si
    mov bx, immediate_si
    sub bx, ax
    write_part bx
    write_tag 4, putag
    call write_immediate
    write_tag 5, putagoff
    setbool b_immediate, 0                  ; safeguard
    jmp @@return
not_matched:
    write_part word_length
@@return:
    pop bx
    pop ax
    ret
check_match endp

write_immediate PROC NEAR
    push bx
    push si
find_whitespace:
    cmp si, bytes_process
    je not_space_immediate
    mov dl, source_code+si
    cmp dl, 39                              ; '
    je add_2
    cmp dl, '"'
    je add_2
    cmp dl, ' '
    jbe not_space_immediate
    cmp dl, ';'                             ; could also be a comment just after immediate operand
    je not_space_immediate
    inc si
    jmp find_whitespace
add_2:
    add si, 3
not_space_immediate:
    pop ax
    mov bx, si
    sub bx, ax
    mov si, ax
    write_part bx
    pop bx
    ret
write_immediate endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXCEPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ex_wrong_arguments PROC                     ; wrong structure of command line options
    print help_message
    call exit
ex_wrong_arguments endp

ex_wrong_filename PROC                      ; input/output file name is too long
    print err_wrong_filename
    call exit
ex_wrong_filename endp

ex_line_too_long PROC                       ; one of the lines is too long
    print err_line_too_long
    call exit
ex_line_too_long endp

ex_open_input PROC                          ; failed to open input file
    print err_open_file
    print file_name
    print err_open_file_2
    call exit
ex_open_input endp

ex_create_output PROC                       ; failed to create output (.htm) file
    print err_create_output
    call exit
ex_create_output endp

ex_open_output PROC                         ; failed to open output file
    print err_open_file
    print file_name2
    print err_open_file_2
    call exit
ex_open_output endp

ex_read_input PROC                          ; failed to read data from output file
    print err_read_input
    call exit
ex_read_input endp

ex_close_file PROC                          ; failed to close at least one of the files
    print err_close_file
    call exit
ex_close_file endp

exit PROC
    mov ah, 4ch
    int 21h
exit endp

end start
