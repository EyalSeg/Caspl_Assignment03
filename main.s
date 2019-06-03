global COROUTINE_SIZE

STACK_SIZE equ 16 * 1024

CODEP	equ	0
FLAGSP	equ	4
SPP	equ	8
COROUTINE_SIZE equ 12

x_coordinate equ 8
y_coordinate equ 16
angle equ 24
drone_id equ 28
drone_score equ 32

drone_storage_size equ 32


section .data
    msg db 'hello ', 10, 0
    msg2 db 'DEBUG! ', 10, 0
    print_decimal_format db '%d', 0
    read_int_format db '%d' ,0
    read_float_format db '%f', 0
    print_hex_format db '0x%X', 0
    print_float_format db '%.2f', 0
    print_victory_format db 'Drone id %d: I am a winner', 10, 0
    read_format db '%d %d %d %f %f %h', 0
    newline db 10, 0
    comma db ',', 0

var1: dd 1.1212
twopi_degrees: dq 360.0
pi_degrees: dq 180.0


; Structure for the scheduler
scheduler_coroutine:	dd	scheduler_func
Flags3:	dd	0
scheduler_sp:	dd	scheduler_stack+STACK_SIZE

; Structure for the board printer
printer_coroutine: dd print_board
Flags4: dd 0
printer_sp: dd printer_stack + STACK_SIZE

; Structure for the target creator
createTarget_coroutine: dd createTarget
Flags5: dd 0
createTarget_sp: dd createTarget_stack + STACK_SIZE

section .bss
    align 16

    global drones_count
    global drone_coroutines
    
    SPT:	resd	1
    CURR:	resd	1
    SPMAIN:	resd	1

    drone_stacks resd 1 
    drone_coroutines resd 1
    drones_count resd 1 

    printer_stack resb STACK_SIZE
    scheduler_stack resb STACK_SIZE
    createTarget_stack resb STACK_SIZE

    lfsr resw 1
    beta resd 1
    distance resd 1
    num_of_targets resd 1
    print_every resd 1

    target_x resq 1
    target_y resq 1

    temp_buffer resq 1
    
section .text

align 16
    global main
    global end_co
    global start_coroutine
    global resume
    global createTarget

    extern printf
    extern malloc
    extern free
    extern sscanf

%macro free_bss_memory 1
    mov eax, [%1]
    push eax
    call free
    pop eax
%endmacro

; writes a 16-bit integer from eax to memory, after scaling it to [$1, $2]
; $1 - 8-bytes memory to store the float
; will put eax scaled down from 16-bit integer to [$2, $3] 
%macro int_to_scaled_float 3
    mov dword [%1], eax

    mov eax, 0xFFFF
    push dword eax

    fild dword [%1]
    fild dword [esp]
    fdiv

    mov eax, %3 - %2
    push dword eax

    fimul dword[esp]

    mov eax, %2
    push eax

    fiadd dword[esp]

    fstp qword [%1]

    add esp, 12 ; discard the pushed values
%endmacro

%macro print_comma 0
    push dword comma
    call printf
    add esp, 4
%endmacro

; takes a pointer to an 8-byte memory block of a float and prints it
%macro print_float 1
    ;mov eax, print_float_format
    push dword [%1 + 4]
    push dword [%1]
    ;push eax
    push dword print_float_format
    call printf
    add esp, 12
%endmacro

%macro print_hex 1
    push %1
    push print_hex_format
    call printf
    add esp, 8
%endmacro

%macro print_decimal 1
    push dword [%1]
    push print_decimal_format
    call printf
    add esp, 8
%endmacro

%macro print_newline 0
    push dword newline
    call printf
    add esp, 4
%endmacro

; calls %1 with 2 immidiate args $2 $3
%macro calli2 3
    push dword %3
    push dword %2
    call %1
    add esp, 8
%endmacro 

%macro dummy_print 0
    push dword msg
    call printf
    add esp, 4
%endmacro


main:
    ; mov dword [drones_count], 5 ; TODO : read from cmdline args
    ; mov word [lfsr], 0x3aab; TODO : read from cmdline args
    ; mov dword [beta], 90 ;TODO : read from cmdline args
    ; mov dword [distance], 30 ; TODO : read from cmdline args
    ; mov dword [num_of_targets], 3 ; TODO : read from cmdline args
    ; mov dword [print_every], 10 ; TODO : read from cmdline args

    push   ebp                               ; save Base Pointer (bp) original value
    mov    ebp, esp                          ; use Base Pointer to access stack contents

    mov    ebx, dword [ebp + 4*3]            ; get the second argument - argv


    push   ebx                          ; gets cmd parammeters into vars N,T,K,seed save as decimal and Beta, d as float
    push   drones_count
    push   read_int_format
    push   dword [ebx + 4*1]            ; get argv[1]= number of dornes N
    call   sscanf
    add    esp, 4*3
    pop    ebx

    push   ebx
    push   num_of_targets
    push   read_int_format
    push   dword [ebx + 4*2]            ; get argv[2]= number of target T
    call   sscanf
    add    esp, 4*3
    pop    ebx

    push   ebx
    push   print_every
    push   read_int_format
    push   dword [ebx + 4*3]            ; get argv[3]= number of step K
    call   sscanf
    add    esp, 4*3
    pop    ebx

    push   ebx
    push   beta
    push   read_float_format
    push   dword [ebx + 4*4]            ; get argv[4]= Beta
    call   sscanf
    add    esp, 4*3
    pop    ebx


    push   ebx
    push   distance
    push   read_float_format
    push   dword [ebx + 4*5]            ; get argv[5]=d
    call   sscanf
    add    esp, 4*3
    pop    ebx

    push   ebx
    push   lfsr
    push   read_int_format
    push   dword [ebx + 4*6]            ; get argv[6]=SEED
    call   sscanf
    add    esp, 4*3
    pop    ebx

    ; mov ebx, dword [esp + 4*3] ; get argv
    ; dummy_print

    ; push lfsr
    ; push distance
    ; push angle
    ; push print_every
    ; push num_of_targets
    ; push drones_count
    ; push read_format
    ; push ebx
    ; call sscanf
    ; add esp, 8 * 4 


    mov ebx, createTarget_coroutine
    call co_init

    call init_drone_coroutines

    mov ebx, printer_coroutine
    call co_init

    mov ebx, scheduler_coroutine
    call co_init

    mov ebx, createTarget_coroutine
    call start_coroutine

    ; ; small example of how to read from a drone of another coroutine
    ; mov eax, [drone_stacks]
    ; mov eax, [eax + STACK_SIZE - y_coordinate]

    ; push eax
    ; mov eax, print_decimal
    ; push eax
    ; call printf
    ; pop eax 
    ; pop eax
    
    jmp exit

; starts the coroutine in ebx
start_co:
	push EBP
	mov	EBP, ESP
	pusha
	mov	[SPMAIN], ESP             ; Save SP of main code

	jmp	do_resume


; End co-routine mechanism, back to C main
end_co:
	mov	ESP, [SPMAIN]            ; Restore state of main code
	popa
	pop	EBP
	ret

; TODO: move to a different file
scheduler_func:
    xor ecx, ecx

    scheduler_preiterate:
    xor esi, esi
    mov ebx, [drone_coroutines]
    scheduler_iterate_over_coroutines:
        cmp esi, dword [drones_count]
            je scheduler_iterate_over_coroutines_end

        call resume

        add ebx, COROUTINE_SIZE
        inc esi

        jmp scheduler_iterate_over_coroutines

    scheduler_iterate_over_coroutines_end:

    inc ecx
    cmp ecx, dword [print_every]
    jl scheduler_preiterate

    xor ecx, ecx
    mov ebx, printer_coroutine
    call resume
    
    jmp scheduler_preiterate; comment this line to disable the looping

    jmp end_co

print_board:
    print_float target_x
    print_comma
    print_float target_y
    print_newline

    printer_preiterate:
    xor esi, esi
    mov eax, [drone_stacks]

    printer_iterate:
        cmp esi, dword [drones_count]
            je printer_iterate_end

        ; eax points to the stack of some drone
        push eax
        call print_drone_from_stack
        pop eax

        add eax, STACK_SIZE
        inc esi

        jmp printer_iterate

    printer_iterate_end:

    mov ebx, scheduler_coroutine
    call resume

    jmp print_board

    


; returns the output bit of the lfsr and shifts it once
shift_lfsr:
    push    ebp             ; Save caller state
    mov     ebp, esp
    sub     esp, 4          ; Leave space for local var on stack
    pushad   

    xor edx, edx
    mov dx, word [lfsr]
    
    xor eax, eax
    xor ebx, ebx

    mov ax, 1
    and ax, dx ; get the first bit

    mov [ebp-4], eax    ; Save returned value, the rightmost-bit

    mov bx, 4
    and bx, dx
    shrd bx, bx, 2
    xor ax, bx

    mov bx, 8
    and bx, dx
    shrd bx, bx, 3
    xor ax, bx

    mov bx, 32
    and bx, dx
    shrd bx, bx, 5
    xor ax, bx 

    ; ax now holds the input bit
    shrd dx, dx, 1 ; shift once
    mov cx, 0x7FFF
    and dx, cx ; turn of dx's msb

    shrd ax, ax, 1 ; turn the lsb to msb
    or dx, ax  

    mov word[lfsr], dx

    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    mov     eax, [ebp-4]    ; place returned value where caller can see it
    add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


init_drone_coroutines:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad   

    ; init drone structs
    mov eax, [drones_count]
    mov ebx, STACK_SIZE
    mul ebx

    push eax
    call malloc
    add esp, 4 ; discard from the stack

    mov dword [drone_stacks], eax ; store the newly created memory block


    ; init drone coroutine structs
    mov eax, [drones_count]
    mov ebx, COROUTINE_SIZE
    mul ebx

    push eax
    call malloc
    add esp, 4

    mov dword [drone_coroutines], eax ; store the newly created memory block

    xor ecx, ecx
    mov ebx, [drone_coroutines]
    
    init_drone_coroutine_loop:
        cmp ecx, dword [drones_count]
            je init_drone_coroutines_ret

        mov eax, STACK_SIZE
        mul ecx
        add eax, [drone_stacks] ; eax now holds a pointer to the stack for the current coroutine
        add eax, STACK_SIZE ; eax now holds a pointer to the end of said stack

        mov dword [ebx + CODEP], drone_coroutine
        mov dword [ebx + FLAGSP], 0
        mov dword [ebx + SPP], eax 

        call co_init

        inc ecx
        mov dword [eax - drone_id], ecx ; write drone id
        add ebx, COROUTINE_SIZE
        jmp init_drone_coroutine_loop

    init_drone_coroutines_ret:

    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


; EBX is pointer to co-init structure of co-routine to be resumed
; CURR holds a pointer to co-init structure of the curent co-routine
resume:
	pushf			; Save state of caller
	pusha
	mov	EDX, [CURR]
	mov	[EDX+SPP],ESP	; Save current SP
do_resume:
	mov	ESP, [EBX+SPP]  ; Load SP for resumed co-routine
	mov	[CURR], EBX
	popa			; Restore resumed co-routine state
	popf
	ret                     ; "return" to resumed co-routine!

; DELETE ME
emptyfunc:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad   

    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller

createTarget:
    call generate_random_integer
    int_to_scaled_float target_x, 0, 100

    call generate_random_integer
    int_to_scaled_float target_y, 0, 100

    mov ebx, scheduler_coroutine
    call resume

    jmp createTarget


; takes a pointer to the stack containing the drone and prints the drone
print_drone_from_stack:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad   

    mov eax, [ebp + 8] 
    print_decimal eax + STACK_SIZE - drone_id   

    print_comma

    mov eax, [ebp + 8]    
    print_float eax + STACK_SIZE - x_coordinate

    print_comma

    mov eax, [ebp + 8]    
    print_float eax + STACK_SIZE - y_coordinate

    print_comma

    mov eax, [ebp + 8]    
    print_float eax + STACK_SIZE - angle

    print_comma

    mov eax, [ebp + 8]
    print_decimal eax + STACK_SIZE - drone_score

; TODO: remove after testing
    ; mov eax, [ebp + 8]    
    ; push eax
    ; call mayDestroy
    ; add esp, 4
    ; print_hex eax

    print_newline

    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


;generates a random 16-bit integer
generate_random_integer:
    push    ebp             ; Save caller state
    mov     ebp, esp
    sub     esp, 4          ; Leave space for local var on stack
    pushad   

    ; xor edi, edi
    ; mov ebx, 16

    ; generate_random_integer_loop:
    ; cmp ebx, 0
    ;     jle generate_random_integer_loop_end

    ; shld edi, edi, 1
    ; call shift_lfsr
    ; or edi, eax

    ; dec ebx
    ; jmp generate_random_integer_loop
    ; generate_random_integer_loop_end:

    mov ecx, 16
    .loop:
    call shift_lfsr
    loop generate_random_integer.loop, ecx

    xor eax, eax
    mov ax, word [lfsr]

    ; push eax
    ; mov ebx, print_hex_format
    ; push ebx
    ; call printf
    ; pop ebx
    ; pop eax

    mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    mov     eax, [ebp-4]    ; place returned value where caller can see it
    add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


;  EBX is pointer to co-routine structure to initialize
co_init:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad

	pusha
	bts	dword [EBX+FLAGSP],0  ; test if already initialized
	jc	init_done
	mov	EAX,[EBX+CODEP] ; Get initial PC
	mov	[SPT], ESP
	mov	ESP,[EBX+SPP]   ; Get initial SP

    mov	EBP, ESP        ; Also use as EBP

	push	EAX             ; Push initial "return" address
	pushf                   ; and flags
	pusha                   ; and all other regs
	mov	[EBX+SPP],ESP    ; Save new SP in structure
	mov	ESP, [SPT]      ; Restore original SP
init_done:
	popa

        ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller

; ebx holds the pointer to the coroutine struct
start_coroutine:
	push	EBP
	mov	EBP, ESP
	pusha
	mov	[SPMAIN], ESP             ; Save SP of main code

	jmp	do_resume


free_memory:
    free_bss_memory drone_stacks
    free_bss_memory drone_coroutines

    ret                     ; Back to caller

exit: 
    call free_memory
    mov     eax, 1 ; exit
    mov     ebx, 0 ; return value
    int     0x80


drone_coroutine:
    ; allocate local memory
    sub esp, drone_storage_size

    mov dword [ebp - drone_score], 0 ; init score to 0
    ; initalize 
    call generate_random_integer
    int_to_scaled_float ebp - x_coordinate, 0, 100

    call generate_random_integer
    int_to_scaled_float ebp - y_coordinate, 0, 100

    call generate_random_integer
    int_to_scaled_float ebp - angle, 0, 360


    ; print_float ebp - x_coordinate
    ; print_newline

    mov ebx, scheduler_coroutine
    call resume


    drone_coroutine_update_loop:

    push ebp
    call update_drone_movement
    call update_drone_tryhit_target
    pop ebp

    mov ebx, scheduler_coroutine
    cmp eax, 0
    je drone_coroutine_resume

    mov ebx, createTarget_coroutine

    drone_coroutine_resume:
    call resume
    jmp drone_coroutine_update_loop


; returns true if the target was destroyed
update_drone_tryhit_target:
    push    ebp             ; Save caller state
    mov     ebp, esp
    sub     esp, 4          ; Leave space for local var on stack
    pushad   


    mov esi, [ebp + 8]    
    push esi
    call mayDestroy
    pop esi

    ;if not hit - return
    cmp eax, 0
    je update_drone_tryhit_target_return

    mov eax, 0xFFFF ; return true

    inc dword [esi - drone_score]


    ;if hit, check if this drone won

    mov ebx, dword [esi - drone_score]
    mov ecx, dword [num_of_targets]


    cmp ebx, ecx
    jl update_drone_tryhit_target_return


    push dword [esi - drone_id] 
    push print_victory_format
    call printf
    add esp, 8

    jmp end_co

    update_drone_tryhit_target_return:
    mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    mov     eax, [ebp-4]    ; place returned value where caller can see it
    add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


; takes a pointer to the drone variable and updates it's movement 
update_drone_movement:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad   

    call update_drone_angle

    sub esp, 8 ; make room for a new float

    call generate_random_integer ; generate distance
    int_to_scaled_float ebp-8, 0, 50 ; generate a float in [0, 50]
    mov esi, [ebp + 8]

;update x coordinate
    fld qword [esi - angle]
    fcos

    fld qword [ebp-8]
    fmul st0, st1

    fld qword [esi - x_coordinate]
    fadd st0, st1
    calli2 wrap_float, 0, 100
    fstp qword [esi - x_coordinate]
    fstp st0
    fstp st0
    fstp st0

;update y cordinate
    fld qword [esi - angle]
    fsin
    fld qword [ebp-8]
    fmul st0, st1
    fld qword [esi - y_coordinate]
    fadd st0, st1
    calli2 wrap_float, 0, 100
    fstp qword [esi - y_coordinate]
    fstp st0
    fstp st0
    fstp st0

    add esp, 8 ; discard new float

    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller

update_drone_angle:
    sub esp, 8 ; make room a new float

    call generate_random_integer ; generate angle
    int_to_scaled_float ebp-8, -60, 60 ; generate a float in [-60, 60]

    mov esi, [ebp + 8]

    ; add the new float to the angle
    fld qword [esi - angle]
    fadd qword [ebp-8]

    calli2 wrap_float, 0, 360

    fst qword [esi - angle]

    add esp, 8 ; remove the read integer   
    ret        ; Back to caller


; TODO: FIX - it is not working!!
; ebp + 8 is a pointer to the drone's stack
mayDestroy:
    push    ebp             ; Save caller state
    mov     ebp, esp
    sub     esp, 4          ; Leave space for local var on stack
    pushad   

    finit
    xor eax, eax ; by default return false
    mov esi, [ebp + 8]

    sub esp, 16 ; make room for gamma and alpha
   ; check abs(arctan(y2-y1 / x2-x1)) < beta

    fld qword [target_y] ; y2
    fld qword [esi - y_coordinate] ; y1
    ; fsubp st1, st0
    fsub

    fld qword [target_x] ; x2
    fld qword [esi - x_coordinate] ; x1
    fsub ; x2 - x1 

    fpatan

    fst qword [ebp - 8] ; store gamma
    fld qword [esi - angle] ; load alpha
    fst qword [ebp - 16] ; store alpha
    fsub 
    fabs

    ; if abs(gamma - alpha) > pi add 2pi add 2pi to the smaller angle
    fld qword [pi_degrees]
    fcomp

    jbe mayDestroy_compare_beta

    finit

    fld qword [ebp - 8] ; gamma
    fld qword [ebp - 16] ; alpha
    fcom

    ja maydestroy_add2pi_to_gamma
    jmp maydestroy_add2pi_to_angle
     
    ; after each case, st1 and st0 will have modified alpha and modified gamma (unspecified order)
    maydestroy_add2pi_to_gamma: 
    finit
    fld qword [ebp - 8]
    fld qword [twopi_degrees]
    fadd
    fst qword [ebp - 8]
    jmp mayDestroy_compare_beta

    maydestroy_add2pi_to_angle:
    finit
    fld qword [ebp - 16]
    fld qword [twopi_degrees]
    fadd
    fst qword [ebp - 16]
    jmp mayDestroy_compare_beta


    mayDestroy_compare_beta:
    finit

    fld qword [ebp - 16]
    fld qword [ebp - 8]
    fsub
    fabs

    fld qword [beta]
    fcomip st0, st1
    
    jb mayDestroy_return

    ; check sqrt((y2-y2)^2 + (x2-x1)^2) < d
    mayDestroy_distance:
    finit

    fld qword [target_y] ; y2
    fld qword [esi - y_coordinate] ; y1

    fsubp 
    fmul st0, st0

    fld qword [target_x] ; x2
    fld qword [esi - x_coordinate] ; x1

    fsubp 
    fmul st0, st0

    faddp 
    fsqrt

    fld dword [distance]
    fcomip st0, st1

    jb mayDestroy_return

    mov eax, 0xFFFF ; return true


    mayDestroy_return:

    finit
    add esp, 16

    mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    mov     eax, [ebp-4]    ; place returned value where caller can see it
    add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller


; wraps the loaded float between $1, $2:
; if the float is < $1, float := float+$1
; if the float is > %2, float := float-$2
wrap_float:
    push    ebp             ; Save caller state
    mov     ebp, esp
    ;sub     esp, 4          ; Leave space for local var on stack
    pushad   

    fild dword [ebp + 8]
    fcomip

    jbe wrap_compare_2
    
    fild dword [ebp + 12]
    fadd st1, st0

    jmp wrap_return

    wrap_compare_2:
    fild dword [ebp + 12]

    fcomi

    ja wrap_return
    fsub st1, st0

    wrap_return:
    fstp st0 ;discard previous integer
    ;mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    ;mov     eax, [ebp-4]    ; place returned value where caller can see it
   ; add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller