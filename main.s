global COROUTINE_SIZE

STACK_SIZE equ 16 * 1024

CODEP	equ	0
FLAGSP	equ	4
SPP	equ	8
COROUTINE_SIZE equ 12


section .data
    msg db 'hello ', 10, 0
    msg2 db 'DEBUG! ', 10, 0
    print_decimal db '%d ', 10, 0

    ; Structure for the scheduler
scheduler_coroutine:	dd	scheduler_func
Flags3:	dd	0
scheduler_sp:	dd	scheduler_stack+STACK_SIZE

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


    
section .text

align 16
    global main
    global end_co
    global start_coroutine
    global resume

    extern printf
    extern malloc
    extern free

%macro free_bss_memory 1
    mov eax, [%1]
    push eax
    call free
    pop eax
%endmacro

main:
    mov dword [drones_count], 5 ; TODO : read from cmdline args

    call init_drone_coroutines

    mov ebx, scheduler_coroutine
    call co_init
    call start_coroutine
    ; mov ebx, [drone_coroutines]
    ; call start_coroutine

    ; add ebx, COROUTINE_SIZE
    ; call start_coroutine

    ;     add ebx, COROUTINE_SIZE
    ; call start_coroutine

    ;     add ebx, COROUTINE_SIZE
    ; call start_coroutine

    ;     add ebx, COROUTINE_SIZE
    ; call start_coroutine


    jmp exit

; starts the coroutine in ebx
start_co:
    mov eax, msg2
    push eax
    call printf
    pop eax

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
; TODO: loop over the coroutines for resume (maybe wait until a real drone coroutine is created)
scheduler_func:
    xor esi, esi
    mov ebx, [drone_coroutines]
    iterate_over_coroutines:
        cmp esi, dword [drones_count]
            je iterate_over_coroutines_end

        call resume

        add ebx, COROUTINE_SIZE
        inc esi

        jmp iterate_over_coroutines

    iterate_over_coroutines_end:
    
    jmp end_co


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

        mov dword [ebx + CODEP], stupid_coroutine ; TODO
        mov dword [ebx + FLAGSP], 0
        mov dword [ebx + SPP], eax 

        call co_init

        inc ecx
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

stupid_coroutine:
    mov eax, msg
    push eax
    call printf
    pop eax

    mov ebx, scheduler_coroutine
    call resume



