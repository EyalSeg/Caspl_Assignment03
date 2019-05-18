STACK_SIZE equ 16 * 1024

CODEP	equ	0
FLAGSP	equ	4
SPP	equ	8
COROUTINE_SIZE equ 12


section .data
    msg db 'hello ', 10, 0
    print_decimal db '%d ', 10, 0

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

    mov ebx, [drone_coroutines]
    call start_coroutine

    add ebx, COROUTINE_SIZE
    call start_coroutine

        add ebx, COROUTINE_SIZE
    call start_coroutine

        add ebx, COROUTINE_SIZE
    call start_coroutine

        add ebx, COROUTINE_SIZE
    call start_coroutine

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

    jmp end_co



