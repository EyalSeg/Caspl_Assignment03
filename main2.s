STKSZ	equ	16*1024
	CODEP	equ	0
	FLAGSP	equ	4
	SPP	equ	8

COROUTINE_STRUCT_SIZE equ 12

section .rodata
	align 16

	global	numco
numco:	dd	3
CORS:	dd	CO1
	dd	CO2
	dd	CO3

section .data
	align 16
; Structure for first co-routine
CO1:	dd	Function1
Flags1:	dd	0
SP1:	dd	STK1+STKSZ

; Structure for second co-routine
CO2:	dd	Function1
Flags2:	dd	0
SP2:	dd	STK2+STKSZ

; Structure for third co-routine
CO3:	dd	Function2
Flags3:	dd	0
SP3:	dd	STK3+STKSZ


section .bss
	align 16
CURR:	resd	1
SPT:	resd	1
SPMAIN:	resd	1
STK1:	resb	STKSZ
STK2:	resb	STKSZ
STK3:	resb	STKSZ

DRONES_COUNT: resb 3 ; TODO: read from cmdline arg
DRONE_STACKS_PTR: resd 0
DRONE_COROUTINES_PTR: resd 0



section .text
	align 16
	extern	printf
	extern malloc
	extern free
	global	init_co_from_c
	global	start_co_from_c
	global	end_co
    global main

%macro free_bss_memory 1
    mov eax, [%1]
    push eax
    call free
    pop eax
%endmacro

main:
	call init_drone_coroutines

    mov eax, 0
    push eax
    call init_co_from_c
    pop eax


    mov eax, 1
    push eax
    call init_co_from_c
    pop eax


    mov eax, 2
    push eax
    call init_co_from_c
    pop eax


    mov eax, 2
    push eax
    call init_co_from_c
    pop eax

    mov eax, 2
    push eax
    call start_co_from_c
    pop eax

    jmp exit

exit:     
	free_bss_memory DRONE_STACKS_PTR
	free_bss_memory DRONE_COROUTINES_PTR

    mov     eax, 1 ; exit
    mov     ebx, 0 ; return value
    int     0x80

init_drone_coroutines:
	; init stacks
	mov eax, [DRONES_COUNT]
	mov ebx, STKSZ
	mul ebx

	push eax
	call malloc
	add esp, 4 ; discard 1 from the stack

	mov dword [DRONE_STACKS_PTR], eax

	; ; allocate memory for coroutine structs
	; mov eax, [DRONES_COUNT]
	; mov ebx, COROUTINE_STRUCT_SIZE
	; mul ebx

	; push eax
	; call malloc
	; add esp, 4

	; mov dword [DRONE_COROUTINES_PTR], eax


	ret

; Initalize a co-routine number given as an argument from C
init_co_from_c:
	push	EBP
	mov	EBP, ESP
	push	EBX

	mov	EBX, [EBP+8]
	mov	EBX, [EBX*4+CORS]

	call	co_init

	pop	EBX
	pop	EBP
	ret


;  EBX is pointer to co-routine structure to initialize
co_init:
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
	ret

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

; C-callable start of the first co-routine
start_co_from_c:
	push	EBP
	mov	EBP, ESP
	pusha
	mov	[SPMAIN], ESP             ; Save SP of main code

	mov	EBX, [EBP+8]		; Get number of co-routine
	mov	EBX, [EBX*4+CORS]       ; and pointer to co-routine structure
	jmp	do_resume


; End co-routine mechanism, back to C main
end_co:

	mov	ESP, [SPMAIN]            ; Restore state of main code

	popa
	pop	EBP
	ret
	

; Example functions using co-routine mechanism

FMT1:	db	"Func 1, co %lx, call %lx, pass %ld", 10, 0
FMT2:	db	"Func 2, co %lx, call %lx, pass %ld", 10, 0
msg db "HELLO ", 10, 0

; This function used as code for co-routines 0 and 1
Function1:

	push	dword	1
	push	dword [CORS+8]
	push	dword [CURR]
	push	dword FMT1
	call	printf
	add	ESP, 16

	mov	EBX, [CORS+8]
	call	dword resume

	push	dword	2
	push	dword [CORS+8]
	push	dword [CURR]
	push	dword FMT1
	call	printf
	add	ESP, 16

	mov	EBX, [CORS+8]
	call	dword resume

	jmp end_co

; This function used as code for co-routine 2
Function2:
	push	dword	1
	push	dword [CORS]
	push	dword [CURR]
	push	dword FMT2
	call	printf
	add	ESP, 16

	mov	EBX, [CORS]
	call	dword resume

	push	dword	2
	push	dword [CORS+4]
	push	dword [CURR]
	push	dword FMT2
	call	printf
	add	ESP, 16

	mov	EBX, [CORS+4]
	call	dword resume

	push	dword	3
	push	dword [CORS]
	push	dword [CURR]
	push	dword FMT2
	call	printf
	add	ESP, 16

	mov	EBX, [CORS]
	call	dword resume

	push	dword	4
	push	dword [CORS+4]
	push	dword [CURR]
	push	dword FMT2
	call	printf
	add	ESP, 16

	mov	EBX, [CORS+4]
	call	dword resume

	jmp end_co

