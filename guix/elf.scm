;;; Guile ELF reader and writer

;; Copyright (C)  2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; This file was taken from the Guile 2.1 branch, where it is known as
;;; (system vm elf), and renamed to (guix elf).  It will be unneeded when Guix
;;; switches to Guile 2.1/2.2.
;;;
;;; A module to read and write Executable and Linking Format (ELF)
;;; files.
;;;
;;; This module exports a number of record types that represent the
;;; various parts that make up ELF files.  Fundamentally this is the
;;; main header, the segment headers (program headers), and the section
;;; headers.  It also exports bindings for symbolic constants and
;;; utilities to parse and write special kinds of ELF sections.
;;;
;;; See elf(5) for more information on ELF.
;;;
;;; Code:

(define-module (guix elf)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:export (has-elf-header?

            (make-elf* . make-elf)
            elf?
            elf-bytes elf-word-size elf-byte-order
            elf-abi elf-type elf-machine-type
            elf-entry elf-phoff elf-shoff elf-flags elf-ehsize
            elf-phentsize elf-phnum elf-shentsize elf-shnum elf-shstrndx

            ELFOSABI_NONE ELFOSABI_HPUX ELFOSABI_NETBSD ELFOSABI_GNU
            ELFOSABI_SOLARIS ELFOSABI_AIX ELFOSABI_IRIX ELFOSABI_FREEBSD
            ELFOSABI_TRU64 ELFOSABI_MODESTO ELFOSABI_OPENBSD
            ELFOSABI_ARM_AEABI ELFOSABI_ARM ELFOSABI_STANDALONE

            ET_NONE ET_REL ET_EXEC ET_DYN ET_CORE

            EM_NONE EM_SPARC EM_386 EM_MIPS EM_PPC EM_PPC64 EM_ARM EM_SH
            EM_SPARCV9 EM_IA_64 EM_X86_64

            elf-header-len elf-header-shoff-offset
            write-elf-header

            (make-elf-segment* . make-elf-segment)
            elf-segment?
            elf-segment-index
            elf-segment-type elf-segment-offset elf-segment-vaddr
            elf-segment-paddr elf-segment-filesz elf-segment-memsz
            elf-segment-flags elf-segment-align

            elf-program-header-len write-elf-program-header

            PT_NULL PT_LOAD PT_DYNAMIC PT_INTERP PT_NOTE PT_SHLIB
            PT_PHDR PT_TLS PT_NUM PT_LOOS PT_GNU_EH_FRAME PT_GNU_STACK
            PT_GNU_RELRO

            PF_R PF_W PF_X

            (make-elf-section* . make-elf-section)
            elf-section?
            elf-section-index
            elf-section-name elf-section-type elf-section-flags
            elf-section-addr elf-section-offset elf-section-size
            elf-section-link elf-section-info elf-section-addralign
            elf-section-entsize

            elf-section-header-len elf-section-header-addr-offset
            elf-section-header-offset-offset
            write-elf-section-header

            (make-elf-symbol* . make-elf-symbol)
            elf-symbol?
            elf-symbol-name elf-symbol-value elf-symbol-size
            elf-symbol-info elf-symbol-other elf-symbol-shndx
            elf-symbol-binding elf-symbol-type elf-symbol-visibility

            elf-symbol-len elf-symbol-value-offset write-elf-symbol

            SHN_UNDEF

            SHT_NULL SHT_PROGBITS SHT_SYMTAB SHT_STRTAB SHT_RELA
            SHT_HASH SHT_DYNAMIC SHT_NOTE SHT_NOBITS SHT_REL SHT_SHLIB
            SHT_DYNSYM SHT_INIT_ARRAY SHT_FINI_ARRAY SHT_PREINIT_ARRAY
            SHT_GROUP SHT_SYMTAB_SHNDX SHT_NUM SHT_LOOS SHT_HIOS
            SHT_LOPROC SHT_HIPROC SHT_LOUSER SHT_HIUSER

            SHF_WRITE SHF_ALLOC SHF_EXECINSTR SHF_MERGE SHF_STRINGS
            SHF_INFO_LINK SHF_LINK_ORDER SHF_OS_NONCONFORMING SHF_GROUP
            SHF_TLS

            DT_NULL DT_NEEDED DT_PLTRELSZ DT_PLTGOT DT_HASH DT_STRTAB
            DT_SYMTAB DT_RELA DT_RELASZ DT_RELAENT DT_STRSZ DT_SYMENT
            DT_INIT DT_FINI DT_SONAME DT_RPATH DT_SYMBOLIC DT_REL
            DT_RELSZ DT_RELENT DT_PLTREL DT_DEBUG DT_TEXTREL DT_JMPREL
            DT_BIND_NOW DT_INIT_ARRAY DT_FINI_ARRAY DT_INIT_ARRAYSZ
            DT_FINI_ARRAYSZ DT_RUNPATH DT_FLAGS DT_ENCODING
            DT_PREINIT_ARRAY DT_PREINIT_ARRAYSZ DT_NUM DT_LOGUILE
            DT_GUILE_GC_ROOT DT_GUILE_GC_ROOT_SZ DT_GUILE_ENTRY
            DT_GUILE_VM_VERSION DT_GUILE_FRAME_MAPS DT_HIGUILE
            DT_LOOS DT_HIOS DT_LOPROC DT_HIPROC

            string-table-ref

            STB_LOCAL STB_GLOBAL STB_WEAK STB_NUM STB_LOOS STB_GNU
            STB_HIOS STB_LOPROC STB_HIPROC

            STT_NOTYPE STT_OBJECT STT_FUNC STT_SECTION STT_FILE
            STT_COMMON STT_TLS STT_NUM STT_LOOS STT_GNU STT_HIOS
            STT_LOPROC STT_HIPROC

            STV_DEFAULT STV_INTERNAL STV_HIDDEN STV_PROTECTED

            NT_GNU_ABI_TAG NT_GNU_HWCAP NT_GNU_BUILD_ID NT_GNU_GOLD_VERSION

            parse-elf
            elf-segment elf-segments
            elf-section elf-sections elf-section-by-name elf-sections-by-name
            elf-symbol-table-len elf-symbol-table-ref

            parse-elf-note
            elf-note-name elf-note-desc elf-note-type))

;; #define EI_NIDENT 16

;; typedef struct {
;;     unsigned char e_ident[EI_NIDENT];
;;     uint16_t      e_type;
;;     uint16_t      e_machine;
;;     uint32_t      e_version;
;;     ElfN_Addr     e_entry;
;;     ElfN_Off      e_phoff;
;;     ElfN_Off      e_shoff;
;;     uint32_t      e_flags;
;;     uint16_t      e_ehsize;
;;     uint16_t      e_phentsize;
;;     uint16_t      e_phnum;
;;     uint16_t      e_shentsize;
;;     uint16_t      e_shnum;
;;     uint16_t      e_shstrndx;
;; } ElfN_Ehdr;

(define elf32-header-len 52)
(define elf64-header-len 64)
(define (elf-header-len word-size)
  (case word-size
    ((4) elf32-header-len)
    ((8) elf64-header-len)
    (else (error "invalid word size" word-size))))
(define (elf-header-shoff-offset word-size)
  (case word-size
    ((4) 32)
    ((8) 40)
    (else (error "bad word size" word-size))))

(define ELFCLASS32      1)              ; 32-bit objects
(define ELFCLASS64      2)              ; 64-bit objects

(define ELFDATA2LSB     1)              ; 2's complement, little endian
(define ELFDATA2MSB     2)              ; 2's complement, big endian

(define EV_CURRENT      1)              ; Current version

(define ELFOSABI_NONE		0)	; UNIX System V ABI */
(define ELFOSABI_HPUX		1)	; HP-UX
(define ELFOSABI_NETBSD		2)	; NetBSD.
(define ELFOSABI_GNU		3)	; Object uses GNU ELF extensions.
(define ELFOSABI_SOLARIS	6)	; Sun Solaris.
(define ELFOSABI_AIX		7)	; IBM AIX.
(define ELFOSABI_IRIX		8)	; SGI Irix.
(define ELFOSABI_FREEBSD	9)	; FreeBSD.
(define ELFOSABI_TRU64		10)	; Compaq TRU64 UNIX.
(define ELFOSABI_MODESTO	11)	; Novell Modesto.
(define ELFOSABI_OPENBSD	12)	; OpenBSD.
(define ELFOSABI_ARM_AEABI	64)	; ARM EABI
(define ELFOSABI_ARM		97)	; ARM
(define ELFOSABI_STANDALONE     255)    ; Standalone (embedded) application

(define ET_NONE		0)		; No file type
(define ET_REL		1)		; Relocatable file
(define ET_EXEC		2)		; Executable file
(define ET_DYN		3)		; Shared object file
(define ET_CORE		4)		; Core file

;;
;; Machine types
;;
;; Just a sampling of these values.  We could include more, but the
;; important thing is to recognize architectures for which we have a
;; native compiler.  Recognizing more common machine types is icing on
;; the cake.
;; 
(define EM_NONE          0)             ; No machine
(define EM_SPARC         2)             ; SUN SPARC
(define EM_386           3)             ; Intel 80386
(define EM_MIPS          8)             ; MIPS R3000 big-endian
(define EM_PPC          20)             ; PowerPC
(define EM_PPC64        21)             ; PowerPC 64-bit
(define EM_ARM          40)             ; ARM
(define EM_SH           42)             ; Hitachi SH
(define EM_SPARCV9      43)             ; SPARC v9 64-bit
(define EM_IA_64        50)             ; Intel Merced
(define EM_X86_64       62)             ; AMD x86-64 architecture

(define cpu-mapping (make-hash-table))
(for-each (lambda (pair)
            (hashq-set! cpu-mapping (car pair) (cdr pair)))
          `((none . ,EM_NONE)
            (sparc . ,EM_SPARC) ; FIXME: map 64-bit to SPARCV9 ?
            (i386 . ,EM_386)
            (mips . ,EM_MIPS)
            (ppc . ,EM_PPC)
            (ppc64 . ,EM_PPC64)
            (arm . ,EM_ARM) ; FIXME: there are more arm cpu variants
            (sh . ,EM_SH) ; FIXME: there are more sh cpu variants
            (ia64 . ,EM_IA_64)
            (x86_64 . ,EM_X86_64)))

(define SHN_UNDEF 0)

(define host-machine-type
  (hashq-ref cpu-mapping
             (string->symbol (car (string-split %host-type #\-)))
             EM_NONE))

(define host-word-size
  (sizeof '*))

(define host-byte-order
  (native-endianness))

(define (has-elf-header? bv)
  (and
   ;; e_ident
   (>= (bytevector-length bv) 16)
   (= (bytevector-u8-ref bv 0) #x7f)
   (= (bytevector-u8-ref bv 1) (char->integer #\E))
   (= (bytevector-u8-ref bv 2) (char->integer #\L))
   (= (bytevector-u8-ref bv 3) (char->integer #\F))
   (cond
    ((= (bytevector-u8-ref bv 4) ELFCLASS32)
     (>= (bytevector-length bv) elf32-header-len))
    ((= (bytevector-u8-ref bv 4) ELFCLASS64)
     (>= (bytevector-length bv) elf64-header-len))
    (else #f))
   (or (= (bytevector-u8-ref bv 5) ELFDATA2LSB)
       (= (bytevector-u8-ref bv 5) ELFDATA2MSB))
   (= (bytevector-u8-ref bv 6) EV_CURRENT)
   ;; Look at ABI later.
   (= (bytevector-u8-ref bv 8) 0)       ; ABI version
   ;; The rest of the e_ident is padding.

   ;; e_version
   (let ((byte-order (if (= (bytevector-u8-ref bv 5) ELFDATA2LSB)
                         (endianness little)
                         (endianness big))))
     (= (bytevector-u32-ref bv 20 byte-order) EV_CURRENT))))

(define-record-type <elf>
  (make-elf bytes word-size byte-order abi type machine-type
            entry phoff shoff flags ehsize
            phentsize phnum shentsize shnum shstrndx)
  elf?
  (bytes elf-bytes)
  (word-size elf-word-size)
  (byte-order elf-byte-order)
  (abi elf-abi)
  (type elf-type)
  (machine-type elf-machine-type)
  (entry elf-entry)
  (phoff elf-phoff)
  (shoff elf-shoff)
  (flags elf-flags)
  (ehsize elf-ehsize)
  (phentsize elf-phentsize)
  (phnum elf-phnum)
  (shentsize elf-shentsize)
  (shnum elf-shnum)
  (shstrndx elf-shstrndx))

(define* (make-elf* #:key (bytes #f)
                    (byte-order (target-endianness))
                    (word-size (target-word-size))
                    (abi ELFOSABI_STANDALONE)
                    (type ET_DYN)
                    (machine-type EM_NONE)
                    (entry 0)
                    (phoff (elf-header-len word-size))
                    (shoff -1)
                    (flags 0)
                    (ehsize (elf-header-len word-size))
                    (phentsize (elf-program-header-len word-size))
                    (phnum 0)
                    (shentsize (elf-section-header-len word-size))
                    (shnum 0)
                    (shstrndx SHN_UNDEF))
  (make-elf bytes word-size byte-order abi type machine-type
            entry phoff shoff flags ehsize
            phentsize phnum shentsize shnum shstrndx))

(define (parse-elf32 bv byte-order)
  (make-elf bv 4 byte-order
            (bytevector-u8-ref bv 7)
            (bytevector-u16-ref bv 16 byte-order)
            (bytevector-u16-ref bv 18 byte-order)
            (bytevector-u32-ref bv 24 byte-order)
            (bytevector-u32-ref bv 28 byte-order)
            (bytevector-u32-ref bv 32 byte-order)
            (bytevector-u32-ref bv 36 byte-order)
            (bytevector-u16-ref bv 40 byte-order)
            (bytevector-u16-ref bv 42 byte-order)
            (bytevector-u16-ref bv 44 byte-order)
            (bytevector-u16-ref bv 46 byte-order)
            (bytevector-u16-ref bv 48 byte-order)
            (bytevector-u16-ref bv 50 byte-order)))

(define (write-elf-ident bv class data abi)
  (bytevector-u8-set! bv 0 #x7f)
  (bytevector-u8-set! bv 1 (char->integer #\E))
  (bytevector-u8-set! bv 2 (char->integer #\L))
  (bytevector-u8-set! bv 3 (char->integer #\F))
  (bytevector-u8-set! bv 4 class)
  (bytevector-u8-set! bv 5 data)
  (bytevector-u8-set! bv 6 EV_CURRENT)
  (bytevector-u8-set! bv 7 abi)
  (bytevector-u8-set! bv 8 0) ; ABI version
  (bytevector-u8-set! bv 9 0) ; Pad to 16 bytes.
  (bytevector-u8-set! bv 10 0)
  (bytevector-u8-set! bv 11 0)
  (bytevector-u8-set! bv 12 0)
  (bytevector-u8-set! bv 13 0)
  (bytevector-u8-set! bv 14 0)
  (bytevector-u8-set! bv 15 0))

(define (write-elf32-header bv elf)
  (let ((byte-order (elf-byte-order elf)))
    (write-elf-ident bv ELFCLASS32
                     (case byte-order
                       ((little) ELFDATA2LSB)
                       ((big) ELFDATA2MSB)
                       (else (error "unknown endianness" byte-order)))
                     (elf-abi elf))
    (bytevector-u16-set! bv 16 (elf-type elf) byte-order)
    (bytevector-u16-set! bv 18 (elf-machine-type elf) byte-order)
    (bytevector-u32-set! bv 20 EV_CURRENT byte-order)
    (bytevector-u32-set! bv 24 (elf-entry elf) byte-order)
    (bytevector-u32-set! bv 28 (elf-phoff elf) byte-order)
    (bytevector-u32-set! bv 32 (elf-shoff elf) byte-order)
    (bytevector-u32-set! bv 36 (elf-flags elf) byte-order)
    (bytevector-u16-set! bv 40 (elf-ehsize elf) byte-order)
    (bytevector-u16-set! bv 42 (elf-phentsize elf) byte-order)
    (bytevector-u16-set! bv 44 (elf-phnum elf) byte-order)
    (bytevector-u16-set! bv 46 (elf-shentsize elf) byte-order)
    (bytevector-u16-set! bv 48 (elf-shnum elf) byte-order)
    (bytevector-u16-set! bv 50 (elf-shstrndx elf) byte-order)))

(define (parse-elf64 bv byte-order)
  (make-elf bv 8 byte-order
            (bytevector-u8-ref bv 7)
            (bytevector-u16-ref bv 16 byte-order)
            (bytevector-u16-ref bv 18 byte-order)
            (bytevector-u64-ref bv 24 byte-order)
            (bytevector-u64-ref bv 32 byte-order)
            (bytevector-u64-ref bv 40 byte-order)
            (bytevector-u32-ref bv 48 byte-order)
            (bytevector-u16-ref bv 52 byte-order)
            (bytevector-u16-ref bv 54 byte-order)
            (bytevector-u16-ref bv 56 byte-order)
            (bytevector-u16-ref bv 58 byte-order)
            (bytevector-u16-ref bv 60 byte-order)
            (bytevector-u16-ref bv 62 byte-order)))

(define (write-elf64-header bv elf)
  (let ((byte-order (elf-byte-order elf)))
    (write-elf-ident bv ELFCLASS64
                     (case byte-order
                       ((little) ELFDATA2LSB)
                       ((big) ELFDATA2MSB)
                       (else (error "unknown endianness" byte-order)))
                     (elf-abi elf))
    (bytevector-u16-set! bv 16 (elf-type elf) byte-order)
    (bytevector-u16-set! bv 18 (elf-machine-type elf) byte-order)
    (bytevector-u32-set! bv 20 EV_CURRENT byte-order)
    (bytevector-u64-set! bv 24 (elf-entry elf) byte-order)
    (bytevector-u64-set! bv 32 (elf-phoff elf) byte-order)
    (bytevector-u64-set! bv 40 (elf-shoff elf) byte-order)
    (bytevector-u32-set! bv 48 (elf-flags elf) byte-order)
    (bytevector-u16-set! bv 52 (elf-ehsize elf) byte-order)
    (bytevector-u16-set! bv 54 (elf-phentsize elf) byte-order)
    (bytevector-u16-set! bv 56 (elf-phnum elf) byte-order)
    (bytevector-u16-set! bv 58 (elf-shentsize elf) byte-order)
    (bytevector-u16-set! bv 60 (elf-shnum elf) byte-order)
    (bytevector-u16-set! bv 62 (elf-shstrndx elf) byte-order)))

(define (parse-elf bv)
  (cond
   ((has-elf-header? bv)
    (let ((class (bytevector-u8-ref bv 4))
          (byte-order (let ((data (bytevector-u8-ref bv 5)))
                        (cond
                         ((= data ELFDATA2LSB) (endianness little))
                         ((= data ELFDATA2MSB) (endianness big))
                         (else (error "unhandled byte order" data))))))
      (cond
       ((= class ELFCLASS32) (parse-elf32 bv byte-order))
       ((= class ELFCLASS64) (parse-elf64 bv byte-order))
       (else (error "unhandled class" class)))))
   (else
    (error "Invalid ELF" bv))))

(define* (write-elf-header bv elf)
  ((case (elf-word-size elf)
     ((4) write-elf32-header)
     ((8) write-elf64-header)
     (else (error "unknown word size" (elf-word-size elf))))
   bv elf))

;;
;; Segment types
;;
(define PT_NULL         0)              ; Program header table entry unused
(define PT_LOAD         1)              ; Loadable program segment
(define PT_DYNAMIC      2)              ; Dynamic linking information
(define PT_INTERP       3)              ; Program interpreter
(define PT_NOTE         4)              ; Auxiliary information
(define PT_SHLIB        5)              ; Reserved
(define PT_PHDR         6)              ; Entry for header table itself
(define PT_TLS          7)              ; Thread-local storage segment
(define PT_NUM          8)              ; Number of defined types
(define PT_LOOS         #x60000000)     ; Start of OS-specific
(define PT_GNU_EH_FRAME #x6474e550)     ; GCC .eh_frame_hdr segment
(define PT_GNU_STACK    #x6474e551)     ; Indicates stack executability
(define PT_GNU_RELRO    #x6474e552)     ; Read-only after relocation

;;
;; Segment flags
;;
(define PF_X            (ash 1 0))      ; Segment is executable
(define PF_W            (ash 1 1))      ; Segment is writable
(define PF_R            (ash 1 2))      ; Segment is readable

(define-record-type <elf-segment>
  (make-elf-segment index type offset vaddr paddr filesz memsz flags align)
  elf-segment?
  (index elf-segment-index)
  (type elf-segment-type)
  (offset elf-segment-offset)
  (vaddr elf-segment-vaddr)
  (paddr elf-segment-paddr)
  (filesz elf-segment-filesz)
  (memsz elf-segment-memsz)
  (flags elf-segment-flags)
  (align elf-segment-align))

(define* (make-elf-segment* #:key (index -1) (type PT_LOAD) (offset 0) (vaddr 0)
                            (paddr 0) (filesz 0) (memsz filesz)
                            (flags (logior PF_W PF_R))
                            (align 8))
  (make-elf-segment index type offset vaddr paddr filesz memsz flags align))

;; typedef struct {
;;     uint32_t   p_type;
;;     Elf32_Off  p_offset;
;;     Elf32_Addr p_vaddr;
;;     Elf32_Addr p_paddr;
;;     uint32_t   p_filesz;
;;     uint32_t   p_memsz;
;;     uint32_t   p_flags;
;;     uint32_t   p_align;
;; } Elf32_Phdr;

(define (parse-elf32-program-header index bv offset byte-order)
  (if (<= (+ offset 32) (bytevector-length bv))
      (make-elf-segment index
                        (bytevector-u32-ref bv offset byte-order)
                        (bytevector-u32-ref bv (+ offset 4) byte-order)
                        (bytevector-u32-ref bv (+ offset 8) byte-order)
                        (bytevector-u32-ref bv (+ offset 12) byte-order)
                        (bytevector-u32-ref bv (+ offset 16) byte-order)
                        (bytevector-u32-ref bv (+ offset 20) byte-order)
                        (bytevector-u32-ref bv (+ offset 24) byte-order)
                        (bytevector-u32-ref bv (+ offset 28) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf32-program-header bv offset byte-order seg)
  (bytevector-u32-set! bv offset (elf-segment-type seg) byte-order)
  (bytevector-u32-set! bv (+ offset 4) (elf-segment-offset seg) byte-order)
  (bytevector-u32-set! bv (+ offset 8) (elf-segment-vaddr seg) byte-order)
  (bytevector-u32-set! bv (+ offset 12) (elf-segment-paddr seg) byte-order)
  (bytevector-u32-set! bv (+ offset 16) (elf-segment-filesz seg) byte-order)
  (bytevector-u32-set! bv (+ offset 20) (elf-segment-memsz seg) byte-order)
  (bytevector-u32-set! bv (+ offset 24) (elf-segment-flags seg) byte-order)
  (bytevector-u32-set! bv (+ offset 28) (elf-segment-align seg) byte-order))


;; typedef struct {
;;     uint32_t   p_type;
;;     uint32_t   p_flags;
;;     Elf64_Off  p_offset;
;;     Elf64_Addr p_vaddr;
;;     Elf64_Addr p_paddr;
;;     uint64_t   p_filesz;
;;     uint64_t   p_memsz;
;;     uint64_t   p_align;
;; } Elf64_Phdr;

;; NB: position of `flags' is different!

(define (parse-elf64-program-header index bv offset byte-order)
  (if (<= (+ offset 56) (bytevector-length bv))
      (make-elf-segment index
                        (bytevector-u32-ref bv offset byte-order)
                        (bytevector-u64-ref bv (+ offset 8) byte-order)
                        (bytevector-u64-ref bv (+ offset 16) byte-order)
                        (bytevector-u64-ref bv (+ offset 24) byte-order)
                        (bytevector-u64-ref bv (+ offset 32) byte-order)
                        (bytevector-u64-ref bv (+ offset 40) byte-order)
                        (bytevector-u32-ref bv (+ offset 4) byte-order)
                        (bytevector-u64-ref bv (+ offset 48) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf64-program-header bv offset byte-order seg)
  (bytevector-u32-set! bv offset (elf-segment-type seg) byte-order)
  (bytevector-u64-set! bv (+ offset 8) (elf-segment-offset seg) byte-order)
  (bytevector-u64-set! bv (+ offset 16) (elf-segment-vaddr seg) byte-order)
  (bytevector-u64-set! bv (+ offset 24) (elf-segment-paddr seg) byte-order)
  (bytevector-u64-set! bv (+ offset 32) (elf-segment-filesz seg) byte-order)
  (bytevector-u64-set! bv (+ offset 40) (elf-segment-memsz seg) byte-order)
  (bytevector-u32-set! bv (+ offset 4) (elf-segment-flags seg) byte-order)
  (bytevector-u64-set! bv (+ offset 48) (elf-segment-align seg) byte-order))

(define (write-elf-program-header bv offset byte-order word-size seg)
  ((case word-size
     ((4) write-elf32-program-header)
     ((8) write-elf64-program-header)
     (else (error "invalid word size" word-size)))
   bv offset byte-order seg))

(define (elf-program-header-len word-size)
  (case word-size
    ((4) 32)
    ((8) 56)
    (else (error "bad word size" word-size))))

(define (elf-segment elf n)
  (if (not (< -1 n (elf-phnum elf)))
      (error "bad segment number" n))
  ((case (elf-word-size elf)
     ((4) parse-elf32-program-header)
     ((8) parse-elf64-program-header)
     (else (error "unhandled pointer size")))
   n
   (elf-bytes elf)
   (+ (elf-phoff elf) (* n (elf-phentsize elf)))
   (elf-byte-order elf)))

(define (elf-segments elf)
  (let lp ((n (elf-phnum elf)) (out '()))
    (if (zero? n)
        out
        (lp (1- n) (cons (elf-segment elf (1- n)) out)))))

(define-record-type <elf-section>
  (make-elf-section index name type flags
                    addr offset size link info addralign entsize)
  elf-section?
  (index elf-section-index)
  (name elf-section-name)
  (type elf-section-type)
  (flags elf-section-flags)
  (addr elf-section-addr)
  (offset elf-section-offset)
  (size elf-section-size)
  (link elf-section-link)
  (info elf-section-info)
  (addralign elf-section-addralign)
  (entsize elf-section-entsize))

(define* (make-elf-section* #:key (index SHN_UNDEF) (name 0) (type SHT_PROGBITS)
                            (flags SHF_ALLOC) (addr 0) (offset 0) (size 0)
                            (link 0) (info 0) (addralign 8) (entsize 0))
  (make-elf-section index name type flags addr offset size link info addralign
                    entsize))

;; typedef struct {
;;     uint32_t   sh_name;
;;     uint32_t   sh_type;
;;     uint32_t   sh_flags;
;;     Elf32_Addr sh_addr;
;;     Elf32_Off  sh_offset;
;;     uint32_t   sh_size;
;;     uint32_t   sh_link;
;;     uint32_t   sh_info;
;;     uint32_t   sh_addralign;
;;     uint32_t   sh_entsize;
;; } Elf32_Shdr;

(define (parse-elf32-section-header index bv offset byte-order)
  (if (<= (+ offset 40) (bytevector-length bv))
      (make-elf-section index
                        (bytevector-u32-ref bv offset byte-order)
                        (bytevector-u32-ref bv (+ offset 4) byte-order)
                        (bytevector-u32-ref bv (+ offset 8) byte-order)
                        (bytevector-u32-ref bv (+ offset 12) byte-order)
                        (bytevector-u32-ref bv (+ offset 16) byte-order)
                        (bytevector-u32-ref bv (+ offset 20) byte-order)
                        (bytevector-u32-ref bv (+ offset 24) byte-order)
                        (bytevector-u32-ref bv (+ offset 28) byte-order)
                        (bytevector-u32-ref bv (+ offset 32) byte-order)
                        (bytevector-u32-ref bv (+ offset 36) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf32-section-header bv offset byte-order sec)
  (bytevector-u32-set! bv offset (elf-section-name sec) byte-order)
  (bytevector-u32-set! bv (+ offset 4) (elf-section-type sec) byte-order)
  (bytevector-u32-set! bv (+ offset 8) (elf-section-flags sec) byte-order)
  (bytevector-u32-set! bv (+ offset 12) (elf-section-addr sec) byte-order)
  (bytevector-u32-set! bv (+ offset 16) (elf-section-offset sec) byte-order)
  (bytevector-u32-set! bv (+ offset 20) (elf-section-size sec) byte-order)
  (bytevector-u32-set! bv (+ offset 24) (elf-section-link sec) byte-order)
  (bytevector-u32-set! bv (+ offset 28) (elf-section-info sec) byte-order)
  (bytevector-u32-set! bv (+ offset 32) (elf-section-addralign sec) byte-order)
  (bytevector-u32-set! bv (+ offset 36) (elf-section-entsize sec) byte-order))


;; typedef struct {
;;     uint32_t   sh_name;
;;     uint32_t   sh_type;
;;     uint64_t   sh_flags;
;;     Elf64_Addr sh_addr;
;;     Elf64_Off  sh_offset;
;;     uint64_t   sh_size;
;;     uint32_t   sh_link;
;;     uint32_t   sh_info;
;;     uint64_t   sh_addralign;
;;     uint64_t   sh_entsize;
;; } Elf64_Shdr;

(define (elf-section-header-len word-size)
  (case word-size
    ((4) 40)
    ((8) 64)
    (else (error "bad word size" word-size))))

(define (elf-section-header-addr-offset word-size)
  (case word-size
    ((4) 12)
    ((8) 16)
    (else (error "bad word size" word-size))))

(define (elf-section-header-offset-offset word-size)
  (case word-size
    ((4) 16)
    ((8) 24)
    (else (error "bad word size" word-size))))

(define (parse-elf64-section-header index bv offset byte-order)
  (if (<= (+ offset 64) (bytevector-length bv))
      (make-elf-section index
                        (bytevector-u32-ref bv offset byte-order)
                        (bytevector-u32-ref bv (+ offset 4) byte-order)
                        (bytevector-u64-ref bv (+ offset 8) byte-order)
                        (bytevector-u64-ref bv (+ offset 16) byte-order)
                        (bytevector-u64-ref bv (+ offset 24) byte-order)
                        (bytevector-u64-ref bv (+ offset 32) byte-order)
                        (bytevector-u32-ref bv (+ offset 40) byte-order)
                        (bytevector-u32-ref bv (+ offset 44) byte-order)
                        (bytevector-u64-ref bv (+ offset 48) byte-order)
                        (bytevector-u64-ref bv (+ offset 56) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf64-section-header bv offset byte-order sec)
  (bytevector-u32-set! bv offset (elf-section-name sec) byte-order)
  (bytevector-u32-set! bv (+ offset 4) (elf-section-type sec) byte-order)
  (bytevector-u64-set! bv (+ offset 8) (elf-section-flags sec) byte-order)
  (bytevector-u64-set! bv (+ offset 16) (elf-section-addr sec) byte-order)
  (bytevector-u64-set! bv (+ offset 24) (elf-section-offset sec) byte-order)
  (bytevector-u64-set! bv (+ offset 32) (elf-section-size sec) byte-order)
  (bytevector-u32-set! bv (+ offset 40) (elf-section-link sec) byte-order)
  (bytevector-u32-set! bv (+ offset 44) (elf-section-info sec) byte-order)
  (bytevector-u64-set! bv (+ offset 48) (elf-section-addralign sec) byte-order)
  (bytevector-u64-set! bv (+ offset 56) (elf-section-entsize sec) byte-order))

(define (elf-section elf n)
  (if (not (< -1 n (elf-shnum elf)))
      (error "bad section number" n))
  ((case (elf-word-size elf)
     ((4) parse-elf32-section-header)
     ((8) parse-elf64-section-header)
     (else (error "unhandled pointer size")))
   n
   (elf-bytes elf)
   (+ (elf-shoff elf) (* n (elf-shentsize elf)))
   (elf-byte-order elf)))

(define (write-elf-section-header bv offset byte-order word-size sec)
  ((case word-size
     ((4) write-elf32-section-header)
     ((8) write-elf64-section-header)
     (else (error "invalid word size" word-size)))
   bv offset byte-order sec))

(define (elf-sections elf)
  (let lp ((n (elf-shnum elf)) (out '()))
    (if (zero? n)
        out
        (lp (1- n) (cons (elf-section elf (1- n)) out)))))

;;
;; Section Types
;;
(define SHT_NULL          0)            ; Section header table entry unused
(define SHT_PROGBITS      1)            ; Program data
(define SHT_SYMTAB        2)            ; Symbol table
(define SHT_STRTAB        3)            ; String table
(define SHT_RELA          4)            ; Relocation entries with addends
(define SHT_HASH          5)            ; Symbol hash table
(define SHT_DYNAMIC       6)            ; Dynamic linking information
(define SHT_NOTE          7)            ; Notes
(define SHT_NOBITS        8)            ; Program space with no data (bss)
(define SHT_REL           9)            ; Relocation entries, no addends
(define SHT_SHLIB         10)           ; Reserved
(define SHT_DYNSYM        11)           ; Dynamic linker symbol table
(define SHT_INIT_ARRAY    14)           ; Array of constructors
(define SHT_FINI_ARRAY    15)           ; Array of destructors
(define SHT_PREINIT_ARRAY 16)           ; Array of pre-constructors
(define SHT_GROUP         17)           ; Section group
(define SHT_SYMTAB_SHNDX  18)           ; Extended section indeces
(define SHT_NUM           19)           ; Number of defined types. 
(define SHT_LOOS          #x60000000)   ; Start OS-specific. 
(define SHT_HIOS          #x6fffffff)   ; End OS-specific type
(define SHT_LOPROC        #x70000000)   ; Start of processor-specific
(define SHT_HIPROC        #x7fffffff)   ; End of processor-specific
(define SHT_LOUSER        #x80000000)   ; Start of application-specific
(define SHT_HIUSER        #x8fffffff)   ; End of application-specific

;;
;; Section Flags
;;
(define SHF_WRITE            (ash 1 0)) ; Writable
(define SHF_ALLOC            (ash 1 1)) ; Occupies memory during execution
(define SHF_EXECINSTR        (ash 1 2)) ; Executable
(define SHF_MERGE            (ash 1 4)) ; Might be merged
(define SHF_STRINGS          (ash 1 5)) ; Contains nul-terminated strings
(define SHF_INFO_LINK        (ash 1 6)) ; `sh_info' contains SHT index
(define SHF_LINK_ORDER       (ash 1 7)) ; Preserve order after combining
(define SHF_OS_NONCONFORMING (ash 1 8)) ; Non-standard OS specific handling required
(define SHF_GROUP            (ash 1 9)) ; Section is member of a group. 
(define SHF_TLS              (ash 1 10)) ; Section hold thread-local data. 

;;
;; Dynamic entry types.  The DT_GUILE types are non-standard.
;;
(define DT_NULL		0)		; Marks end of dynamic section
(define DT_NEEDED	1)		; Name of needed library
(define DT_PLTRELSZ	2)		; Size in bytes of PLT relocs
(define DT_PLTGOT	3)		; Processor defined value
(define DT_HASH		4)		; Address of symbol hash table
(define DT_STRTAB	5)		; Address of string table
(define DT_SYMTAB	6)		; Address of symbol table
(define DT_RELA		7)		; Address of Rela relocs
(define DT_RELASZ	8)		; Total size of Rela relocs
(define DT_RELAENT	9)		; Size of one Rela reloc
(define DT_STRSZ	10)		; Size of string table
(define DT_SYMENT	11)		; Size of one symbol table entry
(define DT_INIT		12)		; Address of init function
(define DT_FINI		13)		; Address of termination function
(define DT_SONAME	14)		; Name of shared object
(define DT_RPATH	15)		; Library search path (deprecated)
(define DT_SYMBOLIC	16)		; Start symbol search here
(define DT_REL		17)		; Address of Rel relocs
(define DT_RELSZ	18)		; Total size of Rel relocs
(define DT_RELENT	19)		; Size of one Rel reloc
(define DT_PLTREL	20)		; Type of reloc in PLT
(define DT_DEBUG	21)		; For debugging ; unspecified
(define DT_TEXTREL	22)		; Reloc might modify .text
(define DT_JMPREL	23)		; Address of PLT relocs
(define	DT_BIND_NOW	24)		; Process relocations of object
(define	DT_INIT_ARRAY	25)		; Array with addresses of init fct
(define	DT_FINI_ARRAY	26)		; Array with addresses of fini fct
(define	DT_INIT_ARRAYSZ	27)		; Size in bytes of DT_INIT_ARRAY
(define	DT_FINI_ARRAYSZ	28)		; Size in bytes of DT_FINI_ARRAY
(define DT_RUNPATH	29)		; Library search path
(define DT_FLAGS	30)		; Flags for the object being loaded
(define DT_ENCODING	32)		; Start of encoded range
(define DT_PREINIT_ARRAY 32)		; Array with addresses of preinit fc
(define DT_PREINIT_ARRAYSZ 33)		; size in bytes of DT_PREINIT_ARRAY
(define	DT_NUM		34)		; Number used
(define DT_LOGUILE      #x37146000)     ; Start of Guile-specific
(define DT_GUILE_GC_ROOT    #x37146000) ; Offset of GC roots
(define DT_GUILE_GC_ROOT_SZ #x37146001) ; Size in machine words of GC roots
(define DT_GUILE_ENTRY      #x37146002) ; Address of entry thunk
(define DT_GUILE_VM_VERSION #x37146003) ; Bytecode version
(define DT_GUILE_FRAME_MAPS #x37146004) ; Offset of .guile.frame-maps
(define DT_HIGUILE      #x37146fff)     ; End of Guile-specific
(define DT_LOOS		#x6000000d)	; Start of OS-specific
(define DT_HIOS		#x6ffff000)	; End of OS-specific
(define DT_LOPROC	#x70000000)	; Start of processor-specific
(define DT_HIPROC	#x7fffffff)	; End of processor-specific


(define (string-table-ref bv offset)
  (let lp ((end offset))
    (if (zero? (bytevector-u8-ref bv end))
        (let ((out (make-bytevector (- end offset))))
          (bytevector-copy! bv offset out 0 (- end offset))
          (utf8->string out))
        (lp (1+ end)))))

(define (elf-section-by-name elf name)
  (let ((off (elf-section-offset (elf-section elf (elf-shstrndx elf)))))
    (let lp ((n (elf-shnum elf)))
      (and (> n 0)
           (let ((section (elf-section elf (1- n))))
             (if (equal? (string-table-ref (elf-bytes elf)
                                           (+ off (elf-section-name section)))
                         name)
                 section
                 (lp (1- n))))))))

(define (elf-sections-by-name elf)
  (let* ((sections (elf-sections elf))
         (off (elf-section-offset (list-ref sections (elf-shstrndx elf)))))
    (map (lambda (section)
           (cons (string-table-ref (elf-bytes elf)
                                   (+ off (elf-section-name section)))
                 section))
         sections)))

(define-record-type <elf-symbol>
  (make-elf-symbol name value size info other shndx)
  elf-symbol?
  (name elf-symbol-name)
  (value elf-symbol-value)
  (size elf-symbol-size)
  (info elf-symbol-info)
  (other elf-symbol-other)
  (shndx elf-symbol-shndx))

(define* (make-elf-symbol* #:key (name 0) (value 0) (size 0)
                           (binding STB_LOCAL) (type STT_NOTYPE)
                           (info (logior (ash binding 4) type))
                           (visibility STV_DEFAULT) (other visibility)
                           (shndx SHN_UNDEF))
  (make-elf-symbol name value size info other shndx))

;; typedef struct {
;;     uint32_t      st_name;
;;     Elf32_Addr    st_value;
;;     uint32_t      st_size;
;;     unsigned char st_info;
;;     unsigned char st_other;
;;     uint16_t      st_shndx;
;; } Elf32_Sym;

(define (elf-symbol-len word-size)
  (case word-size
    ((4) 16)
    ((8) 24)
    (else (error "bad word size" word-size))))

(define (elf-symbol-value-offset word-size)
  (case word-size
    ((4) 4)
    ((8) 8)
    (else (error "bad word size" word-size))))

(define (parse-elf32-symbol bv offset stroff byte-order)
  (if (<= (+ offset 16) (bytevector-length bv))
      (make-elf-symbol (let ((name (bytevector-u32-ref bv offset byte-order)))
                         (if stroff
                             (string-table-ref bv (+ stroff name))
                             name))
                       (bytevector-u32-ref bv (+ offset 4) byte-order)
                       (bytevector-u32-ref bv (+ offset 8) byte-order)
                       (bytevector-u8-ref bv (+ offset 12))
                       (bytevector-u8-ref bv (+ offset 13))
                       (bytevector-u16-ref bv (+ offset 14) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf32-symbol bv offset byte-order sym)
  (bytevector-u32-set! bv offset (elf-symbol-name sym) byte-order)
  (bytevector-u32-set! bv (+ offset 4) (elf-symbol-value sym) byte-order)
  (bytevector-u32-set! bv (+ offset 8) (elf-symbol-size sym) byte-order)
  (bytevector-u8-set! bv (+ offset 12) (elf-symbol-info sym))
  (bytevector-u8-set! bv (+ offset 13) (elf-symbol-other sym))
  (bytevector-u16-set! bv (+ offset 14) (elf-symbol-shndx sym) byte-order))

;; typedef struct {
;;     uint32_t      st_name;
;;     unsigned char st_info;
;;     unsigned char st_other;
;;     uint16_t      st_shndx;
;;     Elf64_Addr    st_value;
;;     uint64_t      st_size;
;; } Elf64_Sym;

(define (parse-elf64-symbol bv offset stroff byte-order)
  (if (<= (+ offset 24) (bytevector-length bv))
      (make-elf-symbol (let ((name (bytevector-u32-ref bv offset byte-order)))
                         (if stroff
                             (string-table-ref bv (+ stroff name))
                             name))
                       (bytevector-u64-ref bv (+ offset 8) byte-order)
                       (bytevector-u64-ref bv (+ offset 16) byte-order)
                       (bytevector-u8-ref bv (+ offset 4))
                       (bytevector-u8-ref bv (+ offset 5))
                       (bytevector-u16-ref bv (+ offset 6) byte-order))
      (error "corrupt ELF (offset out of range)" offset)))

(define (write-elf64-symbol bv offset byte-order sym)
  (bytevector-u32-set! bv offset (elf-symbol-name sym) byte-order)
  (bytevector-u8-set! bv (+ offset 4) (elf-symbol-info sym))
  (bytevector-u8-set! bv (+ offset 5) (elf-symbol-other sym))
  (bytevector-u16-set! bv (+ offset 6) (elf-symbol-shndx sym) byte-order)
  (bytevector-u64-set! bv (+ offset 8) (elf-symbol-value sym) byte-order)
  (bytevector-u64-set! bv (+ offset 16) (elf-symbol-size sym) byte-order))

(define (write-elf-symbol bv offset byte-order word-size sym)
  ((case word-size
     ((4) write-elf32-symbol)
     ((8) write-elf64-symbol)
     (else (error "invalid word size" word-size)))
   bv offset byte-order sym))

(define (elf-symbol-table-len section)
  (let ((len (elf-section-size section))
        (entsize (elf-section-entsize section)))
    (unless (and (not (zero? entsize)) (zero? (modulo len entsize)))
      (error "bad symbol table" section))
    (/ len entsize)))

(define* (elf-symbol-table-ref elf section n #:optional strtab)
  (let ((bv (elf-bytes elf))
        (byte-order (elf-byte-order elf))
        (stroff (and strtab (elf-section-offset strtab)))
        (base (elf-section-offset section))
        (len (elf-section-size section))
        (entsize (elf-section-entsize section)))
    (unless (<= (* (1+ n) entsize) len)
      (error "out of range symbol table access" section n))
    (case (elf-word-size elf)
      ((4)
       (unless (<= 16 entsize)
         (error "bad entsize for symbol table" section))
       (parse-elf32-symbol bv (+ base (* n entsize)) stroff byte-order))
      ((8)
       (unless (<= 24 entsize)
         (error "bad entsize for symbol table" section))
       (parse-elf64-symbol bv (+ base (* n entsize)) stroff byte-order))
      (else (error "bad word size" elf)))))

;; Legal values for ST_BIND subfield of st_info (symbol binding).

(define STB_LOCAL	0)		; Local symbol
(define STB_GLOBAL	1)		; Global symbol
(define STB_WEAK	2)		; Weak symbol
(define STB_NUM		3)		; Number of defined types. 
(define STB_LOOS	10)		; Start of OS-specific
(define STB_GNU_UNIQUE	10)		; Unique symbol. 
(define STB_HIOS	12)		; End of OS-specific
(define STB_LOPROC	13)		; Start of processor-specific
(define STB_HIPROC	15)		; End of processor-specific

;; Legal values for ST_TYPE subfield of st_info (symbol type).

(define STT_NOTYPE	0)		; Symbol type is unspecified
(define STT_OBJECT	1)		; Symbol is a data object
(define STT_FUNC	2)		; Symbol is a code object
(define STT_SECTION	3)		; Symbol associated with a section
(define STT_FILE	4)		; Symbol's name is file name
(define STT_COMMON	5)		; Symbol is a common data object
(define STT_TLS		6)		; Symbol is thread-local data objec
(define STT_NUM		7)		; Number of defined types. 
(define STT_LOOS	10)		; Start of OS-specific
(define STT_GNU_IFUNC	10)		; Symbol is indirect code object
(define STT_HIOS	12)		; End of OS-specific
(define STT_LOPROC	13)		; Start of processor-specific
(define STT_HIPROC	15)		; End of processor-specific

;; Symbol visibility specification encoded in the st_other field.

(define STV_DEFAULT	0)		; Default symbol visibility rules
(define STV_INTERNAL	1)		; Processor specific hidden class
(define STV_HIDDEN	2)		; Sym unavailable in other modules
(define STV_PROTECTED	3)		; Not preemptible, not exported

(define (elf-symbol-binding sym)
  (ash (elf-symbol-info sym) -4))

(define (elf-symbol-type sym)
  (logand (elf-symbol-info sym) #xf))

(define (elf-symbol-visibility sym)
  (logand (elf-symbol-other sym) #x3))

(define NT_GNU_ABI_TAG 1)
(define NT_GNU_HWCAP 2)
(define NT_GNU_BUILD_ID 3)
(define NT_GNU_GOLD_VERSION 4)

(define-record-type <elf-note>
  (make-elf-note name desc type)
  elf-note?
  (name elf-note-name)
  (desc elf-note-desc)
  (type elf-note-type))

(define (parse-elf-note elf section)
  (let ((bv (elf-bytes elf))
        (byte-order (elf-byte-order elf))
        (offset (elf-section-offset section)))
    (unless (<= (+ offset 12) (bytevector-length bv))
      (error "corrupt ELF (offset out of range)" offset))
    (let ((namesz (bytevector-u32-ref bv offset byte-order))
          (descsz (bytevector-u32-ref bv (+ offset 4) byte-order))
          (type (bytevector-u32-ref bv (+ offset 8) byte-order)))
      (unless (<= (+ offset 12 namesz descsz) (bytevector-length bv))
        (error "corrupt ELF (offset out of range)" offset))
      (let ((name (make-bytevector (1- namesz)))
            (desc (make-bytevector descsz)))
        (bytevector-copy! bv (+ offset 12) name 0 (1- namesz))
        (bytevector-copy! bv (+ offset 12 namesz) desc 0 descsz)
        (make-elf-note (utf8->string name) desc type)))))
