      ***===========================================================***
      *** NOME INC                                     LENGTH=00065 ***
      *** I#SLIGC3                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO PARA CARGA DB2           ***
      ***                                                           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGC3-CGC-CPF-CLI             PIC 9(009)    COMP-3.
           05 SLIGC3-TIPO-PESSOA-CLI         PIC X(001).
           05 SLIGC3-CSEQ-CLASF-COCEN        PIC 9(015)    COMP-3.
           05 SLIGC3-BANCO-SACDO             PIC 9(003)    COMP-3.
           05 SLIGC3-AGENCIA-SACDO           PIC 9(005)    COMP-3.
           05 SLIGC3-CCONTA-SACDO            PIC 9(013)    COMP-3.
           05 SLIGC3-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIGC3-PCOCEN-SIMPLE-NULL      PIC X(001).
           05 SLIGC3-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIGC3-PCOCEN-CAUCN-NULL       PIC X(001).
           05 SLIGC3-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIGC3-PCOCEN-DESC-NULL        PIC X(001).
           05 SLIGC3-VL-ABERTO-SAC-SIMP      PIC 9(013)V99 COMP-3.
           05 SLIGC3-VL-ABERTO-SAC-SIMP-NULL PIC X(001).
           05 SLIGC3-VL-ABERTO-SAC-CAUC      PIC 9(013)V99 COMP-3.
           05 SLIGC3-VL-ABERTO-SAC-CAUC-NULL PIC X(001).
           05 SLIGC3-VL-ABERTO-SAC-DESC      PIC 9(013)V99 COMP-3.
           05 SLIGC3-VL-ABERTO-SAC-DESC-NULL PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIGC3-CGC-CPF-CLI-ST          PIC X(009).
