      ***===========================================================***
      *** NOME INC                                     LENGTH=00070 ***
      *** I#SLIGC2                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***                                                           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGC2-CGC-CPF-CLI            PIC 9(009)    COMP-3.
           05 SLIGC2-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIGC2-BANCO-SACDO            PIC 9(003)    COMP-3.
           05 SLIGC2-AGENCIA-SACDO          PIC 9(005)    COMP-3.
           05 SLIGC2-CCONTA-SACDO           PIC 9(013)    COMP-3.
           05 SLIGC2-PCOCEN-MAIOR           PIC 9(003)V99 COMP-3.
           05 SLIGC2-PCOCEN-SIMPLE          PIC 9(003)V99 COMP-3.
           05 SLIGC2-PCOCEN-CAUCN           PIC 9(003)V99 COMP-3.
           05 SLIGC2-PCOCEN-DESC            PIC 9(003)V99 COMP-3.
           05 SLIGC2-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           05 SLIGC2-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           05 SLIGC2-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           05 SLIGC2-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
           05 SLIGC2-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIGC2-FILLER                 PIC X(08).
