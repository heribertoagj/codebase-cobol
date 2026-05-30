      ***===========================================================***
      *** NOME INC                                     LENGTH=00075 ***
      *** I#SLIGCB                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***                                                           ***
      ***           (COPIA DA I#SLIGC2                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGCB-CGC-CPF-CLI            PIC 9(009)    COMP-3.
           05 SLIGCB-CFLIAL-CLI             PIC 9(005)    COMP-3.
           05 SLIGCB-CCTRL-CLI              PIC 9(002)    COMP-3.
           05 SLIGCB-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIGCB-BANCO-SACDO            PIC 9(003)    COMP-3.
           05 SLIGCB-AGENCIA-SACDO          PIC 9(005)    COMP-3.
           05 SLIGCB-CCONTA-SACDO           PIC 9(013)    COMP-3.
           05 SLIGCB-PCOCEN-MAIOR           PIC 9(003)V99 COMP-3.
           05 SLIGCB-PCOCEN-SIMPLE          PIC 9(003)V99 COMP-3.
           05 SLIGCB-PCOCEN-CAUCN           PIC 9(003)V99 COMP-3.
           05 SLIGCB-PCOCEN-DESC            PIC 9(003)V99 COMP-3.
           05 SLIGCB-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           05 SLIGCB-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           05 SLIGCB-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           05 SLIGCB-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
           05 SLIGCB-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIGCB-CFLIAL-CLI-ST          PIC X(005).
           05 SLIGCB-CCTRL-CLI-ST           PIC 9(002)    COMP-3.
           05 SLIGCB-FILLER                 PIC X(17).
