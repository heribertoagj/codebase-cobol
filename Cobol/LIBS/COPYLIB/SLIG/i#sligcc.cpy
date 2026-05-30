      ***===========================================================***
      *** NOME INC                                     LENGTH=00070 ***
      *** I#SLIGCC                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO PARA CARGA DA TABELA DB2               ***
      ***            CONCENTRACAO CLIENTE BANDA                     ***
      ***                                                           ***
      ***           (COPIA DA I#SLIGC3                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***                                                           ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGCC-CGC-CPF-CLI             PIC 9(009)    COMP-3.
           05 SLIGCC-CFLIAL-CLI              PIC 9(005)    COMP-3.
           05 SLIGCC-CSEQ-CLASF-COCEN        PIC 9(015)    COMP-3.
           05 SLIGCC-CCTRL-CLI               PIC 9(002)    COMP-3.
           05 SLIGCC-TIPO-PESSOA-CLI         PIC X(001).
           05 SLIGCC-BANCO-SACDO             PIC 9(003)    COMP-3.
           05 SLIGCC-AGENCIA-SACDO           PIC 9(005)    COMP-3.
           05 SLIGCC-CCONTA-SACDO            PIC 9(013)    COMP-3.
           05 SLIGCC-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIGCC-PCOCEN-SIMPLE-NULL      PIC X(001).
           05 SLIGCC-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIGCC-PCOCEN-CAUCN-NULL       PIC X(001).
           05 SLIGCC-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIGCC-PCOCEN-DESC-NULL        PIC X(001).
           05 SLIGCC-VL-ABERTO-SAC-SIMP      PIC 9(013)V99 COMP-3.
           05 SLIGCC-VL-ABERTO-SAC-SIMP-NULL PIC X(001).
           05 SLIGCC-VL-ABERTO-SAC-CAUC      PIC 9(013)V99 COMP-3.
           05 SLIGCC-VL-ABERTO-SAC-CAUC-NULL PIC X(001).
           05 SLIGCC-VL-ABERTO-SAC-DESC      PIC 9(013)V99 COMP-3.
           05 SLIGCC-VL-ABERTO-SAC-DESC-NULL PIC X(001).
CNPJ7C     05 SLIGCC-CGC-CPF-CLI-N           PIC X(009).
CNPJ7C     05 SLIGCC-CFLIAL-CLI-N            PIC X(005).
CNPJ7C     05 SLIGCC-CCTRL-CLI-N             PIC 9(002)    COMP-3.

