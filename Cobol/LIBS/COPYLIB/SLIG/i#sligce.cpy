      ***===========================================================***
      *** NOME INC                                     LENGTH=00052 ***
      *** I#SLIGCE                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO PARTICIPACAO BANDA       ***
      ***                    AGENCIA                                ***
      ***                                                           ***
      ***           (COPIA DA I#SLIGC5                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       01  REG-INTERMEDIARIO-PAGBC0.
           05 SLIGCE-BANCO-CLI              PIC S9(005)    COMP-3.
           05 SLIGCE-AGENCIA-CLI            PIC S9(005)    COMP-3.
           05 SLIGCE-SEQ-CLAS               PIC S9(015)    COMP-3.
           05 SLIGCE-CGC-CPF-EMITT          PIC S9(009)    COMP-3.
           05 SLIGCE-CFLIAL-EMITT           PIC S9(005)    COMP-3.
           05 SLIGCE-CCTRL-EMITT            PIC S9(002)    COMP-3.
           05 SLIGCE-CIDTFD-TPO-PSSOA       PIC  X(001).
           05 SLIGCE-CIDTFD-TPO-PSSOA-NULL  PIC  X(001).
           05 SLIGCE-BANCO-EMITT            PIC S9(003)    COMP-3.
           05 SLIGCE-BANCO-EMITT-NULL       PIC  X(001).
           05 SLIGCE-AGENCIA-EMITT          PIC S9(005)    COMP-3.
           05 SLIGCE-AGENCIA-EMITT-NULL     PIC  X(001).
           05 SLIGCE-CCONTA-EMITT           PIC S9(013)    COMP-3.
           05 SLIGCE-PPARTC-SIMPLE          PIC S9(003)V99 COMP-3.
           05 SLIGCE-PPARTC-SIMPLE-NULL     PIC  X(001).
           05 SLIGCE-PPARTC-CAUCN           PIC S9(003)V99 COMP-3.
           05 SLIGCE-PPARTC-CAUCN-NULL      PIC  X(001).
           05 SLIGCE-PPARTC-DESC            PIC S9(003)V99 COMP-3.
           05 SLIGCE-PPARTC-DESC-NULL       PIC  X(001).
CNPJ7C     05 SLIGCE-CGC-CPF-EMITT-N        PIC  X(009).
CNPJ7C     05 SLIGCE-CFLIAL-EMITT-N         PIC  X(005).
CNPJ7C     05 SLIGCE-CCTRL-EMITT-N          PIC S9(002)    COMP-3.

