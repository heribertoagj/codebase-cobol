      ***===========================================================***
      *** NOME INC                                     LENGTH=00048 ***
      *** I#SLIGC5                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO PARTICIPACAO BANDA       ***
      ***                    AGENCIA                                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMEDIARIO-PAGBC0.
           05 SLIGC5-BANCO-CLI              PIC 9(005)    COMP-3.
           05 SLIGC5-AGENCIA-CLI            PIC 9(005)    COMP-3.
           05 SLIGC5-SEQ-CLAS               PIC 9(015)    COMP-3.
           05 SLIGC5-CGC-CPF-EMITT          PIC 9(009)    COMP-3.
           05 SLIGC5-CGC-CPF-EMITT-NULL     PIC X(001).
           05 SLIGC5-CIDTFD-TPO-PSSOA       PIC X(001).
           05 SLIGC5-CIDTFD-TPO-PSSOA-NULL  PIC X(001).
           05 SLIGC5-BANCO-EMITT            PIC 9(003)    COMP-3.
           05 SLIGC5-BANCO-EMITT-NULL       PIC X(001).
           05 SLIGC5-AGENCIA-EMITT          PIC 9(005)    COMP-3.
           05 SLIGC5-AGENCIA-EMITT-NULL     PIC X(001).
           05 SLIGC5-CCONTA-EMITT           PIC 9(013)    COMP-3.
           05 SLIGC5-PPARTC-SIMPLE          PIC 9(003)V99 COMP-3.
           05 SLIGC5-PPARTC-SIMPLE-NULL     PIC X(001).
           05 SLIGC5-PPARTC-CAUCN           PIC 9(003)V99 COMP-3.
           05 SLIGC5-PPARTC-CAUCN-NULL      PIC X(001).
           05 SLIGC5-PPARTC-DESC            PIC 9(003)V99 COMP-3.
           05 SLIGC5-PPARTC-DESC-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIGC5-CGC-CPF-EMITT-ST       PIC X(009).
           05 SLIGC5-CGC-CPF-EMITT-NULL-ST  PIC X(001).
