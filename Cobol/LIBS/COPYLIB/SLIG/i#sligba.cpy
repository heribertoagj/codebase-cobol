      ***===========================================================***
      *** NOME INC                                     LENGTH=00098 ***
      *** I#SLIGBA                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE CONCENTRACAO     ***
      ***            FORNECEDOR X COMPRADOR PARA O SISTEMA DE       ***
      ***            BASE HISTORICA DE RISCO DE CREDITO             ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGBA-REGISTRO.
           05 SLIGBA-EMPRESA                PIC  9(005).
CNPJ7C     05 SLIGBA-CPF-CNPJ-FOR           PIC  X(009).
           05 SLIGBA-TIPO-PESSOA-FOR        PIC  X(001).
           05 SLIGBA-TIPO-REG               PIC  9(001).
           05 SLIGBA-REG-TOTAIS.
              10 SLIGBA-DATA-REFER          PIC  X(010).
              10 SLIGBA-TOT-ABERTO          PIC  9(015)V99.
              10 SLIGBA-RESERVA             PIC  X(055).
           05 SLIGBA-REG-COMPR   REDEFINES  SLIGBA-REG-TOTAIS.
CNPJ7C        10 SLIGBA-CPF-CNPJ-COM        PIC  X(009).
              10 SLIGBA-TIPO-PESSOA-COM     PIC  X(001).
              10 SLIGBA-NOME-COM            PIC  X(050).
              10 SLIGBA-PER-COM-CS          PIC  9(003)V99.
              10 SLIGBA-VLR-COM-CS          PIC  9(015)V99.
