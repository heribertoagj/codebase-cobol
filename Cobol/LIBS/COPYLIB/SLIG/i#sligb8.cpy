      ***===========================================================***
      *** NOME INC                                     LENGTH=00147 ***
      *** I#SLIGB8                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE CONCENTRACAO     ***
      ***            CLIENTE X EMITENTE PARA O SISTEMA DE           ***
      ***            BASE HISTORICA DE RISCO DE CREDITO             ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGB8-REGISTRO.
           05 SLIGB8-EMPRESA                PIC  9(005).
CNPJ7C     05 SLIGB8-CPF-CNPJ-CLI           PIC  X(009).
           05 SLIGB8-TIPO-PESSOA-CLI        PIC  X(001).
           05 SLIGB8-TIPO-REG               PIC  9(001).
           05 SLIGB8-REG-TOTAIS.
              10 SLIGB8-DATA-REFER          PIC  X(010).
              10 SLIGB8-TOT-ABERTO          PIC  9(015)V99.
              10 SLIGB8-TOT-CART-SIMP       PIC  9(015)V99.
              10 SLIGB8-TOT-CART-CAUC       PIC  9(015)V99.
              10 SLIGB8-TOT-CART-DESC       PIC  9(015)V99.
              10 SLIGB8-RESERVA             PIC  X(053).
           05 SLIGB8-REG-CLIENTE REDEFINES  SLIGB8-REG-TOTAIS.
CNPJ7C        10 SLIGB8-CPF-CNPJ-EMI        PIC  X(009).
              10 SLIGB8-TIPO-PESSOA-EMI     PIC  X(001).
              10 SLIGB8-NOME-EMI            PIC  X(050).
              10 SLIGB8-PERC-TOT-EMI        PIC  9(003)V99.
              10 SLIGB8-PER-EMI-CS          PIC  9(003)V99.
              10 SLIGB8-VLR-EMI-CS          PIC  9(015)V99.
              10 SLIGB8-PER-EMI-CC          PIC  9(003)V99.
              10 SLIGB8-VLR-EMI-CC          PIC  9(015)V99.
              10 SLIGB8-PER-EMI-CD          PIC  9(003)V99.
              10 SLIGB8-VLR-EMI-CD          PIC  9(015)V99.
