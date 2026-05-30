      ***===========================================================***
      *** NOME INC                                     LENGTH=00147 ***
      *** I#SLIGB9                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE CONCENTRACAO     ***
      ***            CLIENTE X SACADO PARA O SISTEMA DE BASE        ***
      ***            HISTORICA DE RISCO DE CREDITO                  ***
      ***===========================================================***
      *
       01  SLIGB9-REGISTRO.
           05 SLIGB9-EMPRESA                PIC  9(005).
CNPJ7C     05 SLIGB9-CPF-CNPJ-CLI           PIC  X(009).
           05 SLIGB9-TIPO-PESSOA-CLI        PIC  X(001).
           05 SLIGB9-TIPO-REG               PIC  9(001).
           05 SLIGB9-REG-TOTAIS.
              10 SLIGB9-DATA-REFER          PIC  X(010).
              10 SLIGB9-TOT-ABERTO          PIC  9(015)V99.
              10 SLIGB9-TOT-CART-SIMP       PIC  9(015)V99.
              10 SLIGB9-TOT-CART-CAUC       PIC  9(015)V99.
              10 SLIGB9-TOT-CART-DESC       PIC  9(015)V99.
              10 SLIGB9-RESERVA             PIC  X(053).
           05 SLIGB9-REG-SACADO  REDEFINES  SLIGB9-REG-TOTAIS.
CNPJ7C        10 SLIGB9-CPF-CNPJ-SAC        PIC  X(009).
              10 SLIGB9-TIPO-PESSOA-SAC     PIC  X(001).
              10 SLIGB9-NOME-SAC            PIC  X(050).
              10 SLIGB9-PERC-TOT-SAC        PIC  9(003)V99.
              10 SLIGB9-PER-SAC-CS          PIC  9(003)V99.
              10 SLIGB9-VLR-SAC-CS          PIC  9(015)V99.
              10 SLIGB9-PER-SAC-CC          PIC  9(003)V99.
              10 SLIGB9-VLR-SAC-CC          PIC  9(015)V99.
              10 SLIGB9-PER-SAC-CD          PIC  9(003)V99.
              10 SLIGB9-VLR-SAC-CD          PIC  9(015)V99.
