      ***===========================================================***
      *** NOME INC                                     LENGTH=00100 ***
      *** I#SLIGC8                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***            BANDA MAGNETICA                                ***
      ***                                                           ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*

       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGC8-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIGC8-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIGC8-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIGC8-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIGC8-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIGC8-CD-CART-CLI            PIC 9(002)    COMP-3.
           05 SLIGC8-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           05 SLIGC8-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           05 SLIGC8-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           05 SLIGC8-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
CNPJ7C     05 SLIGC8-CGC-CPF-CLI-ST         PIC X(009).
CNPJ7C     05 SLIGC8-FILLER                 PIC X(39).

