      ***===========================================================***
      *** NOME INC                                     LENGTH=00100 ***
      *** I#SLIGCH                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***            BANDA MAGNETICA                                ***
      ***                                                           ***
      ***           (COPIA DA I#SLIGC8                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***                                                           ***
      ***===========================================================***
      *
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*

       01  REG-INTERMEDIARIO-COCEN.
           05 SLIGCH-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIGCH-CFLIAL-CLI             PIC 9(005) COMP-3.
           05 SLIGCH-CCTRL-CLI              PIC 9(002) COMP-3.
           05 SLIGCH-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIGCH-BCO-EMITT              PIC 9(003) COMP-3.
           05 SLIGCH-AGENCIA-EMITT          PIC 9(005) COMP-3.
           05 SLIGCH-CONTA-EMITT            PIC 9(013) COMP-3.
           05 SLIGCH-CD-CART-CLI            PIC 9(002)    COMP-3.
           05 SLIGCH-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           05 SLIGCH-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           05 SLIGCH-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           05 SLIGCH-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
CNPJ7C     05 SLIGCH-CGC-CPF-CLI-N          PIC X(009).
CNPJ7C     05 SLIGCH-CFLIAL-CLI-N           PIC X(005).
CNPJ7C     05 SLIGCH-CCTRL-CLI-N            PIC 9(002) COMP-3.
CNPJ7C     05 SLIGCH-FILLER                 PIC X(27).

