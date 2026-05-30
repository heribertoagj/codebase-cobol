      ***===========================================================***
      *** NOME INC                                     LENGTH=0075  ***
      *** I#SLIGG5                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8073         ***
      ***                    BANCO   X   CLIENTE  -  TITULOS        ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***===========================================================***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG5-DADOS.
         03  REG-SLIGG5-ENTR.
           05 SLIGG5-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG5-TIPO-PESSOA   PIC X(001).
041025     05 SLIGG5-CGC-CPF-ST    PIC 9(009).
         03  REG-SLIGG5-SAIDA.
           05 SLIGG5-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG5-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG5-PCT-CONC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG5-VLR-TCONC-C   PIC S9(13)V9(2) COMP-3.
           05 SLIGG5-PCT-CONC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG5-VLR-TCONC-D   PIC S9(13)V9(2) COMP-3.
           05 SLIGG5-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG5-MSG-ERRO      PIC X(030).
           05 SLIGG5-FILLER        PIC X(003).
      *
      ***===========================================================***
      *
