      ***===========================================================***
      *** NOME INC                                     LENGTH=0050  ***
      *** I#SLIGG8                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8093         ***
      ***                    BANCO   X   CLIENTE  -  VENDOR         ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SLIGG8-DADOS.
         03  REG-SLIGG8-ENTR.
           05 SLIGG8-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG8-TIPO-PESSOA   PIC X(001).
           05 SLIGG8-CGC-CPF-ST    PIC X(009).
         03  REG-SLIGG8-SAIDA.
           05 SLIGG8-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG8-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG8-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG8-MSG-ERRO      PIC X(030).
      *
      ***===========================================================***
      *
