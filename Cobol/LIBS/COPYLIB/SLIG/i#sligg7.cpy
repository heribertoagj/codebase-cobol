      ***===========================================================***
      *** NOME INC                                     LENGTH=0045  ***
      *** I#SLIGG7                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES  DE LIQUIDEZ - SLIG         ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8090         ***
      ***                                   VENDOR                  ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SLIGG7-DADOS.
         03  REG-SLIGG7-ENTR.
           05 SLIGG7-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG7-TIPO-PESSOA   PIC X(001).
           05 SLIGG7-INDC-CLI      PIC 9(001).
           05 SLIGG7-CGC-CPF-ST    PIC X(009).
         03  REG-SLIGG7-SAIDA.
           05 SLIGG7-LQZ-FNAL-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG7-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG7-MSG-ERRO      PIC X(030).
FS0417     05 SLIGG7-LQZ-FNAL-S-N  PIC X(01).
           05 SLIGG7-FILLER        PIC X(001).
      *
      ***===========================================================***
      *
      *
