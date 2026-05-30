      ***===========================================================***
      *** NOME INC                                     LENGTH=0060  ***
      *** I#SLIGG9                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8095         ***
      ***                 CLIENTE   X  SACADO    -  VENDOR          ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SLIGG9-DADOS.
         03  REG-SLIGG9-ENTR.
           05 SLIGG9-CGC-CPF-CLI   PIC S9(09) COMP-3.
           05 SLIGG9-TIPO-PS-CLI   PIC X(001).
           05 SLIGG9-CGC-CPF-SCO   PIC S9(09) COMP-3.
           05 SLIGG9-TIPO-PS-SCO   PIC X(001).
           05 SLIGG9-CGC-CPF-CLI-ST PIC X(009).
           05 SLIGG9-CGC-CPF-SCO-ST PIC X(009).
         03  REG-SLIGG9-SAIDA.
           05 SLIGG9-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG9-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG9-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG9-MSG-ERRO      PIC X(030).
           05 FILLER               PIC X(003).
      *
      ***===========================================================***
      *
