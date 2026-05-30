      ***===========================================================***
      *** NOME INC                                     LENGTH=0080  ***
      *** I#SLIGG6                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8075         ***
      ***                 CLIENTE   X  SACADO    -  TITULOS         ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***===========================================================***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG6-DADOS.
         03  REG-SLIGG6-ENTR.
           05 SLIGG6-CGC-CPF-CLI    PIC S9(09) COMP-3.
           05 SLIGG6-TIPO-PS-CLI    PIC X(001).
           05 SLIGG6-CGC-CPF-SCO    PIC S9(09) COMP-3.
           05 SLIGG6-TIPO-PS-SCO    PIC X(001).
041025     05 SLIGG6-CGC-CPF-CLI-ST PIC X(009).
041025     05 SLIGG6-CGC-CPF-SCO-ST PIC X(009).
         03  REG-SLIGG6-SAIDA.
           05 SLIGG6-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG6-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG6-PCT-CONC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG6-VLR-TCONC-C   PIC S9(13)V9(2) COMP-3.
           05 SLIGG6-PCT-CONC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG6-VLR-TCONC-D   PIC S9(13)V9(2) COMP-3.
           05 SLIGG6-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG6-MSG-ERRO      PIC X(030).
           05 FILLER               PIC X(002).
      *
      ***===========================================================***
      *

















