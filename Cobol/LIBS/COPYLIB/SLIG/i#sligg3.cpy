      ***===========================================================***
      *** NOME INC                                     LENGTH=0078  ***
      *** I#SLIGG3                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8085         ***
      ***                 CLIENTE   X  SACADO    -  CHEQUES         ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***===========================================================***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG3-DADOS.
         03  REG-SLIGG3-ENTR.
           05 SLIGG3-CGC-CPF-CLI      PIC S9(09) COMP-3.
           05 SLIGG3-TIPO-PS-CLI      PIC X(001).
           05 SLIGG3-CGC-CPF-SCO      PIC S9(09) COMP-3.
           05 SLIGG3-TIPO-PS-SCO      PIC X(001).
041025     05 SLIGG3-CGC-CPF-CLI-ST   PIC X(009).
041025     05 SLIGG3-CGC-CPF-SCO-ST   PIC X(009).
         03  REG-SLIGG3-SAIDA.
           05 SLIGG3-PCT-CONC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG3-VLR-TCONC-S   PIC S9(13)V9(2) COMP-3.
           05 SLIGG3-PCT-CONC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG3-VLR-TCONC-C   PIC S9(13)V9(2) COMP-3.
           05 SLIGG3-PCT-CONC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG3-VLR-TCONC-D   PIC S9(13)V9(2) COMP-3.
           05 SLIGG3-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG3-MSG-ERRO      PIC X(030).
      *
      ***===========================================================***
      *
