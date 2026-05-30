      ***===========================================================***
      *** NOME INC                                     LENGTH=0105  ***
      *** I#SLIGG1                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES  DE LIQUIDEZ - SLIG         ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8080         ***
      ***                                   CHEQUES                 ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***-----------------------------------------------------------***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG1-DADOS.
         03  REG-SLIGG1-ENTR.
           05 SLIGG1-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG1-TIPO-PESSOA   PIC X(001).
           05 SLIGG1-INDC-CLI      PIC 9(001).
041025     05 SLIGG1-CGC-CPF-ST    PIC X(009).
         03  REG-SLIGG1-SAIDA.
           05 SLIGG1-LQZ-FNAL-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-PRIM-DEP-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-LQZ-REAP-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-VCTO-PER-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-DEP-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-BXA-ANTC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-REAP-S   PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-LQZ-FNAL-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-PRIM-DEP-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-LQZ-REAP-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-VCTO-PER-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-DEP-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-BXA-ANTC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-REAP-C   PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-LQZ-FNAL-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-PRIM-DEP-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-LQZ-REAP-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-VCTO-PER-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-DEP-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-BXA-ANTC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-CHEQ-REAP-D   PIC S9(03)V9(2) COMP-3.
           05 SLIGG1-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG1-MSG-ERRO      PIC X(030).
FS0417     05 SLIGG1-LQZ-FNAL-S-N      PIC X(01).
'          05 SLIGG1-PRIM-DEP-S-N      PIC X(01).
'          05 SLIGG1-LQZ-REAP-S-N      PIC X(01).
'          05 SLIGG1-VCTO-PER-S-N      PIC X(01).
'          05 SLIGG1-CHEQ-DEP-S-N      PIC X(01).
'          05 SLIGG1-BXA-ANTC-S-N      PIC X(01).
'          05 SLIGG1-CHEQ-REAP-S-N     PIC X(01).
'          05 SLIGG1-LQZ-FNAL-C-N      PIC X(01).
'          05 SLIGG1-PRIM-DEP-C-N      PIC X(01).
'          05 SLIGG1-LQZ-REAP-C-N      PIC X(01).
'          05 SLIGG1-VCTO-PER-C-N      PIC X(01).
'          05 SLIGG1-CHEQ-DEP-C-N      PIC X(01).
'          05 SLIGG1-BXA-ANTC-C-N      PIC X(01).
'          05 SLIGG1-CHEQ-REAP-C-N     PIC X(01).
'          05 SLIGG1-LQZ-FNAL-D-N      PIC X(01).
'          05 SLIGG1-PRIM-DEP-D-N      PIC X(01).
'          05 SLIGG1-LQZ-REAP-D-N      PIC X(01).
'          05 SLIGG1-VCTO-PER-D-N      PIC X(01).
'          05 SLIGG1-CHEQ-DEP-D-N      PIC X(01).
'          05 SLIGG1-BXA-ANTC-D-N      PIC X(01).
FS0417     05 SLIGG1-CHEQ-REAP-D-N     PIC X(01).
           05 SLIGG1-FILLER        PIC X(002).
      *
      ***===========================================================***
      *
      *
