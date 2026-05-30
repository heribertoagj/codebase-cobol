      ***===========================================================***
      *** NOME INC                                     LENGTH=115   ***
      *** I#SLIGG4                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES  DE LIQUIDEZ - SLIG         ***
      ***            AREA DE COMUNICACAO DO MODULO SLIG8070         ***
      ***                                   TITULOS                 ***
      *** OBJETIVO : ENVIO DE DADOS PARA O SISTEMA GATI             ***
      ***            - INDICES CARTEIRA SIMPLES                     ***
      ***            - INDICES CARTEIRA CAUCIONADA                  ***
      ***            - INDICES CARTEIRA DE DESCONTOS                ***
      ***                                                           ***
      ***===========================================================***
      *** 04/10/25 - AJUSTE CPF/CNPJ ALFANUMERICO                   ***
      ***===========================================================***
      *
       01  REG-SLIGG4-DADOS.
         03  REG-SLIGG4-ENTR.
           05 SLIGG4-CGC-CPF       PIC S9(09) COMP-3.
           05 SLIGG4-TIPO-PESSOA   PIC X(001).
           05 SLIGG4-INDC-CLI      PIC 9(001).
041025     05 SLIGG4-CGC-CPF-ST    PIC 9(009).
         03  REG-SLIGG4-SAIDA.
           05 SLIGG4-LQZ-FNAL-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PER-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PERANT-S PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-BXA-ANTC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-LQA-BXANTC-S  PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PRORR-S       PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-ABATIM-S      PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PAG-ANTC-S    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-LQZ-FNAL-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PER-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PERANT-C PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-BXA-ANTC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-LQA-BXANTC-C  PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PRORR-C       PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-ABATIM-C      PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PAG-ANTC-C    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-LQZ-FNAL-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PER-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-VCTO-PERANT-D PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-BXA-ANTC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-LQA-BXANTC-D  PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PRORR-D       PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-ABATIM-D      PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-PAG-ANTC-D    PIC S9(03)V9(2) COMP-3.
           05 SLIGG4-COD-RETORNO   PIC S9(05) COMP-3.
           05 SLIGG4-MSG-ERRO      PIC X(030).
FS0417     05 SLIGG4-LQZ-FNAL-S-N      PIC X(001).
'          05 SLIGG4-VCTO-PER-S-N      PIC X(001).
'          05 SLIGG4-VCTO-PERANT-S-N   PIC X(001).
'          05 SLIGG4-BXA-ANTC-S-N      PIC X(001).
'          05 SLIGG4-LQA-BXANTC-S-N    PIC X(001).
'          05 SLIGG4-PRORR-S-N         PIC X(001).
'          05 SLIGG4-ABATIM-S-N        PIC X(001).
'          05 SLIGG4-PAG-ANTC-S-N      PIC X(001).
'          05 SLIGG4-LQZ-FNAL-C-N      PIC X(001).
'          05 SLIGG4-VCTO-PER-C-N      PIC X(001).
'          05 SLIGG4-VCTO-PERANT-C-N   PIC X(001).
'          05 SLIGG4-BXA-ANTC-C-N      PIC X(001).
'          05 SLIGG4-LQA-BXANTC-C-N    PIC X(001).
'          05 SLIGG4-PRORR-C-N         PIC X(001).
'          05 SLIGG4-ABATIM-C-N        PIC X(001).
'          05 SLIGG4-PAG-ANTC-C-N      PIC X(001).
'          05 SLIGG4-LQZ-FNAL-D-N      PIC X(001).
'          05 SLIGG4-VCTO-PER-D-N      PIC X(001).
'          05 SLIGG4-VCTO-PERANT-D-N   PIC X(001).
'          05 SLIGG4-BXA-ANTC-D-N      PIC X(001).
'          05 SLIGG4-LQA-BXANTC-D-N    PIC X(001).
'          05 SLIGG4-PRORR-D-N         PIC X(001).
'          05 SLIGG4-ABATIM-D-N        PIC X(001).
FS0417     05 SLIGG4-PAG-ANTC-D-N      PIC X(001).
           05 SLIGG4-FILLER        PIC X(003).
      *
      ***===========================================================***
      *
      *
