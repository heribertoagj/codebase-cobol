      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPV                                      *
      * TAMANHO : 0123 BYTES                                           *
      * ARQUIVO : TRANSITORIA SELECAO OPERACAO                         *
      * INCLUDE DA TABELA DCOMB0G7 - DB2PRD.TTRASI_SELEC_OPER          *
      ******************************************************************
       01  PV-G7-REGISTRO.
 01 03     05 PV-G7-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
 04 05     05 PV-G7-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
 09 09     05 PV-G7-VOPER-ARGMT-SELEC    PIC S9(15)V9(2) USAGE COMP-3.
 18 09     05 PV-G7-VFAIXA-VLR-INIC      PIC S9(15)V9(2) USAGE COMP-3.
 27 09     05 PV-G7-VFAIXA-VLR-FNAL      PIC S9(15)V9(2) USAGE COMP-3.
 36 10     05 PV-G7-DFAIXA-VCTO-INIC     PIC X(10).
 46 10     05 PV-G7-DFAIXA-VCTO-FNAL     PIC X(10).
 56 02     05 PV-G7-QDIA-PRZ-INIC        PIC S9(3)V USAGE COMP-3.
 58 02     05 PV-G7-QDIA-PRZ-FNAL        PIC S9(3)V USAGE COMP-3.
 60 01     05 PV-G7-CINDCD-SELEC-VCTO    PIC S9(1)V USAGE COMP-3.
 61 01     05 PV-G7-CINDCD-SELEC-VLR     PIC S9(1)V USAGE COMP-3.
 62 02     05 PV-G7-CINDCD-SELEC-ESPEC   PIC X(2).
 64 01     05 PV-G7-CINDCD-VDA-RCBVL     PIC X(1).
 65 05     05 PV-G7-CADM-CATAO-CREDT     PIC S9(9)V USAGE COMP-3.
 70 01     05 PV-G7-CTPO-LOTE-OPER       PIC X(1).
 71 01     05 PV-G7-CINDCD-SELEC-CHEQ    PIC X(1).
 72 01     05 PV-G7-CINDCD-LISTA-CART    PIC X(1).
 73 01     05 PV-G7-CINDCD-PRZ-VCTO      PIC X(1).
 74 01     05 PV-G7-CINDCD-SELEC-DESC    PIC X(1).
 75 26     05 PV-G7-HULT-ATULZ           PIC X(26).
101 05     05 PV-G7-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
106 08     05 PV-G7-CTERM                PIC X(8).
114 10     05 PV-G7-DINCL-REG            PIC X(10).
