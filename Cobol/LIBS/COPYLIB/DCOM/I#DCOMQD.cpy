      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQD                                      *
      * TAMANHO : 0054 BYTES                                           *
      * ARQUIVO : RELACAO CHEQUE IMPRESSAO                             *
      * INCLUDE DA TABELA DCOMB0D5 - DB2PRD.TRELAC_CHEQ_IMPRE          *
      ******************************************************************
       01  DCOMB0D5.
01 03      05 QD-D5-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 QD-D5-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 03      05 QD-D5-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
12 02      05 QD-D5-CBCO                 PIC S9(3)V USAGE COMP-3.
14 03      05 QD-D5-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
17 06      05 QD-D5-CCTA-CORR-BCRIA      PIC S9(11)V USAGE COMP-3.
23 04      05 QD-D5-CCHEQ                PIC S9(7)V USAGE COMP-3.
27 03      05 QD-D5-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
30 05      05 QD-D5-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
35 02      05 QD-D5-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
37 10      05 QD-D5-DVCTO-CHEQ-CTDIA     PIC X(10).
47 08      05 QD-D5-VCHEQC               PIC S9(13)V9(2) USAGE COMP-3.
