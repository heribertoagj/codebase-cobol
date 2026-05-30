      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPJ                                      *
      * TAMANHO : 0175 BYTES                                           *
      * ARQUIVO : TRANSITORIA FORNECEDOR OPERACAO                      *
      * INCLUDE DA TABELA DCOMB0G0 - DB2PRD.TTRASI_FORNC_OPER          *
      ******************************************************************
       01   PJ-G0-REGISTRO.
  1  3      05 PJ-G0-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5      05 PJ-G0-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3      05 PJ-G0-NFORNC-DESC-COML     PIC S9(4)V USAGE COMP-3.
 12 20      05 PJ-G0-CDOCTO-FSCAL-DESC    PIC X(20).
 32  5      05 PJ-G0-CSERIE-DOCTO-FSCAL   PIC X(5).
 37 10      05 PJ-G0-DEMIS-DOCTO-FSCAL    PIC X(10).
 47 10      05 PJ-G0-DVCTO-DOCTO-FSCAL    PIC X(10).
 57  9      05 PJ-G0-VDOCTO-FSCAL         PIC S9(15)V9(2) USAGE COMP-3.
 66  3      05 PJ-G0-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 69  3      05 PJ-G0-CCONDC-PGTO-FORNC    PIC S9(5)V USAGE COMP-3.
 72 16      05 PJ-G0-CIDTFD-SEQ-PGTO      PIC X(16).
 88  1      05 PJ-G0-CORIGE-PGTO-FORNC    PIC S9(1)V USAGE COMP-3.
 89  5      05 PJ-G0-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 94  3      05 PJ-G0-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 97  2      05 PJ-G0-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 99  1      05 PJ-G0-CINDCD-CONF-UTILZ    PIC X(1).
100  2      05 PJ-G0-CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
102  2      05 PJ-G0-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
104 10      05 PJ-G0-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
114 10      05 PJ-G0-CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
124  2      05 PJ-G0-CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
126  1      05 PJ-G0-CINDCD-SELEC-DESC    PIC X(1).
127 26      05 PJ-G0-HULT-ATULZ           PIC X(26).
153  5      05 PJ-G0-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
158  8      05 PJ-G0-CTERM                PIC X(8).
166 10      05 PJ-G0-DINCL-REG            PIC X(10).
