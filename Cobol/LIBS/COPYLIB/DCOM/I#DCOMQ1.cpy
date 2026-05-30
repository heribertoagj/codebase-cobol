      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ1                                      *
      * TAMANHO : 0174 BYTES                                           *
      * ARQUIVO : FORNECEDOR OPERACAO DESCONTO COMERCIAL               *
      * INCLUDE DA TABELA DCOMB0A3 - DB2PRD.TFORNC_DESC_COML           *
      ******************************************************************
       01   Q1-A3-REGISTRO.
  1  3      05 Q1-A3-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5      05 Q1-A3-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3      05 Q1-A3-NFORNC-DESC-COML     PIC S9(4)V USAGE COMP-3.
 12 20      05 Q1-A3-CDOCTO-FSCAL-DESC    PIC X(20).
 32  5      05 Q1-A3-CSERIE-DOCTO-FSCAL   PIC X(5).
 37 10      05 Q1-A3-DEMIS-DOCTO-FSCAL    PIC X(10).
 47 10      05 Q1-A3-DVCTO-DOCTO-FSCAL    PIC X(10).
 57  9      05 Q1-A3-VDOCTO-FSCAL         PIC S9(15)V9(2) USAGE COMP-3.
 66  3      05 Q1-A3-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 69  3      05 Q1-A3-CCONDC-PGTO-FORNC    PIC S9(5)V USAGE COMP-3.
 72 16      05 Q1-A3-CIDTFD-SEQ-PGTO      PIC X(16).
 88  1      05 Q1-A3-CORIGE-PGTO-FORNC    PIC S9(1)V USAGE COMP-3.
 89  5      05 Q1-A3-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 94  3      05 Q1-A3-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 97  2      05 Q1-A3-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 99  1      05 Q1-A3-CINDCD-CONF-UTILZ    PIC X(1).
100  2      05 Q1-A3-CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
102  2      05 Q1-A3-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
104 10      05 Q1-A3-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
114 10      05 Q1-A3-CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
124  2      05 Q1-A3-CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
126 26      05 Q1-A3-HULT-ATULZ           PIC X(26).
152  5      05 Q1-A3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
157  8      05 Q1-A3-CTERM                PIC X(8).
165 10      05 Q1-A3-DINCL-REG            PIC X(10).
