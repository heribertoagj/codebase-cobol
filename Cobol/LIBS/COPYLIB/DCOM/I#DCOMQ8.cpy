      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ8                                      *
      * TAMANHO : 0101 BYTES                                           *
      * ARQUIVO : OPERACAO ISENCAO PESSOA                              *
      * INCLUDE DA TABELA DCOMB036 - DB2PRD.TOPER_ISENC_PSSOA          *
      ******************************************************************
       01  Q8-36-REGISTRO.
 01 05     05 Q8-36-CCNPJ-CPF            PIC S9(9)V  USAGE COMP-3.
 06 02     05 Q8-36-NSEQ-PSSOA-DESC      PIC S9(3)V  USAGE COMP-3.
 08 02     05 Q8-36-CTPO-ISENC-DESC      PIC S9(3)V  USAGE COMP-3.
 10 10     05 Q8-36-DINIC-VGCIA-ISENC    PIC X(10).
 20 03     05 Q8-36-DANO-OPER-DESC       PIC S9(4)V  USAGE COMP-3.
 23 05     05 Q8-36-NSEQ-OPER-DESC       PIC S9(9)V  USAGE COMP-3.
 28 03     05 Q8-36-CFLIAL-CNPJ          PIC S9(5)V  USAGE COMP-3.
 31 02     05 Q8-36-CCTRL-CNPJ-CPF       PIC S9(2)V  USAGE COMP-3.
 33 06     05 Q8-36-CCLUB                PIC S9(10)V USAGE COMP-3.
 39 02     05 Q8-36-CINDCD-PSSOA-DESC    PIC X(2).
 41 10     05 Q8-36-DFIM-VGCIA-ISENC     PIC X(10).
 51 26     05 Q8-36-HULT-ATULZ           PIC X(26).
 77 05     05 Q8-36-CFUNC-BDSCO          PIC S9(9)V  USAGE COMP-3.
 82 08     05 Q8-36-CTERM                PIC X(8).
 90 02     05 Q8-36-CBCO                 PIC S9(3)V  USAGE COMP-3.
 92 03     05 Q8-36-CAG-BCRIA            PIC S9(5)V  USAGE COMP-3.
 95 07     05 Q8-36-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
