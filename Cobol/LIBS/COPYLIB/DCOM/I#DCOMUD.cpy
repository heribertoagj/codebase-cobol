      ******************************************************************
      *SISTEMA - NET EMPRESA - XXXXXXXX                BOOK = I#DCOMUD *
      *ARQUIVO - SEQUENCIAL DA TABELA TPOSIC_CHEQ_BXADO                *
      *                                                                *
      *LRELCL  - 120 BYTES                             14/11/2007      *
      ******************************************************************
001    01 UD-REGISTRO.
001       05 UD-CHAVE.
0 1 02       10 UD-CBCO                        PIC  S9(003)V     COMP-3.
0 3 03       10 UD-CAG-BCRIA                   PIC  S9(005)V     COMP-3.
0 6 07       10 UD-CCTA-BCRIA-CLI              PIC  S9(013)V     COMP-3.
013 02       10 UD-CPRODT                      PIC  S9(003)V     COMP-3.
015 02       10 UD-CSPROD-DESC-COML            PIC  S9(003)V     COMP-3.
017 03       10 UD-DANO-OPER-DESC              PIC  S9(004)V     COMP-3.
020 05       10 UD-NSEQ-OPER-DESC              PIC  S9(009)V     COMP-3.
025 03       10 UD-NPCELA-DESC-COML            PIC  S9(005)V     COMP-3.
028 02    05 UD-CSIT-DESC-COML                 PIC  S9(003)V     COMP-3.
030 10    05 UD-DVCTO-PCELA-DESC               PIC   X(010).
040 01    05 UD-DVCTO-PCELA-DESC-NULL          PIC   X(01).
041 09    05 UD-VPRINC-PCELA-DESC              PIC  S9(015)V99   COMP-3.
050 01    05 UD-VPRINC-PCELA-DESC-NULL         PIC   X(01).
051 09    05 UD-VJURO-PCELA-DESC               PIC  S9(015)V99   COMP-3.
060 01    05 UD-VJURO-PCELA-DESC-NULL          PIC   X(01).
061 09    05 UD-VIOF-PCELA-DESC                PIC  S9(015)V99   COMP-3.
070 01    05 UD-VIOF-PCELA-DESC-NULL           PIC   X(01).
071 09    05 UD-VEFETV-BAIXA-PCELA             PIC  S9(015)V99   COMP-3.
080 01    05 UD-VEFETV-BAIXA-PCELA-NULL        PIC   X(01).
081 10    05 UD-DEFETV-BAIXA-PCELA             PIC   X(010).
091 01    05 UD-DEFETV-BAIXA-PCELA-NULL        PIC   X(01).
092 09    05 UD-VJURO-BAIXA-PCELA              PIC  S9(015)V99   COMP-3.
101 01    05 UD-VJURO-BAIXA-PCELA-NULL         PIC   X(01).
102 09    05 UD-VIOF-BAIXA-PCELA               PIC  S9(015)V99   COMP-3.
111 01    05 UD-VIOF-BAIXA-PCELA-NULL          PIC   X(01).
112 05    05 UD-QTITLO-CHEQ-PCELA              PIC  S9(009)V     COMP-3.
117 01    05 UD-QTITLO-CHEQ-PCELA-NULL         PIC   X(01).
118 02    05 UD-TOPER-DESC-APURA               PIC  S9(003)V     COMP-3.
120 01    05 UD-TOPER-DESC-APURA-NULL          PIC   X(01).
