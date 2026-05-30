      ******************************************************************
      *SISTEMA - NET EMPRESA - XXXXXXXX                BOOK = I#DCOMUC *
      *ARQUIVO - SEQUENCIAL DA TABELA TPOSIC_CHEQ_ATIVO                *
      *                                                                *
      *LRELCL  - 109 BYTES                             14/11/2007      *
      ******************************************************************
001    01 UC-REGISTRO.
003       05 UC-CHAVE.
006 02       10 UC-CBCO                        PIC  S9(003)V     COMP-3.
013 03       10 UC-CAG-BCRIA                   PIC  S9(005)V     COMP-3.
015 07       10 UC-CCTA-BCRIA-CLI              PIC  S9(013)V     COMP-3.
017 02       10 UC-CPRODT                      PIC  S9(003)V     COMP-3.
020 02       10 UC-CSPROD-DESC-COML            PIC  S9(003)V     COMP-3.
025 03       10 UC-DANO-OPER-DESC              PIC  S9(004)V     COMP-3.
028 05       10 UC-NSEQ-OPER-DESC              PIC  S9(009)V     COMP-3.
030 03       10 UC-NPCELA-DESC-COML            PIC  S9(005)V     COMP-3.
040 02    05 UC-CSIT-DESC-COML                 PIC  S9(003)V     COMP-3.
041 10    05 UC-DVCTO-PCELA-DESC               PIC   X(010).
050 01    05 UC-DVCTO-PCELA-DESC-NULL          PIC    X(01).
051 09    05 UC-VPRINC-PCELA-DESC              PIC  S9(015)V99   COMP-3.
060 01    05 UC-VPRINC-PCELA-DESC-NULL         PIC    X(01).
061 09    05 UC-VJURO-PCELA-DESC               PIC  S9(015)V99   COMP-3.
070 01    05 UC-VJURO-PCELA-DESC-NULL          PIC    X(01).
071 09    05 UC-VIOF-PCELA-DESC                PIC  S9(015)V99   COMP-3.
080 01    05 UC-VIOF-PCELA-DESC-NULL           PIC    X(01).
081 01    05 UC-VSDO-DVDOR-PCELA               PIC  S9(015)V99   COMP-3.
090 01    05 UC-VSDO-DVDOR-PCELA-NULL          PIC    X(01).
091 09    05 UC-VJURO-MORA-GERC                PIC  S9(015)V99   COMP-3.
100 01    05 UC-VJURO-MORA-GERC-NULL           PIC    X(01).
101 09    05 UC-VIOF-PCELA-VENCD               PIC  S9(015)V99   COMP-3.
106 01    05 UC-VIOF-PCELA-VENCD-NULL          PIC    X(01).
107 05    05 UC-QTITLO-CHEQ-PCELA              PIC  S9(009)V     COMP-3.
109 01    05 UC-QTITLO-CHEQ-PCELA-NULL         PIC    X(01).
108 02    05 UC-TOPER-DESC-APURA               PIC  S9(003)V     COMP-3.
110 01    05 UC-TOPER-DESC-APURA-NULL          PIC    X(01).
