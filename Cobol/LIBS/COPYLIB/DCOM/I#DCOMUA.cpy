      ******************************************************************
      *SISTEMA - NET EMPRESA - XXXXXXXX                BOOK = I#DCOMUA *
      *ARQUIVO - SEQUENCIAL DA TABELA TPOSIC_TITLO_ATIVO               *
      *                                                                *
      *LRELCL  - 207 BYTES                             14/11/2007      *
      ******************************************************************
001    01 UA-REGISTRO.
001       05 U7-CHAVE.
001 02       10 UA-CBCO                        PIC  S9(003)V     COMP-3.
003 03       10 UA-CAG-BCRIA                   PIC  S9(005)V     COMP-3.
006 07       10 UA-CCTA-BCRIA-CLI              PIC  S9(013)V     COMP-3.
013 02       10 UA-CPRODT                      PIC  S9(003)V     COMP-3.
015 02       10 UA-CSPROD-DESC-COML            PIC  S9(003)V     COMP-3.
017 03       10 UA-DANO-OPER-DESC              PIC  S9(004)V     COMP-3.
020 05       10 UA-NSEQ-OPER-DESC              PIC  S9(009)V     COMP-3.
025 03       10 UA-NPCELA-DESC-COML            PIC  S9(005)V     COMP-3.
028 02    05 UA-CSIT-DESC-COML                 PIC  S9(003)V     COMP-3.
030 10    05 UA-DVCTO-PCELA-DESC               PIC   X(010).
040 01    05 UA-DVCTO-PCELA-DESC-NULL          PIC   X(01).
041 09    05 UA-VPRINC-PCELA-DESC              PIC  S9(015)V99   COMP-3.
050 01    05 UA-VPRINC-PCELA-DESC-NULL         PIC   X(01).
051 09    05 UA-VJURO-PCELA-DESC               PIC  S9(015)V99   COMP-3.
060 01    05 UA-VJURO-PCELA-DESC-NULL          PIC   X(01).
061 09    05 UA-VIOF-PCELA-DESC                PIC  S9(015)V99   COMP-3.
070 01    05 UA-VIOF-PCELA-DESC-NULL           PIC   X(01).
071 09    05 UA-VSDO-DVDOR-PCELA               PIC  S9(015)V99   COMP-3.
080 01    05 UA-VSDO-DVDOR-PCELA-NULL          PIC   X(01).
081 09    05 UA-VJURO-MORA-GERC                PIC  S9(015)V99   COMP-3.
090 01    05 UA-VJURO-MORA-GERC-NULL           PIC   X(01).
091 09    05 UA-VIOF-PCELA-VENCD               PIC  S9(015)V99   COMP-3.
100 01    05 UA-VIOF-PCELA-VENCD-NULL          PIC   X(01).
101 05    05 UA-CCNPJ-CPF                      PIC  S9(009)V     COMP-3.
106 01    05 UA-CCNPJ-CPF-NULL                 PIC   X(01).
107 03    05 UA-CFLIAL-CNPJ                    PIC  S9(005)V     COMP-3.
110 01    05 UA-CFLIAL-CNPJ-NULL               PIC   X(01).
111 02    05 UA-CCTRL-CNPJ-CPF                 PIC  S9(002)V     COMP-3.
113 01    05 UA-CCTRL-CNPJ-CPF-NULL            PIC   X(01).
114 60    05 UA-IPSSOA-SACDO                   PIC   X(060).
174 01    05 UA-IPSSOA-SACDO-NULL              PIC   X(01).
175 10    05 UA-CTITLO-COBR-BCO                PIC  S9(018)V     COMP-3.
185 01    05 UA-CTITLO-COBR-BCO-NULL           PIC   X(01).
186 15    05 UA-CSEU-NRO-TITLO                 PIC   X(015).
201 01    05 UA-CSEU-NRO-TITLO-NULL            PIC   X(01).
202 05    05 UA-QTITLO-CHEQ-PCELA              PIC  S9(009)V     COMP-3.
207 01    05 UA-QTITLO-CHEQ-PCELA-NULL         PIC   X(01).
