      ******************************************************************
      *SISTEMA - NET EMPRESA - XXXXXXXX                BOOK = I#DCOMUB *
      *ARQUIVO - SEQUENCIAL DA TABELA TPOSIC_TITLO_BXADO               *
      *                                                                *
      *LRELCL  - 218 BYTES                             14/11/2007      *
      ******************************************************************
001    01 UB-REGISTRO.
001       05 UB-CHAVE.
001 02       10 UB-CBCO                        PIC  S9(003)V     COMP-3.
003 03       10 UB-CAG-BCRIA                   PIC  S9(005)V     COMP-3.
006 07       10 UB-CCTA-BCRIA-CLI              PIC  S9(013)V     COMP-3.
013 02       10 UB-CPRODT                      PIC  S9(003)V     COMP-3.
015 02       10 UB-CSPROD-DESC-COML            PIC  S9(003)V     COMP-3.
017 03       10 UB-DANO-OPER-DESC              PIC  S9(004)V     COMP-3.
020 05       10 UB-NSEQ-OPER-DESC              PIC  S9(009)V     COMP-3.
025 03       10 UB-NPCELA-DESC-COML            PIC  S9(005)V     COMP-3.
028 02    05 UB-CSIT-DESC-COML                 PIC  S9(003)V     COMP-3.
030 10    05 UB-DVCTO-PCELA-DESC               PIC   X(010).
040 01    05 UB-DVCTO-PCELA-DESC-NULL          PIC   X(01).
041 09    05 UB-VPRINC-PCELA-DESC              PIC  S9(015)V99   COMP-3.
050 01    05 UB-VPRINC-PCELA-DESC-NULL         PIC   X(01).
051 09    05 UB-VJURO-PCELA-DESC               PIC  S9(015)V99   COMP-3.
060 01    05 UB-VJURO-PCELA-DESC-NULL          PIC   X(01).
061 09    05 UB-VIOF-PCELA-DESC                PIC  S9(015)V99   COMP-3.
070 01    05 UB-VIOF-PCELA-DESC-NULL           PIC   X(01).
071 09    05 UB-VEFETV-BAIXA-PCELA             PIC  S9(015)V99   COMP-3.
080 01    05 UB-VEFETV-BAIXA-PCELA-NULL        PIC   X(01).
081 10    05 UB-DEFETV-BAIXA-PCELA             PIC   X(010).
091 01    05 UB-DEFETV-BAIXA-PCELA-NULL        PIC   X(01).
092 09    05 UB-VJURO-BAIXA-PCELA              PIC  S9(015)V99   COMP-3.
101 01    05 UB-VJURO-BAIXA-PCELA-NULL         PIC   X(01).
102 09    05 UB-VIOF-BAIXA-PCELA               PIC  S9(015)V99   COMP-3.
111 01    05 UB-VIOF-BAIXA-PCELA-NULL          PIC   X(01).
112 05    05 UB-CCNPJ-CPF                      PIC  S9(009)V     COMP-3.
117 01    05 UB-CCNPJ-CPF-NULL                 PIC   X(01).
118 03    05 UB-CFLIAL-CNPJ                    PIC  S9(005)V     COMP-3.
121 01    05 UB-CFLIAL-CNPJ-NULL               PIC   X(01).
122 02    05 UB-CCTRL-CNPJ-CPF                 PIC  S9(002)V     COMP-3.
124 01    05 UB-CCTRL-CNPJ-CPF-NULL            PIC   X(01).
125 60    05 UB-IPSSOA-SACDO                   PIC   X(060).
185 01    05 UB-IPSSOA-SACDO-NULL              PIC   X(01).
186 10    05 UB-CTITLO-COBR-BCO                PIC  S9(018)V     COMP-3.
196 01    05 UB-CTITLO-COBR-BCO-NULL           PIC   X(01).
184 15    05 UB-CSEU-NRO-TITLO                 PIC   X(015).
199 01    05 UB-CSEU-NRO-TITLO-NULL            PIC   X(01).
200 05    05 UB-QTITLO-CHEQ-PCELA              PIC  S9(009)V     COMP-3.
205 01    05 UB-QTITLO-CHEQ-PCELA-NULL         PIC   X(01).
