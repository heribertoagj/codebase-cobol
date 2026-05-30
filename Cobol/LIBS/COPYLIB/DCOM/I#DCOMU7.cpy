      ******************************************************************
      *SISTEMA - NET EMPRESA - XXXXXXXX                BOOK = I#DCOMU7 *
      *ARQUIVO - ARQ. AUXILIAR PARA GERAR CARGA NAS TABELAS            *
      *          TPOSIC_TITLO_ATIVO TPOSIC_TITLO_BXADO                 *
      *          TPOSIC_CHEQ_ATIVO  TPOSIC_CHEQ_BXADO                  *
      *LRELCL  - 300 BYTES                             07/11/2007      *
      ******************************************************************
001    01 U7-REGISTRO.
001       05 U7-CHAVE.
001 02       10 U7-CBCO                        PIC  S9(003)V     COMP-3.
003 03       10 U7-CAG-BCRIA                   PIC  S9(005)V     COMP-3.
006 07       10 U7-CCTA-BCRIA-CLI              PIC  S9(013)V     COMP-3.
013 02       10 U7-CPRODT                      PIC  S9(003)V     COMP-3.
015 02       10 U7-CSPROD-DESC-COML            PIC  S9(003)V     COMP-3.
017 03       10 U7-DANO-OPER-DESC              PIC  S9(004)V     COMP-3.
020 05       10 U7-NSEQ-OPER-DESC              PIC  S9(009)V     COMP-3.
025 03       10 U7-NPCELA-DESC-COML            PIC  S9(005)V     COMP-3.
028 02    05 U7-CSIT-DESC-COML                 PIC  S9(003)V     COMP-3.
030 10    05 U7-DVCTO-PCELA-DESC               PIC   X(010).
040 09    05 U7-VPRINC-PCELA-DESC              PIC  S9(015)V99   COMP-3.
049 09    05 U7-VJURO-PCELA-DESC               PIC  S9(015)V99   COMP-3.
058 09    05 U7-VIOF-PCELA-DESC                PIC  S9(015)V99   COMP-3.
067 09    05 U7-VSDO-DVDOR-PCELA               PIC  S9(015)V99   COMP-3.
076 09    05 U7-VJURO-MORA-GERC                PIC  S9(015)V99   COMP-3.
085 09    05 U7-VIOF-PCELA-VENCD               PIC  S9(015)V99   COMP-3.
094 09    05 U7-VEFETV-BAIXA-PCELA             PIC  S9(015)V99   COMP-3.
103 10    05 U7-DEFETV-BAIXA-PCELA             PIC   X(010).
113 09    05 U7-VJURO-BAIXA-PCELA              PIC  S9(015)V99   COMP-3.
122 09    05 U7-VIOF-BAIXA-PCELA               PIC  S9(015)V99   COMP-3.
131 05    05 U7-CCNPJ-CPF                      PIC  S9(009)V     COMP-3.
136 03    05 U7-CFLIAL-CNPJ                    PIC  S9(005)V     COMP-3.
139 02    05 U7-CCTRL-CNPJ-CPF                 PIC  S9(002)V     COMP-3.
141 60    05 U7-IPSSOA-SACDO                   PIC   X(060).
201 10    05 U7-CTITLO-COBR-BCO                PIC  S9(018)V     COMP-3.
211 15    05 U7-CSEU-NRO-TITLO                 PIC   X(015).
226 05    05 U7-QTITLO-CHEQ-PCELA              PIC  S9(009)V     COMP-3.
231 02    05 U7-TOPER-DESC-APURA               PIC  S9(003)V     COMP-3.
233 01    05 U7-CTPO-BAIXA-PCELA               PIC   X(001).
234 67    05 U7-FILLER                         PIC   X(067).
