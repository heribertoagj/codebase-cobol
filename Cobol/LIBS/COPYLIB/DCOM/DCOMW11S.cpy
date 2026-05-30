      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB011: DB2PRD.TCONTR_LIM_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM411S                                       *
      *    -> FETCH   - DCOM411F                                       *
      *    -> INSERT  - DCOM411I                                       *
      *    -> UPDATE  - DCOM411U                                       *
      *    -> DELETE  - DCOM411D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW11S-HEADER.
          10 DCOMW11S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW11S'.
          10 DCOMW11S-TAM-LAYOUT       PIC  9(005) VALUE 21126.
      *
          10 DCOMW11S-BLOCO-SAIDA.
             15 DCOMW11S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW11S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW11S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW11S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMW11S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMW11S-CINDCD-MODLD-DESC      PIC  X(001).
                20 DCOMW11S-CBCO                   PIC  S9(003) COMP-3.
                20 DCOMW11S-CAG-BCRIA              PIC  S9(005) COMP-3.
                20 DCOMW11S-CCTA-BCRIA-CLI         PIC  S9(013) COMP-3.
                20 DCOMW11S-CPOSTO-SERVC           PIC  S9(005) COMP-3.
                20 DCOMW11S-CBCO-OPER-CONTR        PIC  S9(003) COMP-3.
                20 DCOMW11S-CAG-OPER-CONTR         PIC  S9(005) COMP-3.
                20 DCOMW11S-DINCL-CONTR-LIM        PIC  X(010).
                20 DCOMW11S-DINIC-VGCIA-CONTR      PIC  X(010).
                20 DCOMW11S-DVCTO-CONTR-LIM        PIC  X(010).
                20 DCOMW11S-TCONTR-LIM-DESC        PIC  S9(005) COMP-3.
                20 DCOMW11S-CINDCD-PRZ-CONTR       PIC  X(002).
                20 DCOMW11S-VCONTR-LIM-DESC        PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW11S-VUTLZD-CONTR-LIM       PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW11S-VDISPN-CONTR-LIM       PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW11S-HATULZ-SDO-CONTR       PIC  X(026).
                20 DCOMW11S-CINDCD-CONTR-ROTVO     PIC  X(001).
                20 DCOMW11S-CINDCD-RENOV-AUTOM     PIC  X(001).
                20 DCOMW11S-CINDCD-RTEIO-BNEFC     PIC  X(001).
                20 DCOMW11S-CINDCD-LIBRC-CONTR     PIC  X(001).
                20 DCOMW11S-CINDCD-TARIF-ADTTO     PIC  X(001).
                20 DCOMW11S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW11S-DINIC-PARMZ-CONTR      PIC  X(010).
                20 DCOMW11S-CTPO-COBR-TAC          PIC  S9(003) COMP-3.
                20 DCOMW11S-CINDCD-TAC-SPROD       PIC  X(001).
                20 DCOMW11S-VTAC-CONTR-LIM         PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW11S-DTAC-CONTR-LIM         PIC  X(010).
                20 DCOMW11S-CINDCD-TPO-RENOV       PIC  X(001).
                20 DCOMW11S-DRENOV-CONTR-LIM       PIC  X(010).
                20 DCOMW11S-CSIT-DESC-COML         PIC  S9(003) COMP-3.
                20 DCOMW11S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMW11S-DULT-UTILZ-CONTR       PIC  X(010).
                20 DCOMW11S-DULT-ADTTO-CONTR       PIC  X(010).
                20 DCOMW11S-CINDCD-MOTVO-ADTTO     PIC  X(001).
                20 DCOMW11S-VTARIF-ADTTO-CONTR     PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW11S-DCOBR-TARIF-ADTTO      PIC  X(010).
                20 DCOMW11S-CCNPJ-CPF              PIC  S9(009) COMP-3.
                20 DCOMW11S-CFLIAL-CNPJ            PIC  S9(005) COMP-3.
                20 DCOMW11S-CCTRL-CNPJ-CPF         PIC  S9(002) COMP-3.
                20 DCOMW11S-CCLUB                  PIC  S9(010) COMP-3.
                20 DCOMW11S-HULT-ATULZ             PIC  X(026).
                20 DCOMW11S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW11S-CTERM                  PIC  X(008).
                20 DCOMW11S-CINDCD-OPER-INADP      PIC  X(001).
                20 DCOMW11S-DATULZ-OPER-INADP      PIC  X(010).
                20 DCOMW11S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMW11S-FILLER                 PIC  X(077).
      *                                                                *
      *================================================================*
