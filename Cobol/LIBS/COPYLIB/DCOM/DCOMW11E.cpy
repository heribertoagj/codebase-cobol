      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW11E-HEADER.
          10 DCOMW11E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW11E'.
          10 DCOMW11E-TAM-LAYOUT       PIC  9(005) VALUE 01270.
      *
       05 DCOMW11E-ENTRADA.
          10 DCOMW11E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW11E-BLOCO-ENTRADA.
             15 DCOMW11E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMW11E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMW11E-CINDCD-MODLD-DESC         PIC  X(001).
             15 DCOMW11E-CBCO                      PIC  S9(003) COMP-3.
             15 DCOMW11E-CAG-BCRIA                 PIC  S9(005) COMP-3.
             15 DCOMW11E-CCTA-BCRIA-CLI            PIC  S9(013) COMP-3.
             15 DCOMW11E-CPOSTO-SERVC              PIC  S9(005) COMP-3.
             15 DCOMW11E-CBCO-OPER-CONTR           PIC  S9(003) COMP-3.
             15 DCOMW11E-CAG-OPER-CONTR            PIC  S9(005) COMP-3.
             15 DCOMW11E-DINCL-CONTR-LIM           PIC  X(010).
             15 DCOMW11E-DINIC-VGCIA-CONTR         PIC  X(010).
             15 DCOMW11E-DVCTO-CONTR-LIM           PIC  X(010).
             15 DCOMW11E-TCONTR-LIM-DESC           PIC  S9(005) COMP-3.
             15 DCOMW11E-CINDCD-PRZ-CONTR          PIC  X(002).
             15 DCOMW11E-VCONTR-LIM-DESC           PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW11E-VUTLZD-CONTR-LIM          PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW11E-VDISPN-CONTR-LIM          PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW11E-HATULZ-SDO-CONTR          PIC  X(026).
             15 DCOMW11E-CINDCD-CONTR-ROTVO        PIC  X(001).
             15 DCOMW11E-CINDCD-RENOV-AUTOM        PIC  X(001).
             15 DCOMW11E-CINDCD-RTEIO-BNEFC        PIC  X(001).
             15 DCOMW11E-CINDCD-LIBRC-CONTR        PIC  X(001).
             15 DCOMW11E-CINDCD-TARIF-ADTTO        PIC  X(001).
             15 DCOMW11E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW11E-DINIC-PARMZ-CONTR         PIC  X(010).
             15 DCOMW11E-CTPO-COBR-TAC             PIC  S9(003) COMP-3.
             15 DCOMW11E-CINDCD-TAC-SPROD          PIC  X(001).
             15 DCOMW11E-VTAC-CONTR-LIM            PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW11E-DTAC-CONTR-LIM            PIC  X(010).
             15 DCOMW11E-CINDCD-TPO-RENOV          PIC  X(001).
             15 DCOMW11E-DRENOV-CONTR-LIM          PIC  X(010).
             15 DCOMW11E-CSIT-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMW11E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMW11E-DULT-UTILZ-CONTR          PIC  X(010).
             15 DCOMW11E-DULT-ADTTO-CONTR          PIC  X(010).
             15 DCOMW11E-CINDCD-MOTVO-ADTTO        PIC  X(001).
             15 DCOMW11E-VTARIF-ADTTO-CONTR        PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW11E-DCOBR-TARIF-ADTTO         PIC  X(010).
             15 DCOMW11E-CCNPJ-CPF                 PIC  S9(009) COMP-3.
             15 DCOMW11E-CFLIAL-CNPJ               PIC  S9(005) COMP-3.
             15 DCOMW11E-CCTRL-CNPJ-CPF            PIC  S9(002) COMP-3.
             15 DCOMW11E-CCLUB                     PIC  S9(010) COMP-3.
             15 DCOMW11E-HULT-ATULZ                PIC  X(026).
             15 DCOMW11E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW11E-CTERM                     PIC  X(008).
             15 DCOMW11E-CINDCD-OPER-INADP         PIC  X(001).
             15 DCOMW11E-DATULZ-OPER-INADP         PIC  X(010).
             15 DCOMW11E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMW11E-FILLER                    PIC  X(077).
      *
          10 DCOMW11E-BLOCO-VARIAVEL.
             15 DCOMW11E-V-CPRODT                  PIC  S9(003) COMP-3.
             15 DCOMW11E-V-CSPROD-DESC-COML        PIC  S9(003) COMP-3.
             15 DCOMW11E-V-FILLER                  PIC  X(294).
      *
          10 DCOMW11E-BLOCO-PAGINACAO.
             15 DCOMW11E-MAX-OCORR                 PIC  9(003).
             15 DCOMW11E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW11E-P-INICIAL              VALUE 'I'.
                88 DCOMW11E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW11E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW11E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW11E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW11E-BLOCO-RESTART.
             15 DCOMW11E-CHAVE-INI.
                20 DCOMW11E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW11E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW11E-I-CBCO                 PIC  S9(003) COMP-3.
                20 DCOMW11E-I-CAG-BCRIA            PIC  S9(005) COMP-3.
                20 DCOMW11E-I-CCTA-BCRIA-CLI       PIC  S9(013) COMP-3.
                20 DCOMW11E-I-CAG-OPER-CONTR       PIC  S9(005) COMP-3.
                20 DCOMW11E-I-DINIC-VGCIA-CONTR    PIC  X(010).
                20 DCOMW11E-I-CSIT-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW11E-I-CCNPJ-CPF            PIC  S9(009) COMP-3.
                20 DCOMW11E-I-CFLIAL-CNPJ          PIC  S9(005) COMP-3.
                20 DCOMW11E-I-CCTRL-CNPJ-CPF       PIC  S9(002) COMP-3.
                20 DCOMW11E-I-TCONTR-LIM-DESC      PIC  S9(005) COMP-3.
                20 DCOMW11E-I-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMW11E-I-FILLER               PIC  X(083).
             15 DCOMW11E-CHAVE-FIM.
                20 DCOMW11E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW11E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW11E-F-CBCO                 PIC  S9(003) COMP-3.
                20 DCOMW11E-F-CAG-BCRIA            PIC  S9(005) COMP-3.
                20 DCOMW11E-F-CCTA-BCRIA-CLI       PIC  S9(013) COMP-3.
                20 DCOMW11E-F-CAG-OPER-CONTR       PIC  S9(005) COMP-3.
                20 DCOMW11E-F-DINIC-VGCIA-CONTR    PIC  X(010).
                20 DCOMW11E-F-CSIT-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW11E-F-CCNPJ-CPF            PIC  S9(009) COMP-3.
                20 DCOMW11E-F-CFLIAL-CNPJ          PIC  S9(005) COMP-3.
                20 DCOMW11E-F-CCTRL-CNPJ-CPF       PIC  S9(002) COMP-3.
                20 DCOMW11E-F-TCONTR-LIM-DESC      PIC  S9(005) COMP-3.
                20 DCOMW11E-F-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMW11E-F-FILLER               PIC  X(083).
      *
          10 DCOMW11E-FILLER                       PIC  X(173).
      *
