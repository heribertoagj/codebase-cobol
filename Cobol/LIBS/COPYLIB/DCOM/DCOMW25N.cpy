      ******************************************************************
      * NOME BOOK : DCOMW25N - BOOK DE INTERFACE COM SERVICO DCOM425N  *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 18980 BYTES                                        *
      *----------------------------------------------------------------*
      * DCOMW25N-E-TPO-PESQUISA = 1 - PESQUISA PARCIAL-54, PARCIAL-02, *
      *                               PARCIAL-53                       *
      *                           2 - PESQUISA PARCIAL-40              *
      *                           2 - PESQUISA PARCIAL-51              *
      ******************************************************************
           05 DCOMW25N-HEADER.
             10 DCOMW25N-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25N'.
             10 DCOMW25N-TAM-LAYOUT      PIC 9(05) VALUE 18980.
           05 DCOMW25N-REGISTRO.
             10 DCOMW25N-AREA-ENTRADA.
                15 DCOMW25N-E-TPO-PESQUISA            PIC 9(002).
                15 DCOMW25N-E-MAX-OCORR               PIC 9(003).
                15 DCOMW25N-E-CCONVE-GRAL-DESC        PIC 9(009).
             10 DCOMW25N-AREA-SAIDA.
                15 DCOMW25N-S-TOT-REG                 PIC  9(003).
                15 DCOMW25N-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW25N-S-CCONVE-GRAL-DESC     PIC 9(009).
                   25 DCOMW25N-S-CVRSAO-CONVE-GRAL    PIC 9(003).
                   25 DCOMW25N-S-ICONVE-GRAL-DESC     PIC X(040).
                   25 DCOMW25N-S-IRSUMO-CONVE-GRAL    PIC X(015).
                   25 DCOMW25N-S-CBCO-OPER-CONVE      PIC 9(003).
                   25 DCOMW25N-S-CPRODT               PIC 9(003).
                   25 DCOMW25N-S-CSPROD-DESC-COML     PIC 9(003).
                   25 DCOMW25N-S-CTPO-DEPDC-EXCVD     PIC X(001).
                   25 DCOMW25N-S-CINDCD-ABRGC-GEOGR   PIC X(001).
                   25 DCOMW25N-S-DINCL-CONVE-GRAL     PIC X(010).
                   25 DCOMW25N-S-DINIC-VGCIA-CONVE    PIC X(010).
                   25 DCOMW25N-S-DVCTO-CONVE-DESC     PIC X(010).
                   25 DCOMW25N-S-TCONVE-DESC-COML     PIC 9(005).
                   25 DCOMW25N-S-CINDCD-PRZ-CONVE     PIC X(002).
                   25 DCOMW25N-S-VLIM-CONVE-GRAL      PIC 9(015)V9(2).
                   25 DCOMW25N-S-VUTLZD-CONVE-GRAL    PIC 9(015)V9(2).
                   25 DCOMW25N-S-VDISPN-CONVE-GRAL    PIC 9(015)V9(2).
                   25 DCOMW25N-S-HATULZ-SDO-CONVE     PIC X(026).
                   25 DCOMW25N-S-CINDCD-CONVE-ROTVO   PIC X(001).
                   25 DCOMW25N-S-CTPO-COBR-TAC        PIC 9(003).
                   25 DCOMW25N-S-CIDTFD-FIXO-TAC      PIC 9(005).
                   25 DCOMW25N-S-CIDTFD-PERC-TAC      PIC 9(005).
                   25 DCOMW25N-S-CIDTFD-MIN-TAC       PIC 9(005).
                   25 DCOMW25N-S-CIDTFD-MAX-TAC       PIC 9(005).
                   25 DCOMW25N-S-TDEB-PCELA-VENCD     PIC 9(003).
                   25 DCOMW25N-S-CINDCD-PRZ-PCELA     PIC X(002).
                   25 DCOMW25N-S-CINDCD-TARIF-REG     PIC X(001).
                   25 DCOMW25N-S-CINDCD-PGDOR-JURO    PIC X(001).
                   25 DCOMW25N-S-CINDCD-LIBRC-ANTCP   PIC X(001).
                   25 DCOMW25N-S-PMAX-LIBRC-ANTCP     PIC 9(003)V9(3).
                   25 DCOMW25N-S-TMIN-VCTO-TITLO      PIC 9(005).
                   25 DCOMW25N-S-TMAX-VCTO-TITLO      PIC 9(005).
                   25 DCOMW25N-S-CINDCD-PRZ-VCTO      PIC X(002).
                   25 DCOMW25N-S-CREGRA-GERC-MORA     PIC 9(003).
                   25 DCOMW25N-S-CREGRA-GERC-CRATS    PIC 9(003).
                   25 DCOMW25N-S-CINDCD-TX-SPROD      PIC X(001).
                   25 DCOMW25N-S-CTPO-TX-JURO-DESC    PIC 9(003).
                   25 DCOMW25N-S-CIDTFD-TX-JURO       PIC 9(005).
                   25 DCOMW25N-S-PINDIC-ECONM-JURO    PIC 9(003)V9(3).
                   25 DCOMW25N-S-PTX-INDIC-ECONM      PIC 9(003)V9(3).
                   25 DCOMW25N-S-HSIT-DESC-COML       PIC X(026).
                   25 DCOMW25N-S-DULT-UTILZ-CONVE     PIC X(010).
                   25 DCOMW25N-S-DULT-ADTTO-CONVE     PIC X(010).
                   25 DCOMW25N-S-CINDCD-MOTVO-ADTTO   PIC X(001).
                   25 DCOMW25N-S-CSIT-DESC-COML       PIC 9(003).
                   25 DCOMW25N-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW25N-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW25N-S-CTERM                PIC X(008).
                   25 DCOMW25N-S-PTAC-FIXA-CONVE      PIC 9(003)V9(2).
                   25 DCOMW25N-S-CINDCD-PRZ-MAX       PIC X(002).
                   25 DCOMW25N-S-CREGRA-GERC-CONS     PIC 9(003).
                   25 DCOMW25N-S-QDIA-OPER-ATIVO      PIC 9(004).
                   25 DCOMW25N-S-CINDCD-FREQ-ESTOQ    PIC X(002).
                   25 DCOMW25N-S-CINDCD-OPER-ATRSO    PIC X(001).
