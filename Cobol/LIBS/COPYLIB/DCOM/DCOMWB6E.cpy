      ******************************************************************
      * NOME BOOK : DCOMWB6E                                           *
      * DESCRICAO : LAYOUT OPBK PARA STATUS DE OPERACAO                *
      * DATA      : 08/2022                                            *
      * AUTOR     : BRUNO GRACIANO                                     *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE: DCOM - OPEN BANK                                   *
      * TAMANHO   : 00351 BYTES                                        *
      ******************************************************************
       01 DCOMWB6E-REGISTRO.
          05 DCOMWB6E-DANO-OPER-DESC       PIC S9(04) COMP-3.
          05 DCOMWB6E-NSEQ-OPER-DESC       PIC S9(09) COMP-3.
          05 DCOMWB6E-HULT-ATULZ           PIC  X(26).
          05 DCOMWB6E-CSIT-DESC-COML       PIC S9(03) COMP-3.
          05 DCOMWB6E-HSIT-DESC-COML       PIC  X(26).
          05 DCOMWB6E-VLIBRC-DESC-COML     PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VLIBRC-ANTCP-OPER    PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-CBCO                 PIC S9(03) COMP-3.
          05 DCOMWB6E-CAG-BCRIA            PIC S9(05) COMP-3.
          05 DCOMWB6E-CCTA-BCRIA-CLI       PIC S9(13) COMP-3.
          05 DCOMWB6E-CBCO-OPER-OPER       PIC S9(03) COMP-3.
          05 DCOMWB6E-CAG-OPER-OPER        PIC S9(05) COMP-3.
          05 DCOMWB6E-CPRODT               PIC S9(03) COMP-3.
          05 DCOMWB6E-CSPROD-DESC-COML     PIC S9(03) COMP-3.
          05 DCOMWB6E-CTPO-DESC-COML       PIC S9(03) COMP-3.
          05 DCOMWB6E-CCONTR-CONVE-DESC    PIC S9(09) COMP-3.
          05 DCOMWB6E-CVRSAO-CONTR-CONVE   PIC S9(03) COMP-3.
          05 DCOMWB6E-CELMTO-DESC-COML     PIC S9(03) COMP-3.
          05 DCOMWB6E-NSMULA-OPER-DESC     PIC S9(09) COMP-3.
          05 DCOMWB6E-CINDCD-DIGTC-AG      PIC  X(01).
          05 DCOMWB6E-CINDCD-ISENC-OPER    PIC  X(01).
          05 DCOMWB6E-CINDCD-AUTRZ-OPER    PIC  X(01).
          05 DCOMWB6E-CINDCD-MODLD-INCL    PIC  X(01).
          05 DCOMWB6E-CCTRO-CUSTO          PIC  X(04).
          05 DCOMWB6E-DANO-BASE            PIC S9(04) COMP-3.
          05 DCOMWB6E-CDEPDC               PIC S9(05) COMP-3.
          05 DCOMWB6E-CSEQ-STUDO           PIC S9(09) COMP-3.
          05 DCOMWB6E-CINDCD-APROV-CREDT   PIC  X(01).
          05 DCOMWB6E-VOPER-DESC-COML      PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-DINIC-OPER-DESC      PIC  X(10).
          05 DCOMWB6E-DVCTO-FNAL-OPER      PIC  X(10).
          05 DCOMWB6E-TOPER-DESC-COML      PIC S9(05) COMP-3.
          05 DCOMWB6E-TMED-PONDE-OPER      PIC S9(03) COMP-3.
          05 DCOMWB6E-CMEIO-ENTRD-DESC     PIC S9(03) COMP-3.
          05 DCOMWB6E-QPCELA-OPER          PIC S9(05) COMP-3.
          05 DCOMWB6E-QTITLO-CHEQ-PCELA    PIC S9(09) COMP-3.
          05 DCOMWB6E-NAUTRZ-TRNSM-ESCR    PIC S9(07) COMP-3.
          05 DCOMWB6E-CINDCD-PGDOR-JURO    PIC  X(01).
          05 DCOMWB6E-CTPO-TX-JURO-DESC    PIC S9(03) COMP-3.
          05 DCOMWB6E-PMIN-TX-JURO-REGRA   PIC S9(03)V9(3) COMP-3.
          05 DCOMWB6E-PMAX-TX-JURO-REGRA   PIC S9(03)V9(3) COMP-3.
          05 DCOMWB6E-PALIQT-IOF-DESC      PIC S9(03)V9(5) COMP-3.
          05 DCOMWB6E-PTX-JURO-MES         PIC S9(03)V9(5) COMP-3.
          05 DCOMWB6E-PTX-JURO-ANO         PIC S9(03)V9(5) COMP-3.
          05 DCOMWB6E-PTX-CUSTO-MES        PIC S9(03)V9(6) COMP-3.
          05 DCOMWB6E-PTX-CUSTO-ANO        PIC S9(03)V9(6) COMP-3.
          05 DCOMWB6E-VJURO-OPER-DESC      PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VJURO-CUSTO-OPER     PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VTARIF-REG-TITLO     PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VTAC-OPER-DESC       PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VPSENT-OPER-DESC     PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VIOF-OPER-DESC       PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-VLIQ-OPER-DESC       PIC S9(15)V9(2) COMP-3.
          05 DCOMWB6E-CINDCD-VALDC-ROTRO   PIC  X(01).
          05 DCOMWB6E-DVALDC-ROTRO-ELMTO   PIC  X(10).
          05 DCOMWB6E-CFUNC-VALDC-ROTRO    PIC S9(09) COMP-3.
          05 DCOMWB6E-DULT-ADTTO-OPER      PIC  X(10).
          05 DCOMWB6E-CINDCD-MOTVO-ADTTO   PIC  X(01).
          05 DCOMWB6E-CFUNC-BDSCO          PIC S9(09) COMP-3.
          05 DCOMWB6E-CTERM                PIC  X(08).
          05 DCOMWB6E-CCONTR-CORP-OPER     PIC S9(07) COMP-3.
          05 DCOMWB6E-DINCL-REG            PIC  X(10).
          05 DCOMWB6E-CCART-LIM-CREDT      PIC  X(05).
          05 DCOMWB6E-CINDCD-ALIQT-REDZD   PIC  X(01).
          05 DCOMWB6E-PTX-JURO-DIA         PIC S9(3)V9(5) COMP-3.
          05 DCOMWB6E-CINDCD-FREQ-TX       PIC  X(02).
