      ******************************************************************
      * NOME BOOK : DCOMWC5E                                           *
      * DESCRICAO : LAYOUT OPBK PARA STATUS DE OPERACAO                *
      * DATA      : 08/2022                                            *
      * AUTOR     : BRUNO GRACIANO                                     *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE: DCOM - OPEN BANK                                   *
      * TAMANHO   : 00398 BYTES                                        *
      ******************************************************************
       01 DCOMWC5E-REGISTRO.
           05 DCOMWC5E-DANO-OPER-DESC        PIC S9(04) COMP-3.
           05 DCOMWC5E-NSEQ-OPER-DESC        PIC S9(09) COMP-3.
           05 DCOMWC5E-CSIT-DESC-COML        PIC S9(03) COMP-3.
           05 DCOMWC5E-HSIT-DESC-COML        PIC  X(26).
           05 DCOMWC5E-VLIBRC-DESC-COML      PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VLIBRC-ANTCP-OPER     PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-CBCO                  PIC S9(03) COMP-3.
           05 DCOMWC5E-CAG-BCRIA             PIC S9(05) COMP-3.
           05 DCOMWC5E-CCTA-BCRIA-CLI        PIC S9(13) COMP-3.
           05 DCOMWC5E-CBCO-OPER-OPER        PIC S9(03) COMP-3.
           05 DCOMWC5E-CAG-OPER-OPER         PIC S9(05) COMP-3.
           05 DCOMWC5E-CPRODT                PIC S9(03) COMP-3.
           05 DCOMWC5E-CSPROD-DESC-COML      PIC S9(03) COMP-3.
           05 DCOMWC5E-CTPO-DESC-COML        PIC S9(03) COMP-3.
           05 DCOMWC5E-CCONTR-CONVE-DESC     PIC S9(09) COMP-3.
           05 DCOMWC5E-CVRSAO-CONTR-CONVE    PIC S9(03) COMP-3.
           05 DCOMWC5E-CELMTO-DESC-COML      PIC S9(03) COMP-3.
           05 DCOMWC5E-NSMULA-OPER-DESC      PIC S9(09) COMP-3.
           05 DCOMWC5E-CINDCD-DIGTC-AG       PIC  X(01).
           05 DCOMWC5E-CINDCD-ISENC-OPER     PIC  X(01).
           05 DCOMWC5E-CINDCD-AUTRZ-OPER     PIC  X(01).
           05 DCOMWC5E-CINDCD-MODLD-INCL     PIC  X(01).
           05 DCOMWC5E-CCTRO-CUSTO           PIC  X(04).
           05 DCOMWC5E-DANO-BASE             PIC S9(04) COMP-3.
           05 DCOMWC5E-CDEPDC                PIC S9(05) COMP-3.
           05 DCOMWC5E-CSEQ-STUDO            PIC S9(09) COMP-3.
           05 DCOMWC5E-CINDCD-APROV-CREDT    PIC  X(01).
           05 DCOMWC5E-VOPER-DESC-COML       PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-DINIC-OPER-DESC       PIC  X(10).
           05 DCOMWC5E-DVCTO-FNAL-OPER       PIC  X(10).
           05 DCOMWC5E-TOPER-DESC-COML       PIC S9(05) COMP-3.
           05 DCOMWC5E-TMED-PONDE-OPER       PIC S9(03) COMP-3.
           05 DCOMWC5E-CMEIO-ENTRD-DESC      PIC S9(03) COMP-3.
           05 DCOMWC5E-QPCELA-OPER           PIC S9(05) COMP-3.
           05 DCOMWC5E-QTITLO-CHEQ-PCELA     PIC S9(09) COMP-3.
           05 DCOMWC5E-NAUTRZ-TRNSM-ESCR     PIC S9(07) COMP-3.
           05 DCOMWC5E-CINDCD-PGDOR-JURO     PIC  X(01).
           05 DCOMWC5E-CTPO-TX-JURO-DESC     PIC S9(03) COMP-3.
           05 DCOMWC5E-PMIN-TX-JURO-REGRA    PIC S9(03)V9(3) COMP-3.
           05 DCOMWC5E-PMAX-TX-JURO-REGRA    PIC S9(03)V9(3) COMP-3.
           05 DCOMWC5E-PALIQT-IOF-DESC       PIC S9(03)V9(5) COMP-3.
           05 DCOMWC5E-PTX-JURO-MES          PIC S9(03)V9(5) COMP-3.
           05 DCOMWC5E-PTX-JURO-ANO          PIC S9(03)V9(5) COMP-3.
           05 DCOMWC5E-PTX-CUSTO-MES         PIC S9(03)V9(6) COMP-3.
           05 DCOMWC5E-PTX-CUSTO-ANO         PIC S9(03)V9(6) COMP-3.
           05 DCOMWC5E-VJURO-OPER-DESC       PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VJURO-CUSTO-OPER      PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VTARIF-REG-TITLO      PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VTAC-OPER-DESC        PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VPSENT-OPER-DESC      PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VIOF-OPER-DESC        PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-VLIQ-OPER-DESC        PIC S9(15)V9(2) COMP-3.
           05 DCOMWC5E-CINDCD-VALDC-ROTRO    PIC  X(01).
           05 DCOMWC5E-DVALDC-ROTRO-ELMTO    PIC  X(10).
           05 DCOMWC5E-CFUNC-VALDC-ROTRO     PIC S9(9) COMP-3.
           05 DCOMWC5E-DULT-ADTTO-OPER       PIC  X(10).
           05 DCOMWC5E-DULT-ADTTO-OPER-N     PIC  X(01).
           05 DCOMWC5E-CINDCD-MOTVO-ADTTO    PIC  X(01).
           05 DCOMWC5E-CINDCD-MOTVO-ADTTO-N  PIC  X(01).
           05 DCOMWC5E-HULT-ATULZ            PIC  X(26).
           05 DCOMWC5E-CFUNC-BDSCO           PIC S9(09) COMP-3.
           05 DCOMWC5E-CTERM                 PIC  X(08).
           05 DCOMWC5E-CCONTR-CORP-OPER      PIC S9(07) COMP-3.
           05 DCOMWC5E-DINCL-REG             PIC  X(10).
           05 DCOMWC5E-CCART-LIM-CREDT       PIC  X(05).
           05 DCOMWC5E-CCHAVE-ELMTO-DESC     PIC  X(45).
           05 DCOMWC5E-CINDCD-ALIQT-REDZD    PIC  X(01).
           05 DCOMWC5E-PTX-JURO-DIA          PIC S9(03)V9(5) COMP-3.
           05 DCOMWC5E-CINDCD-FREQ-TX        PIC  X(02).
