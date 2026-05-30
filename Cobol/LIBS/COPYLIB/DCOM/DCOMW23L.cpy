      ******************************************************************
      * NOME BOOK : DCOMW23L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA OPERACAO E GARANTIA                       *
      * DATA      : 03/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00480 BYTES                                        *
      ******************************************************************
       05 DCOMW23L-HEADER.
         10 DCOMW23L-COD-LAYOUT              PIC X(08) VALUE 'DCOMW23L'.
         10 DCOMW23L-TAM-LAYOUT                  PIC 9(05) VALUE 00480.
       05 DCOMW23L-REGISTRO.
         10 DCOMW23L-BLOCO-ENTRADA.
           15 DCOMW23L-E-DANO-OPER               PIC 9(04).
           15 DCOMW23L-E-NSEQ-OPER               PIC 9(09).
         10 DCOMW23L-BLOCO-SAIDA1.
           15 DCOMW23L-S-CSIT-DESC               PIC S9(3)V COMP-3.
           15 DCOMW23L-S-HSIT-DESC               PIC X(26).
           15 DCOMW23L-S-VLIBRC-DESC             PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VLIBRC-ANTCP            PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-CBCO                    PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CAG-BCRIA               PIC S9(5)V COMP-3.
           15 DCOMW23L-S-CCTA-BCRIA              PIC S9(13)V COMP-3.
           15 DCOMW23L-S-CBCO-OPER               PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CAG-OPER                PIC S9(5)V COMP-3.
           15 DCOMW23L-S-CPRODT                  PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CSPROD-DESC-COML        PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CTPO-DESC-COML          PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CCONTR-CONVE-DESC       PIC S9(9)V COMP-3.
           15 DCOMW23L-S-CCONTR-LIM              PIC S9(9)V COMP-3.
           15 DCOMW23L-S-CVRSAO-CONTR-CONVE      PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CELMTO-DESC-COML        PIC S9(3)V COMP-3.
           15 DCOMW23L-S-NSMULA-OPER-DESC        PIC S9(9)V COMP-3.
           15 DCOMW23L-S-CINDCD-DIGTC-AG         PIC X(1).
           15 DCOMW23L-S-CINDCD-ISENC-OPER       PIC X(1).
           15 DCOMW23L-S-CINDCD-AUTRZ-OPER       PIC X(1).
           15 DCOMW23L-S-CINDCD-MODLD-INCL       PIC X(1).
           15 DCOMW23L-S-CCTRO-CUSTO             PIC X(4).
           15 DCOMW23L-S-DANO-BASE               PIC S9(4)V COMP-3.
           15 DCOMW23L-S-CDEPDC                  PIC S9(5)V COMP-3.
           15 DCOMW23L-S-CSEQ-STUDO              PIC S9(9)V COMP-3.
           15 DCOMW23L-S-CINDCD-APROV-CREDT      PIC X(1).
           15 DCOMW23L-S-VOPER-DESC-COML         PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-DINIC-OPER-DESC         PIC X(10).
           15 DCOMW23L-S-DVCTO-FNAL-OPER         PIC X(10).
           15 DCOMW23L-S-VOPER-DESC-APURA        PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-QTITLO-OPER-APURA       PIC S9(5)V COMP-3.
           15 DCOMW23L-S-DVCTO-FNAL-APURA        PIC X(10).
           15 DCOMW23L-S-TOPER-DESC-APURA        PIC S9(3)V COMP-3.
           15 DCOMW23L-S-TMED-PONDE-OPER         PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CMEIO-ENTRD-DESC        PIC S9(3)V COMP-3.
           15 DCOMW23L-S-QPCELA-OPER             PIC S9(5)V COMP-3.
           15 DCOMW23L-S-QTITLO-CHEQ-PCELA       PIC S9(9)V COMP-3.
           15 DCOMW23L-S-NAUTRZ-TRNSM-ESCR       PIC S9(7)V COMP-3.
           15 DCOMW23L-S-CINDCD-PGDOR-JURO       PIC X(1).
           15 DCOMW23L-S-CTPO-TX-JURO-DESC       PIC S9(3)V COMP-3.
           15 DCOMW23L-S-PMIN-TX-JURO-REGRA      PIC S9(3)V9(3) COMP-3.
           15 DCOMW23L-S-PMAX-TX-JURO-REGRA      PIC S9(3)V9(3) COMP-3.
           15 DCOMW23L-S-PALIQT-IOF-DESC         PIC S9(3)V9(5) COMP-3.
           15 DCOMW23L-S-PTX-JURO-MES            PIC S9(3)V9(5) COMP-3.
           15 DCOMW23L-S-PTX-JURO-ANO            PIC S9(3)V9(5) COMP-3.
           15 DCOMW23L-S-PTX-CUSTO-MES           PIC S9(3)V9(6) COMP-3.
           15 DCOMW23L-S-PTX-CUSTO-ANO           PIC S9(3)V9(6) COMP-3.
           15 DCOMW23L-S-VJURO-OPER-DESC         PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VJURO-CUSTO-OPER        PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VTARIF-REG-TITLO        PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VTAC-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VPSENT-OPER-DESC        PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VIOF-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-VLIQ-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           15 DCOMW23L-S-CINDCD-VALDC-ROTRO      PIC X(1).
           15 DCOMW23L-S-DVALDC-ROTRO-ELMTO      PIC X(10).
           15 DCOMW23L-S-CFUNC-VALDC-ROTRO       PIC S9(9)V COMP-3.
           15 DCOMW23L-S-DULT-ADTTO-OPER         PIC X(10).
           15 DCOMW23L-S-CINDCD-MOTVO-ADTTO      PIC X(1).
           15 DCOMW23L-S-HULT-ATULZ              PIC X(26).
           15 DCOMW23L-S-CFUNC-BDSCO             PIC S9(9)V COMP-3.
           15 DCOMW23L-S-CTERM                   PIC X(8).
           15 DCOMW23L-S-CCONTR-CORP-OPER        PIC S9(7)V COMP-3.
           15 DCOMW23L-S-DINCL-REG               PIC X(10).
           15 DCOMW23L-S-CCART-LIM-CREDT         PIC X(5).
           15 DCOMW23L-S-CCHAVE-ELMTO-DESC       PIC X(45).
           15 DCOMW23L-S-CESPCE-TITLO-COBR       PIC S9(3)V COMP-3.
           15 DCOMW23L-S-CINDCD-ALIQT-REDZD      PIC X(1).
           15 DCOMW23L-S-PTX-JURO-DIA            PIC S9(3)V9(5) COMP-3.
           15 DCOMW23L-S-CINDCD-FREQ-TX          PIC X(2).
         10 DCOMW23L-BLOCO-SAIDA2.
           15 DCOMW23L-S-CGARAN                  PIC  9(003).
           15 DCOMW23L-S-IGARAN                  PIC  X(040).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
