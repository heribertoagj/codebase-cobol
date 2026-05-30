      ******************************************************************
      ******************************************************************
      * NOME BOOK : I#DCOMZI                                           *
      * DESCRICAO : LAYOUT DA TABELA TTRASI_OPER (DCOMB0G4)            *
      *             LAYOUT DA TABELA TPROT_SOLTC_AGNDA (DCOMB0P5)      *
      * DATA      : 25/08/2020                                         *
      * AUTOR     : AMANDA BELTOSO DOS SANTOS                          *
      * EMPRESA   : CAPGEMINI BRASIL                                   *
      * TAMANHO   : 0999 BYTES                                         *
      ******************************************************************
       05  DCOMZI-REGISTRO.
      *    TABELA TPROT_SOLTC_AGNDA
           10 DCOMZI-CBCO                 PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
           10 DCOMZI-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
           10 DCOMZI-CPROT-SOLTC-AGNDA    PIC X(50).
           10 DCOMZI-CTPO-PROT-AGNDA      PIC X(1).
           10 DCOMZI-QTEMPO-VALDD-PROCM   PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CSIT-PROT-AGNDA      PIC S9(1)V USAGE COMP-3.
           10 DCOMZI-HINCL-REG            PIC X(26).
      *    TABELA TTRASI_OPER_DESC
           10 DCOMZI-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
           10 DCOMZI-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-HSIT-DESC-COML       PIC X(26).
           10 DCOMZI-VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VLIBRC-ANTCP-OPER    PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-CBCO-OPER-OPER       PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CAG-OPER-OPER        PIC S9(5)V USAGE COMP-3.
           10 DCOMZI-CPRODT               PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CTPO-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CVRSAO-CONTR-CONVE   PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CINDCD-DIGTC-AG      PIC X(1).
           10 DCOMZI-CINDCD-ISENC-OPER    PIC X(1).
           10 DCOMZI-CINDCD-AUTRZ-OPER    PIC X(1).
           10 DCOMZI-CINDCD-MODLD-INCL    PIC X(1).
           10 DCOMZI-CCTRO-CUSTO          PIC X(4).
           10 DCOMZI-DANO-BASE            PIC S9(4)V USAGE COMP-3.
           10 DCOMZI-CDEPDC               PIC S9(5)V USAGE COMP-3.
           10 DCOMZI-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CINDCD-APROV-CREDT   PIC X(1).
           10 DCOMZI-VOPER-DESC-COML      PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-DINIC-OPER-DESC      PIC X(10).
           10 DCOMZI-DVCTO-FNAL-OPER      PIC X(10).
           10 DCOMZI-VOPER-DESC-APURA     PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-QTITLO-OPER-APURA    PIC S9(5)V USAGE COMP-3.
           10 DCOMZI-DVCTO-FNAL-APURA     PIC X(10).
           10 DCOMZI-TOPER-DESC-APURA     PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-TMED-PONDE-OPER      PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-QPCELA-OPER          PIC S9(5)V USAGE COMP-3.
           10 DCOMZI-QTITLO-CHEQ-PCELA    PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-NAUTRZ-TRNSM-ESCR    PIC S9(7)V USAGE COMP-3.
           10 DCOMZI-CINDCD-PGDOR-JURO    PIC X(1).
           10 DCOMZI-CTPO-TX-JURO-DESC    PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-PMIN-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
           10 DCOMZI-PMAX-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
           10 DCOMZI-PALIQT-IOF-DESC      PIC S9(3)V9(5) USAGE COMP-3.
           10 DCOMZI-PTX-JURO-MES         PIC S9(3)V9(5) USAGE COMP-3.
           10 DCOMZI-PTX-JURO-ANO         PIC S9(3)V9(5) USAGE COMP-3.
           10 DCOMZI-PTX-CUSTO-MES        PIC S9(3)V9(6) USAGE COMP-3.
           10 DCOMZI-PTX-CUSTO-ANO        PIC S9(3)V9(6) USAGE COMP-3.
           10 DCOMZI-VJURO-OPER-DESC      PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VJURO-CUSTO-OPER     PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VTARIF-REG-TITLO     PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VTAC-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VPSENT-OPER-DESC     PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VIOF-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-VLIQ-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
           10 DCOMZI-CINDCD-VALDC-ROTRO   PIC X(1).
           10 DCOMZI-DVALDC-ROTRO-ELMTO   PIC X(10).
           10 DCOMZI-CFUNC-VALDC-ROTRO    PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-DULT-ADTTO-OPER      PIC X(10).
           10 DCOMZI-CINDCD-MOTVO-ADTTO   PIC X(1).
           10 DCOMZI-HULT-ATULZ           PIC X(26).
           10 DCOMZI-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CTERM                PIC X(8).
           10 DCOMZI-CCONTR-CORP-OPER     PIC S9(7)V USAGE COMP-3.
           10 DCOMZI-DINCL-REG            PIC X(10).
           10 DCOMZI-CCART-LIM-CREDT      PIC X(10).
           10 DCOMZI-CCHAVE-ELMTO-DESC    PIC X(45).
           10 DCOMZI-CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
           10 DCOMZI-CINDCD-ALIQT-REDZD   PIC X(1).
           10 DCOMZI-PTX-JURO-DIA         PIC S9(3)V9(5) USAGE COMP-3.
           10 DCOMZI-CINDCD-FREQ-TX       PIC X(2).
      *    DADOS DE CPF/CNPJ
           10 DCOMZI-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
           10 DCOMZI-CFLIAL-CNPJ          PIC S9(4)V USAGE COMP-3.
           10 DCOMZI-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    DADOS DE ERRO
           10 DCOMZI-MODULO               PIC X(08).
           10 DCOMZI-MSG-RET              PIC X(79).
           10 DCOMZI-COD-RET              PIC 9(04).
           10 DCOMZI-TPO-ACESSO           PIC X(003).
           10 FILLER                      PIC X(391).
