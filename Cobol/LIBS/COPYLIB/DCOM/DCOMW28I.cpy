      ******************************************************************
      * NOME BOOK : DCOMW28I - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA DETALHE CONTRATO                          *
      * DATA      : 27/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 01023 BYTES                                        *
      ******************************************************************
       05 DCOMW28I-HEADER.
          10 DCOMW28I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW28I'.
          10 DCOMW28I-TAM-LAYOUT             PIC 9(05) VALUE 01023.
       05 DCOMW28I-REGISTRO.
          10 DCOMW28I-ENTRADA.
             15 DCOMW28I-E-DANO-OPER         PIC 9(04).
             15 DCOMW28I-E-NSEQ-OPER         PIC 9(09).
          10 DCOMW28I-BLOCO-SAIDA.
             15 DCOMW28I-IPSSOA-DESC-COML    PIC X(40).
             15 DCOMW28I-CCNPJ-CPF           PIC 9(09).
             15 DCOMW28I-CFLIAL-CNPJ         PIC 9(04).
             15 DCOMW28I-CCTRL-CNPJ-CPF      PIC 9(02).
             15 DCOMW28I-CBCO                PIC 9(05).
             15 DCOMW28I-NOME-BANCO          PIC X(20).
             15 DCOMW28I-CAG-BCRIA           PIC 9(05).
             15 DCOMW28I-DIGITO-AGENCIA      PIC X(02).
             15 DCOMW28I-NOME-AGENCIA        PIC X(20).
             15 DCOMW28I-CCTA-BCRIA-CLI      PIC 9(13).
             15 DCOMW28I-DIGITO-CONTA        PIC X(02).
             15 DCOMW28I-OPERACAO            PIC 9(13).
             15 DCOMW28I-CPRODT              PIC 9(03).
             15 DCOMW28I-IRSUMO-CPRODT       PIC X(15).
             15 DCOMW28I-CSPROD-DESC-COML    PIC 9(03).
             15 DCOMW28I-IRSUMO-CSPROD       PIC X(15).
             15 DCOMW28I-IRSUMO-SIT-DESC     PIC X(15).
             15 DCOMW28I-CCONTR-CONVE-DESC   PIC 9(09).
             15 DCOMW28I-CONTRATO-LIMITE     PIC 9(09).
             15 DCOMW28I-VOPER-DESC-COML     PIC 9(15)V99.
             15 DCOMW28I-VTAC-OPER-DESC      PIC 9(13)V99.
             15 DCOMW28I-VTARIF-REG-TITLO    PIC 9(13)V99.
             15 DCOMW28I-VJURO-OPER-DESC     PIC 9(13)V99.
             15 DCOMW28I-VIOF-OPER-DESC      PIC 9(13)V99.
             15 DCOMW28I-IRSUMO-TPO-TX-JURO  PIC X(15).
             15 DCOMW28I-PTX-JURO-MES        PIC 9(07)V9(06).
             15 DCOMW28I-DVCTO-FNAL-OPER     PIC X(10).
             15 DCOMW28I-GARANTIA            PIC X(40).
             15 DCOMW28I-COD-CONE            PIC X(18).
             15 DCOMW28I-VTRIBUTOS           PIC 9(15)V99.
             15 DCOMW28I-VSEGUROS            PIC 9(15)V99.
             15 DCOMW28I-VTARIFA-CET         PIC 9(15)V99.
             15 DCOMW28I-VPGTOSERV           PIC 9(15)V99.
             15 DCOMW28I-VREGISTROS          PIC 9(15)V99.
             15 DCOMW28I-VCET-TOTAL          PIC 9(15)V99.
             15 DCOMW28I-VCET-MES            PIC 9(08)V9(07).
             15 DCOMW28I-QTITLO              PIC 9(09).
             15 DCOMW28I-CMEIO-ENTRD         PIC 9(03).
             15 DCOMW28I-CTPO-DESC           PIC 9(03).
             15 DCOMW28I-CSIT-DESC           PIC 9(03).
             15 DCOMW28I-HSIT-DESC           PIC X(26).
             15 DCOMW28I-VLIBRC-DESC         PIC 9(15)V9(2).
             15 DCOMW28I-VLIBRC-ANTCP        PIC 9(15)V9(2).
             15 DCOMW28I-CBCO-OPER           PIC 9(03).
             15 DCOMW28I-CAG-OPER            PIC 9(05).
             15 DCOMW28I-CTPO-DESC-COML      PIC 9(03).
             15 DCOMW28I-CVRSAO-CONTR-CONVE  PIC 9(03).
             15 DCOMW28I-CELMTO-DESC-COML    PIC 9(03).
             15 DCOMW28I-NSMULA-OPER-DESC    PIC 9(09).
             15 DCOMW28I-CINDCD-DIGTC-AG     PIC X(01).
             15 DCOMW28I-CINDCD-ISENC-OPER   PIC X(01).
             15 DCOMW28I-CINDCD-AUTRZ-OPER   PIC X(01).
             15 DCOMW28I-CINDCD-MODLD-INCL   PIC X(01).
             15 DCOMW28I-CCTRO-CUSTO         PIC X(04).
             15 DCOMW28I-CINDCD-APROV-CREDT  PIC X(01).
             15 DCOMW28I-DINIC-OPER-DESC     PIC X(10).
             15 DCOMW28I-VOPER-DESC-APURA    PIC 9(15)V9(2).
             15 DCOMW28I-QTITLO-OPER-APURA   PIC 9(05).
             15 DCOMW28I-DVCTO-FNAL-APURA    PIC X(10).
             15 DCOMW28I-TOPER-DESC-APURA    PIC 9(03).
             15 DCOMW28I-TMED-PONDE-OPER     PIC 9(03).
             15 DCOMW28I-QPCELA-OPER         PIC 9(05).
             15 DCOMW28I-NAUTRZ-TRNSM-ESCR   PIC 9(07).
             15 DCOMW28I-CINDCD-PGDOR-JURO   PIC X(01).
             15 DCOMW28I-CTPO-TX-JURO-DESC   PIC 9(03).
             15 DCOMW28I-PMIN-TX-JURO-REGRA  PIC 9(03)V9(3).
             15 DCOMW28I-PMAX-TX-JURO-REGRA  PIC 9(03)V9(3).
             15 DCOMW28I-PALIQT-IOF-DESC     PIC 9(03)V9(5).
             15 DCOMW28I-PTX-JURO-ANO        PIC 9(03)V9(5).
             15 DCOMW28I-PTX-CUSTO-MES       PIC 9(03)V9(6).
             15 DCOMW28I-PTX-CUSTO-ANO       PIC 9(03)V9(6).
             15 DCOMW28I-VJURO-CUSTO-OPER    PIC 9(15)V9(2).
             15 DCOMW28I-VPSENT-OPER-DESC    PIC 9(15)V9(2).
             15 DCOMW28I-VLIQ-OPER-DESC      PIC 9(15)V9(2).
             15 DCOMW28I-CINDCD-VALDC-ROTRO  PIC X(01).
             15 DCOMW28I-DVALDC-ROTRO-ELMTO  PIC X(10).
             15 DCOMW28I-CFUNC-VALDC-ROTRO   PIC 9(09).
             15 DCOMW28I-DULT-ADTTO-OPER     PIC X(10).
             15 DCOMW28I-CINDCD-MOTVO-ADTTO  PIC X(01).
             15 DCOMW28I-HULT-ATULZ          PIC X(26).
             15 DCOMW28I-CFUNC-BDSCO         PIC 9(09).
             15 DCOMW28I-CTERM               PIC X(08).
             15 DCOMW28I-CCONTR-CORP-OPER    PIC 9(07).
             15 DCOMW28I-DINCL-REG           PIC X(10).
             15 DCOMW28I-CCART-LIM-CREDT     PIC X(05).
             15 DCOMW28I-CCHAVE-ELMTO-DESC   PIC X(45).
             15 DCOMW28I-CESPCE-TITLO-COBR   PIC 9(03).
             15 DCOMW28I-CINDCD-ALIQT-REDZD  PIC X(01).
             15 DCOMW28I-PTX-JURO-DIA        PIC 9(03)V9(5).
             15 DCOMW28I-CINDCD-FREQ-TX      PIC X(02).
             15 DCOMW28I-CGARAN              PIC 9(03).
             15 DCOMW28I-IRSUMO-MEIO-ENTRD   PIC X(15).
             15 DCOMW28I-CCART-NORML-DESC    PIC X(05).
             15 DCOMW28I-ITPO-TX-JURO-DESC   PIC X(30).
             15 DCOMW28I-CSGL-TPO-TX-JURO    PIC X(06).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
