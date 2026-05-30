      ******************************************************************
      * NOME BOOK : DCOMW25P - BOOK DE INTERFACE COM SERVICO DCOM425P  *
      * DESCRICAO : CONSULTA INFORMACOES DO CONTRATO                   *
      * DATA      : 17/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 591 BYTES                                          *
      *----------------------------------------------------------------*
      * DCOMW25P-E-TPO-PESQUISA = 1 - PESQUISA TOTAL                   *
      *                           2 - PESQUISA PARCIAL-06              *
      ******************************************************************
       05 DCOMW25P-HEADER.
         10 DCOMW25P-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25P'.
         10 DCOMW25P-TAM-LAYOUT        PIC 9(05)   VALUE 591.
       05 DCOMW25P-REGISTRO.
         10 DCOMW25P-BLOCO-ENTRADA.
           15 DCOMW25P-E-TPO-PESQUISA              PIC 9(01).
           15 DCOMW25P-E-CCONVE-CLI-DESC           PIC 9(09).
           15 DCOMW25P-E-CVRSAO-CONVE-CLI          PIC 9(03).
           15 DCOMW25P-E-CSIT-DESC-COML            PIC 9(03).
         10 DCOMW25P-BLOCO-SAIDA.
           15 DCOMW25P-S-CCONVE-CLI-DESC           PIC 9(09).
           15 DCOMW25P-S-CVRSAO-CONVE-CLI          PIC 9(03).
           15 DCOMW25P-S-CBCO                      PIC 9(03).
           15 DCOMW25P-S-CAG-BCRIA                 PIC 9(05).
           15 DCOMW25P-S-CCTA-BCRIA-CLI            PIC 9(13).
           15 DCOMW25P-S-CPOSTO-SERVC              PIC 9(05).
           15 DCOMW25P-S-CBCO-OPER-CONVE           PIC 9(03).
           15 DCOMW25P-S-CAG-OPER-CONVE            PIC 9(05).
           15 DCOMW25P-S-CINDCD-RENTB-NEGOC        PIC X(01).
           15 DCOMW25P-S-CPRODT                    PIC 9(03).
           15 DCOMW25P-S-CSPROD-DESC-COML          PIC 9(03).
           15 DCOMW25P-S-DINCL-CONVE-CLI           PIC X(10).
           15 DCOMW25P-S-DINIC-VGCIA-CONVE         PIC X(10).
           15 DCOMW25P-S-DVCTO-CONVE-DESC          PIC X(10).
           15 DCOMW25P-S-TCONVE-DESC-COML          PIC 9(05).
           15 DCOMW25P-S-CINDCD-PRZ-CONVE          PIC X(02).
           15 DCOMW25P-S-VLIM-CONVE-CLI            PIC 9(15)V9(2).
           15 DCOMW25P-S-VUTLZD-CONVE-CLI          PIC 9(15)V9(2).
           15 DCOMW25P-S-VDISPN-CONVE-CLI          PIC 9(15)V9(2).
           15 DCOMW25P-S-HATULZ-SDO-CONVE          PIC X(26).
           15 DCOMW25P-S-CINDCD-CONVE-ROTVO        PIC X(01).
           15 DCOMW25P-S-CTPO-COBR-TAC             PIC 9(03).
           15 DCOMW25P-S-CIDTFD-FIXO-TAC           PIC 9(05).
           15 DCOMW25P-S-CIDTFD-PERC-TAC           PIC 9(05).
           15 DCOMW25P-S-CIDTFD-MIN-TAC            PIC 9(05).
           15 DCOMW25P-S-CIDTFD-MAX-TAC            PIC 9(05).
           15 DCOMW25P-S-CINDCD-TARIF-REG          PIC X(01).
           15 DCOMW25P-S-CINDCD-PGDOR-JURO         PIC X(01).
           15 DCOMW25P-S-CINDCD-RTEIO-BNEFC        PIC X(01).
           15 DCOMW25P-S-CINDCD-LIBRC-ANTCP        PIC X(01).
           15 DCOMW25P-S-PMAX-LIBRC-ANTCP          PIC 9(03)V9(3).
           15 DCOMW25P-S-PMAX-ANTVL-VCTO           PIC 9(03)V9(3).
           15 DCOMW25P-S-TMIN-VCTO-TITLO           PIC 9(05).
           15 DCOMW25P-S-TMAX-VCTO-TITLO           PIC 9(05).
           15 DCOMW25P-S-CINDCD-PRZ-VCTO           PIC X(02).
           15 DCOMW25P-S-CREGRA-GERC-MORA          PIC 9(03).
           15 DCOMW25P-S-CREGRA-GERC-CRATS         PIC 9(03).
           15 DCOMW25P-S-CINDCD-TX-SPROD           PIC X(01).
           15 DCOMW25P-S-CTPO-TX-JURO-DESC         PIC 9(03).
           15 DCOMW25P-S-CIDTFD-TX-JURO            PIC 9(05).
           15 DCOMW25P-S-PINDIC-ECONM-JURO         PIC 9(03)V9(3).
           15 DCOMW25P-S-PTX-INDIC-ECONM           PIC 9(03)V9(3).
           15 DCOMW25P-S-CREGRA-COMIS-CONVE        PIC 9(03).
           15 DCOMW25P-S-VCALC-COMIS-CONVE         PIC 9(15)V9(2).
           15 DCOMW25P-S-PCALC-COMIS-CONVE         PIC 9(03)V9(2).
           15 DCOMW25P-S-DDIA-PGTO-COMIS           PIC 9(02).
           15 DCOMW25P-S-CINDCD-CONVE-ESPCL        PIC X(01).
           15 DCOMW25P-S-TRECPC-ARQ-CONVE          PIC 9(03).
           15 DCOMW25P-S-CINDCD-PRZ-RECPC          PIC X(02).
           15 DCOMW25P-S-TARQ-DEB-CONVE            PIC 9(03).
           15 DCOMW25P-S-CINDCD-PRZ-ARQ-DEB        PIC X(02).
           15 DCOMW25P-S-CINDCD-CONSL-DEB          PIC X(01).
           15 DCOMW25P-S-CINDCD-CONSL-EXTRT        PIC X(01).
           15 DCOMW25P-S-CINDCD-MEIO-COMUN         PIC X(01).
           15 DCOMW25P-S-CDDD-FAX-CONVN            PIC X(04).
           15 DCOMW25P-S-CFONE-FAX-CONVN           PIC 9(08).
           15 DCOMW25P-S-CRMAL-FAX-CONVN           PIC X(06).
           15 DCOMW25P-S-EEMAIL-CLI-CONVN          PIC X(70).
           15 DCOMW25P-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW25P-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW25P-S-DULT-UTILZ-CONVE          PIC X(10).
           15 DCOMW25P-S-DULT-ADTTO-CONVE          PIC X(10).
           15 DCOMW25P-S-CINDCD-MOTVO-ADTTO        PIC X(01).
           15 DCOMW25P-S-CCNPJ-CPF                 PIC 9(09).
           15 DCOMW25P-S-CFLIAL-CNPJ               PIC 9(05).
           15 DCOMW25P-S-CCTRL-CNPJ-CPF            PIC 9(02).
           15 DCOMW25P-S-CCLUB                     PIC 9(10).
           15 DCOMW25P-S-HULT-ATULZ                PIC X(26).
           15 DCOMW25P-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25P-S-CTERM                     PIC X(08).
           15 DCOMW25P-S-CINDCD-PROTE-TITLO        PIC X(01).
           15 DCOMW25P-S-QDIA-PROTE-TITLO          PIC 9(03).
           15 DCOMW25P-S-CCHAVE-ELMTO-DESC         PIC X(45).
           15 DCOMW25P-S-PTAC-FIXA-CONVE           PIC 9(03)V9(2).
           15 DCOMW25P-S-CINDCD-PRZ-MAX            PIC X(02).
           15 DCOMW25P-S-CINDCD-DIA-PROTE          PIC X(02).
           15 DCOMW25P-S-CREGRA-GERC-CONS          PIC 9(03).
           15 DCOMW25P-S-QDIA-OPER-ATIVO           PIC 9(04).
           15 DCOMW25P-S-CINDCD-FREQ-ESTOQ         PIC X(02).
           15 DCOMW25P-S-CINDCD-OPER-ATRSO         PIC X(01).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
