      ******************************************************************
      * NOME BOOK : DCOMW25G - BOOK DE INTERFACE COM SERVICO DCOM425G  *
      * DESCRICAO : CONSULTA INFORMACOES TABELA TTITLO_DESC_COML       *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 27519 BYTES                                        *
      ******************************************************************
           05 DCOMW25G-HEADER.
             10 DCOMW25G-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25G'.
             10 DCOMW25G-TAM-LAYOUT      PIC 9(05) VALUE 27519.
           05 DCOMW25G-REGISTRO.
             10 DCOMW25G-AREA-ENTRADA.
                15 DCOMW25G-E-MAX-OCORR               PIC 9(003).
                15 DCOMW25G-E-DANO-OPER-DESC          PIC 9(004).
                15 DCOMW25G-E-NSEQ-OPER-DESC          PIC 9(009).
             10 DCOMW25G-AREA-PAGINACAO.
                15 DCOMW25G-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW25G-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW25G-I-INICIAL              VALUE 'I'.
                   88 DCOMW25G-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW25G-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW25G-I-ULTIMA               VALUE 'U'.
                15 DCOMW25G-BLOCO-RESTART.
                   20 DCOMW25G-PRI-CHAVE.
                      25 DCOMW25G-P-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25G-P-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25G-P-NTITLO-DESC-COML  PIC 9(005).
                   20 DCOMW25G-ULT-CHAVE.
                      25 DCOMW25G-U-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25G-U-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25G-U-NTITLO-DESC-COML  PIC 9(005).
             10 DCOMW25G-AREA-SAIDA.
                15 DCOMW25G-S-TOT-REG                 PIC 9(003).
                15 DCOMW25G-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW25G-S-DANO-OPER-DESC       PIC 9(004).
                   25 DCOMW25G-S-NSEQ-OPER-DESC       PIC 9(009).
                   25 DCOMW25G-S-NTITLO-DESC-COML     PIC 9(005).
                   25 DCOMW25G-S-CBCO-PRODT-COBR      PIC 9(003).
                   25 DCOMW25G-S-CIDTFD-PRODT-COBR    PIC 9(002).
                   25 DCOMW25G-S-CNEGOC-COBR          PIC 9(018).
                   25 DCOMW25G-S-CTITLO-COBR-BCO      PIC 9(018).
                   25 DCOMW25G-S-CNRO-SEQ-TITLO       PIC 9(003).
                   25 DCOMW25G-S-NNOTA-PROMS-RURAL    PIC 9(011).
                   25 DCOMW25G-S-CINDCD-NOTA-PROMS    PIC X(003).
                   25 DCOMW25G-S-CTPO-PROTR-RURAL     PIC 9(002).
                   25 DCOMW25G-S-CPRODT-RURAL         PIC 9(003).
                   25 DCOMW25G-S-QPRODT-RURAL         PIC 9(009).
                   25 DCOMW25G-S-CUND-MEDD            PIC 9(003).
                   25 DCOMW25G-S-CINDCD-PGDOR-JURO    PIC X(001).
                   25 DCOMW25G-S-DEMIS-TITLO          PIC X(010).
                   25 DCOMW25G-S-DVCTO-TITLO-DESC     PIC X(010).
                   25 DCOMW25G-S-VTITLO-DESC-COML     PIC 9(015)V99.
                   25 DCOMW25G-S-CCNPJ-CPF            PIC 9(009).
                   25 DCOMW25G-S-CFLIAL-CNPJ          PIC 9(005).
                   25 DCOMW25G-S-CCTRL-CNPJ-CPF       PIC 9(002).
                   25 DCOMW25G-S-IPSSOA-SACDO         PIC X(060).
                   25 DCOMW25G-S-CSEU-NRO-TITLO       PIC X(015).
                   25 DCOMW25G-S-CESPCE-TITLO-COBR    PIC 9(003).
                   25 DCOMW25G-S-CINDCD-INSTR-DESC    PIC 9(001).
                   25 DCOMW25G-S-CCEP                 PIC 9(005).
                   25 DCOMW25G-S-CCEP-COMPL           PIC 9(003).
                   25 DCOMW25G-S-ELOGDR               PIC X(040).
                   25 DCOMW25G-S-IBAIRO               PIC X(040).
                   25 DCOMW25G-S-IMUN                 PIC X(030).
                   25 DCOMW25G-S-CSGL-UF              PIC X(002).
                   25 DCOMW25G-S-CSIT-DESC-COML       PIC 9(003).
                   25 DCOMW25G-S-HSIT-DESC-COML       PIC X(026).
                   25 DCOMW25G-S-VABTMT-TITLO-DESC    PIC 9(015)V99.
                   25 DCOMW25G-S-VORPAG-RCBVL         PIC 9(015)V99.
                   25 DCOMW25G-S-NPCELA-DESC-COML     PIC 9(005).
                   25 DCOMW25G-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW25G-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW25G-S-CTERM                PIC X(008).
                   25 DCOMW25G-S-DINCL-REG            PIC X(010).
                   25 DCOMW25G-S-CORIGE-REC           PIC 9(003).
                   25 DCOMW25G-S-CBCO-DEPOS           PIC 9(003).
                   25 DCOMW25G-S-CAG-BCRIA-DEPOS      PIC 9(005).
                   25 DCOMW25G-S-RANLSE-CREDT-CLI     PIC X(020).
                   25 DCOMW25G-S-DSOLTC-ANLSE-CREDT   PIC X(010).
                   25 DCOMW25G-S-CSIT-ANLSE-CREDT     PIC 9(003).
                   25 DCOMW25G-S-CINDCD-ISENC-ANLSE   PIC X(001).
                   25 DCOMW25G-S-CINDCD-PROTE-TITLO   PIC X(001).
                   25 DCOMW25G-S-QDIA-PROTE-TITLO     PIC 9(003).
                   25 DCOMW25G-S-CBCO-DEB-AUTOM       PIC 9(003).
                   25 DCOMW25G-S-CAG-DEB-AUTOM        PIC 9(005).
                   25 DCOMW25G-S-CCTA-DEB-AUTOM       PIC 9(013).
                   25 DCOMW25G-S-CIDTFD-PRODT-ORIGN   PIC 9(002).
                   25 DCOMW25G-S-CADM-CATAO-CREDT     PIC 9(009).
                   25 DCOMW25G-S-CINDCD-VDA-RCBVL     PIC X(001).
