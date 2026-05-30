      ******************************************************************
      * NOME BOOK : DCOMW25H - BOOK DE INTERFACE COM SERVICO DCOM425H  *
      * DESCRICAO : CONSULTA INFORMACOES TABELA THIST_TITLO_DESC       *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 27969 BYTES                                        *
      ******************************************************************
           05 DCOMW25H-HEADER.
             10 DCOMW25H-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25H'.
             10 DCOMW25H-TAM-LAYOUT      PIC 9(05) VALUE 27969.
           05 DCOMW25H-REGISTRO.
             10 DCOMW25H-AREA-ENTRADA.
                15 DCOMW25H-E-MAX-OCORR               PIC 9(003).
                15 DCOMW25H-E-DANO-OPER-DESC          PIC 9(004).
                15 DCOMW25H-E-NSEQ-OPER-DESC          PIC 9(009).
             10 DCOMW25H-AREA-PAGINACAO.
                15 DCOMW25H-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW25H-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW25H-I-INICIAL              VALUE 'I'.
                   88 DCOMW25H-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW25H-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW25H-I-ULTIMA               VALUE 'U'.
                15 DCOMW25H-BLOCO-RESTART.
                   20 DCOMW25H-PRI-CHAVE.
                      25 DCOMW25H-P-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25H-P-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25H-P-NTITLO-DESC-COML  PIC 9(005).
                   20 DCOMW25H-ULT-CHAVE.
                      25 DCOMW25H-U-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25H-U-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25H-U-NTITLO-DESC-COML  PIC 9(005).
             10 DCOMW25H-AREA-SAIDA.
                15 DCOMW25H-S-TOT-REG                 PIC 9(003).
                15 DCOMW25H-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW25H-S-DANO-OPER-DESC       PIC 9(004).
                   25 DCOMW25H-S-NSEQ-OPER-DESC       PIC 9(009).
                   25 DCOMW25H-S-NTITLO-DESC-COML     PIC 9(005).
                   25 DCOMW25H-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW25H-S-NPSSOA-DESC-COML     PIC 9(009).
                   25 DCOMW25H-S-CBCO-PRODT-COBR      PIC 9(003).
                   25 DCOMW25H-S-CIDTFD-PRODT-COBR    PIC 9(002).
                   25 DCOMW25H-S-CNEGOC-COBR          PIC 9(018).
                   25 DCOMW25H-S-CTITLO-COBR-BCO      PIC 9(018).
                   25 DCOMW25H-S-CNRO-SEQ-TITLO       PIC 9(003).
                   25 DCOMW25H-S-NNOTA-PROMS-RURAL    PIC 9(011).
                   25 DCOMW25H-S-CINDCD-NOTA-PROMS    PIC X(003).
                   25 DCOMW25H-S-CTPO-PROTR-RURAL     PIC 9(002).
                   25 DCOMW25H-S-CPRODT-RURAL         PIC 9(003).
                   25 DCOMW25H-S-QPRODT-RURAL         PIC 9(009).
                   25 DCOMW25H-S-CUND-MEDD            PIC 9(003).
                   25 DCOMW25H-S-CINDCD-PGDOR-JURO    PIC X(001).
                   25 DCOMW25H-S-DEMIS-TITLO          PIC X(010).
                   25 DCOMW25H-S-DVCTO-TITLO-DESC     PIC X(010).
                   25 DCOMW25H-S-VTITLO-DESC-COML     PIC 9(015)V99.
                   25 DCOMW25H-S-CCNPJ-CPF            PIC 9(009).
                   25 DCOMW25H-S-CFLIAL-CNPJ          PIC 9(005).
                   25 DCOMW25H-S-CCTRL-CNPJ-CPF       PIC 9(002).
                   25 DCOMW25H-S-IPSSOA-SACDO         PIC X(060).
                   25 DCOMW25H-S-CSEU-NRO-TITLO       PIC X(015).
                   25 DCOMW25H-S-CESPCE-TITLO-COBR    PIC 9(003).
                   25 DCOMW25H-S-CINDCD-INSTR-DESC    PIC 9(001).
                   25 DCOMW25H-S-CCEP                 PIC 9(005).
                   25 DCOMW25H-S-CCEP-COMPL           PIC 9(003).
                   25 DCOMW25H-S-ELOGDR               PIC X(040).
                   25 DCOMW25H-S-IBAIRO               PIC X(040).
                   25 DCOMW25H-S-IMUN                 PIC X(030).
                   25 DCOMW25H-S-CSGL-UF              PIC X(002).
                   25 DCOMW25H-S-CSIT-DESC-COML       PIC 9(003).
                   25 DCOMW25H-S-HSIT-DESC-COML       PIC X(026).
                   25 DCOMW25H-S-VABTMT-TITLO-DESC    PIC 9(015)V99.
                   25 DCOMW25H-S-VORPAG-RCBVL         PIC 9(015)V99.
                   25 DCOMW25H-S-NPCELA-DESC-COML     PIC 9(005).
                   25 DCOMW25H-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW25H-S-CTERM                PIC X(008).
                   25 DCOMW25H-S-DINCL-REG            PIC X(010).
                   25 DCOMW25H-S-CORIGE-REC           PIC 9(003).
                   25 DCOMW25H-S-CBCO-DEPOS           PIC 9(003).
                   25 DCOMW25H-S-CAG-BCRIA-DEPOS      PIC 9(005).
                   25 DCOMW25H-S-RANLSE-CREDT-CLI     PIC X(020).
                   25 DCOMW25H-S-DSOLTC-ANLSE-CREDT   PIC X(010).
                   25 DCOMW25H-S-CSIT-ANLSE-CREDT     PIC 9(003).
                   25 DCOMW25H-S-CINDCD-ISENC-ANLSE   PIC X(001).
                   25 DCOMW25H-S-CINDCD-PROTE-TITLO   PIC X(001).
                   25 DCOMW25H-S-QDIA-PROTE-TITLO     PIC 9(003).
                   25 DCOMW25H-S-CBCO-DEB-AUTOM       PIC 9(003).
                   25 DCOMW25H-S-CAG-DEB-AUTOM        PIC 9(005).
                   25 DCOMW25H-S-CCTA-DEB-AUTOM       PIC 9(013).
                   25 DCOMW25H-S-CIDTFD-PRODT-ORIGN   PIC 9(002).
                   25 DCOMW25H-S-CADM-CATAO-CREDT     PIC 9(009).
                   25 DCOMW25H-S-CINDCD-VDA-RCBVL     PIC X(001).
