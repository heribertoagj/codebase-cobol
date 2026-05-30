      ******************************************************************
      * NOME BOOK : DCOMW25F - BOOK DE INTERFACE COM SERVICO DCOM425F  *
      * DESCRICAO : CONSULTA INFORMACOES TABELA TTRASI_TITLO_OPER      *
      * DATA      : 17/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 27596 BYTES                                        *
      ******************************************************************
      * DCOMW24F-E-TPO-PESQUISA = 1 - PESQUISA ORIGINAL PARCIAL-02/12  *
      *                           2 - PESQUISA ORIGINAL PARCIAL-46/56  *
      ******************************************************************
           05 DCOMW25F-HEADER.
             10 DCOMW25F-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25F'.
             10 DCOMW25F-TAM-LAYOUT      PIC 9(05) VALUE 27596.
           05 DCOMW25F-REGISTRO.
             10 DCOMW25F-AREA-ENTRADA.
                15 DCOMW25F-E-TPO-PESQUISA            PIC 9(002).
                15 DCOMW25F-E-MAX-OCORR               PIC 9(003).
                15 DCOMW25F-E-DANO-OPER-DESC          PIC 9(004).
                15 DCOMW25F-E-NSEQ-OPER-DESC          PIC 9(009).
                15 DCOMW25F-E-NTITLO-DESC-COML        PIC 9(005).
             10 DCOMW25F-AREA-PAGINACAO.
                15 DCOMW25F-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW25F-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW25F-I-INICIAL              VALUE 'I'.
                   88 DCOMW25F-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW25F-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW25F-I-ULTIMA               VALUE 'U'.
                15 DCOMW25F-BLOCO-RESTART.
                   20 DCOMW25F-PRI-CHAVE.
                      25 DCOMW25F-P-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25F-P-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25F-P-NTITLO-DESC-COML  PIC 9(005).
                      25 DCOMW25F-P-DVCTO-TITLO-DESC  PIC X(010).
                   20 DCOMW25F-ULT-CHAVE.
                      25 DCOMW25F-U-DANO-OPER-DESC    PIC 9(004).
                      25 DCOMW25F-U-NSEQ-OPER-DESC    PIC 9(009).
                      25 DCOMW25F-U-NTITLO-DESC-COML  PIC 9(005).
                      25 DCOMW25F-U-DVCTO-TITLO-DESC  PIC X(010).
             10 DCOMW25F-AREA-SAIDA.
                15 DCOMW25F-S-TOT-REG                 PIC 9(003).
                15 DCOMW25F-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW25F-S-DANO-OPER-DESC       PIC 9(004).
                   25 DCOMW25F-S-NSEQ-OPER-DESC       PIC 9(009).
                   25 DCOMW25F-S-NTITLO-DESC-COML     PIC 9(005).
                   25 DCOMW25F-S-CBCO-PRODT-COBR      PIC 9(003).
                   25 DCOMW25F-S-CIDTFD-PRODT-COBR    PIC 9(002).
                   25 DCOMW25F-S-CNEGOC-COBR          PIC 9(018).
                   25 DCOMW25F-S-CTITLO-COBR-BCO      PIC 9(018).
                   25 DCOMW25F-S-CNRO-SEQ-TITLO       PIC 9(003).
                   25 DCOMW25F-S-NNOTA-PROMS-RURAL    PIC 9(011).
                   25 DCOMW25F-S-CINDCD-NOTA-PROMS    PIC X(003).
                   25 DCOMW25F-S-CTPO-PROTR-RURAL     PIC 9(002).
                   25 DCOMW25F-S-CPRODT-RURAL         PIC 9(003).
                   25 DCOMW25F-S-QPRODT-RURAL         PIC 9(009).
                   25 DCOMW25F-S-CUND-MEDD            PIC 9(003).
                   25 DCOMW25F-S-CINDCD-PGDOR-JURO    PIC X(001).
                   25 DCOMW25F-S-DEMIS-TITLO          PIC X(010).
                   25 DCOMW25F-S-DVCTO-TITLO-DESC     PIC X(010).
                   25 DCOMW25F-S-VTITLO-DESC-COML     PIC 9(015)V99.
                   25 DCOMW25F-S-CCNPJ-CPF            PIC 9(009).
                   25 DCOMW25F-S-CFLIAL-CNPJ          PIC 9(005).
                   25 DCOMW25F-S-CCTRL-CNPJ-CPF       PIC 9(002).
                   25 DCOMW25F-S-IPSSOA-SACDO         PIC X(060).
                   25 DCOMW25F-S-CSEU-NRO-TITLO       PIC X(015).
                   25 DCOMW25F-S-CESPCE-TITLO-COBR    PIC 9(003).
                   25 DCOMW25F-S-CINDCD-INSTR-DESC    PIC 9(001).
                   25 DCOMW25F-S-CCEP                 PIC 9(005).
                   25 DCOMW25F-S-CCEP-COMPL           PIC 9(003).
                   25 DCOMW25F-S-ELOGDR               PIC X(040).
                   25 DCOMW25F-S-IBAIRO               PIC X(040).
                   25 DCOMW25F-S-IMUN                 PIC X(030).
                   25 DCOMW25F-S-CSGL-UF              PIC X(002).
                   25 DCOMW25F-S-CSIT-DESC-COML       PIC 9(003).
                   25 DCOMW25F-S-HSIT-DESC-COML       PIC X(026).
                   25 DCOMW25F-S-VABTMT-TITLO-DESC    PIC 9(015)V99.
                   25 DCOMW25F-S-VORPAG-RCBVL         PIC 9(015)V99.
                   25 DCOMW25F-S-NPCELA-DESC-COML     PIC 9(005).
                   25 DCOMW25F-S-CINDCD-SELEC-DESC    PIC X(001).
                   25 DCOMW25F-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW25F-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW25F-S-CTERM                PIC X(008).
                   25 DCOMW25F-S-DINCL-REG            PIC X(010).
                   25 DCOMW25F-S-CORIGE-REC           PIC 9(003).
                   25 DCOMW25F-S-CBCO-DEPOS           PIC 9(003).
                   25 DCOMW25F-S-CAG-BCRIA-DEPOS      PIC 9(005).
                   25 DCOMW25F-S-RANLSE-CREDT-CLI     PIC X(020).
                   25 DCOMW25F-S-DSOLTC-ANLSE-CREDT   PIC X(010).
                   25 DCOMW25F-S-CSIT-ANLSE-CREDT     PIC 9(003).
                   25 DCOMW25F-S-CINDCD-ISENC-ANLSE   PIC X(001).
                   25 DCOMW25F-S-CINDCD-PROTE-TITLO   PIC X(001).
                   25 DCOMW25F-S-QDIA-PROTE-TITLO     PIC 9(003).
                   25 DCOMW25F-S-CBCO-DEB-AUTOM       PIC 9(003).
                   25 DCOMW25F-S-CAG-DEB-AUTOM        PIC 9(005).
                   25 DCOMW25F-S-CCTA-DEB-AUTOM       PIC 9(013).
                   25 DCOMW25F-S-CIDTFD-PRODT-ORIGN   PIC 9(002).
                   25 DCOMW25F-S-CADM-CATAO-CREDT     PIC 9(009).
                   25 DCOMW25F-S-CINDCD-VDA-RCBVL     PIC X(001).
