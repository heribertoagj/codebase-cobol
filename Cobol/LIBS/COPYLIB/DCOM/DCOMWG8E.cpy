      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0G8: DB2PRD.TTRASI_TITLO_OPER                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4G8S                                       *
      *    -> FETCH   - DCOM4G8F                                       *
      *    -> INSERT  - DCOM4G8I                                       *
      *    -> UPDATE  - DCOM4G8U                                       *
      *    -> DELETE  - DCOM4G8D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWG8E-HEADER.
          10 DCOMWG8E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG8E'.
          10 DCOMWG8E-TAM-LAYOUT       PIC  9(005) VALUE 01500.
      *
       05 DCOMWG8E-BLOCO-ENTRADA.
          10 DCOMWG8E-INSTRUCAO        PIC  X(010).
      *
          10 DCOMWG8E-ENTRADA.
             15 DCOMWG8E-DANO-OPER-DESC       PIC S9(004) COMP-3.
             15 DCOMWG8E-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
             15 DCOMWG8E-NTITLO-DESC-COML     PIC S9(005) COMP-3.
             15 DCOMWG8E-CBCO-PRODT-COBR      PIC S9(003) COMP-3.
             15 DCOMWG8E-CIDTFD-PRODT-COBR    PIC S9(002) COMP-3.
             15 DCOMWG8E-CNEGOC-COBR          PIC S9(018) COMP-3.
             15 DCOMWG8E-CTITLO-COBR-BCO      PIC S9(018) COMP-3.
             15 DCOMWG8E-CNRO-SEQ-TITLO       PIC S9(003) COMP-3.
             15 DCOMWG8E-NNOTA-PROMS-RURAL    PIC S9(011) COMP-3.
             15 DCOMWG8E-CINDCD-NOTA-PROMS    PIC  X(003).
             15 DCOMWG8E-CTPO-PROTR-RURAL     PIC S9(002) COMP-3.
             15 DCOMWG8E-CPRODT-RURAL         PIC S9(003) COMP-3.
             15 DCOMWG8E-QPRODT-RURAL         PIC S9(009) COMP-3.
             15 DCOMWG8E-CUND-MEDD            PIC S9(003) COMP-3.
             15 DCOMWG8E-CINDCD-PGDOR-JURO    PIC  X(001).
             15 DCOMWG8E-DEMIS-TITLO          PIC  X(010).
             15 DCOMWG8E-DVCTO-TITLO-DESC     PIC  X(010).
             15 DCOMWG8E-VTITLO-DESC-COML     PIC S9(015)V9(002) COMP-3.
             15 DCOMWG8E-CCNPJ-CPF            PIC S9(009) COMP-3.
             15 DCOMWG8E-CFLIAL-CNPJ          PIC S9(005) COMP-3.
             15 DCOMWG8E-CCTRL-CNPJ-CPF       PIC S9(002) COMP-3.
             15 DCOMWG8E-IPSSOA-SACDO         PIC  X(060).
             15 DCOMWG8E-CSEU-NRO-TITLO       PIC  X(015).
             15 DCOMWG8E-CESPCE-TITLO-COBR    PIC S9(003) COMP-3.
             15 DCOMWG8E-CINDCD-INSTR-DESC    PIC S9(001) COMP-3.
             15 DCOMWG8E-CCEP                 PIC S9(005) COMP-3.
             15 DCOMWG8E-CCEP-COMPL           PIC S9(003) COMP-3.
             15 DCOMWG8E-ELOGDR               PIC  X(040).
             15 DCOMWG8E-IBAIRO               PIC  X(040).
             15 DCOMWG8E-IMUN                 PIC  X(030).
             15 DCOMWG8E-CSGL-UF              PIC  X(002).
             15 DCOMWG8E-CSIT-DESC-COML       PIC S9(003) COMP-3.
             15 DCOMWG8E-HSIT-DESC-COML       PIC  X(026).
             15 DCOMWG8E-VABTMT-TITLO-DESC    PIC S9(015)V9(002) COMP-3.
             15 DCOMWG8E-VORPAG-RCBVL         PIC S9(015)V9(002) COMP-3.
             15 DCOMWG8E-NPCELA-DESC-COML     PIC S9(005) COMP-3.
             15 DCOMWG8E-CINDCD-SELEC-DESC    PIC  X(001).
             15 DCOMWG8E-HULT-ATULZ           PIC  X(026).
             15 DCOMWG8E-CFUNC-BDSCO          PIC S9(009) COMP-3.
             15 DCOMWG8E-CTERM                PIC  X(008).
             15 DCOMWG8E-DINCL-REG            PIC  X(0010).
             15 DCOMWG8E-CORIGE-REC           PIC S9(003) COMP-3.
             15 DCOMWG8E-CBCO-DEPOS           PIC S9(003) COMP-3.
             15 DCOMWG8E-CAG-BCRIA-DEPOS      PIC S9(005) COMP-3.
             15 DCOMWG8E-RANLSE-CREDT-CLI     PIC  X(020).
             15 DCOMWG8E-DSOLTC-ANLSE-CREDT   PIC  X(010).
             15 DCOMWG8E-CSIT-ANLSE-CREDT     PIC S9(003) COMP-3.
             15 DCOMWG8E-CINDCD-ISENC-ANLSE   PIC  X(001).
             15 DCOMWG8E-CINDCD-PROTE-TITLO   PIC  X(001).
             15 DCOMWG8E-QDIA-PROTE-TITLO     PIC S9(003) COMP-3.
             15 DCOMWG8E-CBCO-DEB-AUTOM       PIC S9(003) COMP-3.
             15 DCOMWG8E-CAG-DEB-AUTOM        PIC S9(005) COMP-3.
             15 DCOMWG8E-CCTA-DEB-AUTOM       PIC S9(013) COMP-3.
             15 DCOMWG8E-CIDTFD-PRODT-ORIGN   PIC S9(002) COMP-3.
             15 DCOMWG8E-CADM-CATAO-CREDT     PIC S9(009) COMP-3.
             15 DCOMWG8E-CINDCD-VDA-RCBVL     PIC  X(001).
             15 DCOMWG8E-FILLER               PIC  X(100).
      *
          10 DCOMWG8E-BLOCO-VARIAVEL.
             15 DCOMWG8E-V-FILLER             PIC  X(300).
      *
          10 DCOMWG8E-BLOCO-PAGINACAO.
             15 DCOMWG8E-MAX-OCORR            PIC  9(003).
             15 DCOMWG8E-TOTALIZAR            PIC  X(001).
             15 DCOMWG8E-INDICADOR-PAGINACAO  PIC  X(001).
                88 DCOMWG8E-P-INICIAL         VALUE 'I'.
                88 DCOMWG8E-P-PRIMEIRA        VALUE 'P'.
                88 DCOMWG8E-P-SEGUINTE        VALUE 'S'.
                88 DCOMWG8E-P-ANTERIOR        VALUE 'A'.
                88 DCOMWG8E-P-ULTIMA          VALUE 'U'.
      *
          10 DCOMWG8E-BLOCO-RESTART.
             15 DCOMWG8E-CHAVE-INI.
                20 DCOMWG8E-I-DANO-OPER-DESC  PIC S9(004) COMP-3.
                20 DCOMWG8E-I-NSEQ-OPER-DESC  PIC S9(009) COMP-3.
                20 DCOMWG8E-I-NTITLO-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG8E-I-NPCELA-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG8E-I-FILLER          PIC  X(100).
             15 DCOMWG8E-CHAVE-FIM.
                20 DCOMWG8E-F-DANO-OPER-DESC  PIC S9(004) COMP-3.
                20 DCOMWG8E-F-NSEQ-OPER-DESC  PIC S9(009) COMP-3.
                20 DCOMWG8E-F-NTITLO-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG8E-F-NPCELA-DESC-COML
                                              PIC S9(005) COMP-3.
                20 DCOMWG8E-F-FILLER          PIC  X(100).
      *
          10 DCOMWG8E-FILLER                  PIC  X(390).
      *
