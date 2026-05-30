      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0E7: DB2PRD.TSMULA_TITLO_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4E7S                                       *
      *    -> FETCH   - DCOM4E7F                                       *
      *    -> INSERT  - DCOM4E7I                                       *
      *    -> UPDATE  - DCOM4E7U                                       *
      *    -> DELETE  - DCOM4E7D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWE7E-HEADER.
          10 DCOMWE7E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE7E'.
          10 DCOMWE7E-TAM-LAYOUT       PIC  9(005) VALUE 01100.
      *
       05 DCOMWE7E-BLOCO-ENTRADA.
          10 DCOMWE7E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWE7E-ENTRADA.
             15 DCOMWE7E-NSMULA-OPER-DESC          PIC S9(009) COMP-3.
             15 DCOMWE7E-NTITLO-DESC-COML          PIC S9(005) COMP-3.
             15 DCOMWE7E-CBCO-PRODT-COBR           PIC S9(003) COMP-3.
             15 DCOMWE7E-CIDTFD-PRODT-COBR         PIC S9(002) COMP-3.
             15 DCOMWE7E-CNEGOC-COBR               PIC S9(018) COMP-3.
             15 DCOMWE7E-CTITLO-COBR-BCO           PIC S9(018) COMP-3.
             15 DCOMWE7E-CNRO-SEQ-TITLO            PIC S9(003) COMP-3.
             15 DCOMWE7E-DEMIS-TITLO               PIC  X(010).
             15 DCOMWE7E-DVCTO-TITLO-DESC          PIC  X(010).
             15 DCOMWE7E-VTITLO-DESC-COML          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWE7E-CINDCD-SELEC-DESC         PIC  X(001).
             15 DCOMWE7E-HULT-ATULZ                PIC  X(026).
             15 DCOMWE7E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWE7E-CTERM                     PIC  X(008).
             15 DCOMWE7E-DINCL-REG                 PIC  X(010).
             15 DCOMWE7E-CADM-CATAO-CREDT          PIC S9(009) COMP-3.
             15 DCOMWE7E-CINDCD-VDA-RCBVL          PIC  X(001).
             15 DCOMWE7E-VORPAG-RCBVL              PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWE7E-IPSSOA-SACDO              PIC  X(060).
             15 DCOMWE7E-CCNPJ-CPF                 PIC S9(009) COMP-3.
             15 DCOMWE7E-CFLIAL-CNPJ               PIC S9(005) COMP-3.
             15 DCOMWE7E-CCTRL-CNPJ-CPF            PIC S9(002) COMP-3.
             15 DCOMWE7E-CSEU-NRO-TITLO            PIC  X(015).
             15 DCOMWE7E-CESPCE-TITLO-COBR         PIC S9(003) COMP-3.
             15 DCOMWE7E-CIDTFD-PRODT-ORIGN        PIC S9(002) COMP-3.
             15 DCOMWE7E-NNOTA-PROMS-RURAL         PIC S9(011) COMP-3.
             15 DCOMWE7E-CINDCD-NOTA-PROMS         PIC  X(003).
             15 DCOMWE7E-CTPO-PROTR-RURAL          PIC S9(002) COMP-3.
             15 DCOMWE7E-CPRODT-RURAL              PIC S9(003) COMP-3.
             15 DCOMWE7E-QPRODT-RURAL              PIC S9(009) COMP-3.
             15 DCOMWE7E-CUND-MEDD                 PIC S9(003) COMP-3.
             15 DCOMWE7E-CINDCD-PGDOR-JURO         PIC  X(001).
             15 DCOMWE7E-CINDCD-INSTR-DESC         PIC S9(001) COMP-3.
             15 DCOMWE7E-CCEP                      PIC S9(005) COMP-3.
             15 DCOMWE7E-CCEP-COMPL                PIC S9(003) COMP-3.
             15 DCOMWE7E-ELOGDR                    PIC  X(030).
             15 DCOMWE7E-IBAIRO                    PIC  X(040).
             15 DCOMWE7E-IMUN                      PIC  X(030).
             15 DCOMWE7E-CSGL-UF                   PIC  X(002).
             15 DCOMWE7E-VABTMT-TITLO-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWE7E-CORIGE-REC                PIC S9(003) COMP-3.
             15 DCOMWE7E-CBCO-DEPOS                PIC S9(003) COMP-3.
             15 DCOMWE7E-CAG-BCRIA-DEPOS           PIC S9(005) COMP-3.
             15 DCOMWE7E-RANLSE-CREDT-CLI          PIC  X(020).
             15 DCOMWE7E-DSOLTC-ANLSE-CREDT        PIC  X(010).
             15 DCOMWE7E-CSIT-ANLSE-CREDT          PIC S9(003) COMP-3.
             15 DCOMWE7E-CINDCD-ISENC-ANLSE        PIC  X(001).
             15 DCOMWE7E-CINDCD-PROTE-TITLO        PIC  X(001).
             15 DCOMWE7E-QDIA-PROTE-TITLO          PIC S9(003) COMP-3.
             15 DCOMWE7E-CBCO-DEB-AUTOM            PIC S9(003) COMP-3.
             15 DCOMWE7E-CAG-DEB-AUTOM             PIC S9(005) COMP-3.
             15 DCOMWE7E-CCTA-DEB-AUTOM            PIC S9(013) COMP-3.

          10 DCOMWE7E-BLOCO-PAGINACAO.
             15 DCOMWE7E-MAX-OCORR                 PIC  9(003).
             15 DCOMWE7E-TOTALIZAR                 PIC  X(001).
             15 DCOMWE7E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWE7E-P-INICIAL              VALUE 'I'.
                88 DCOMWE7E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWE7E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWE7E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWE7E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWE7E-BLOCO-RESTART.
             15 DCOMWE7E-CHAVE-INI.
                20 DCOMWE7E-I-NSMULA-OPER-DESC     PIC S9(009) COMP-3.
                20 DCOMWE7E-I-NTITLO-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWE7E-I-DVCTO-TITLO-DESC     PIC  X(010).
                20 FILLER                          PIC  X(190).
             15 DCOMWE7E-CHAVE-FIM.
                20 DCOMWE7E-F-NSMULA-OPER-DESC     PIC S9(009) COMP-3.
                20 DCOMWE7E-F-NTITLO-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWE7E-F-DVCTO-TITLO-DESC     PIC  X(010).
                20 FILLER                          PIC  X(190).
      *
          10 DCOMWE7E-FILLER                       PIC  X(246).
      *
