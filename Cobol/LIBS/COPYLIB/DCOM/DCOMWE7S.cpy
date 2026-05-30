      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWE7S-HEADER.
          10 DCOMWE7S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE7S'.
          10 DCOMWE7S-TAM-LAYOUT       PIC  9(005) VALUE 21000.
      *
          10 DCOMWE7S-BLOCO-SAIDA.
             15 DCOMWE7S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWE7S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWE7S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWE7S-NSMULA-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWE7S-NTITLO-DESC-COML       PIC S9(005) COMP-3.
                20 DCOMWE7S-CBCO-PRODT-COBR        PIC S9(003) COMP-3.
                20 DCOMWE7S-CIDTFD-PRODT-COBR      PIC S9(002) COMP-3.
                20 DCOMWE7S-CNEGOC-COBR            PIC S9(018) COMP-3.
                20 DCOMWE7S-CTITLO-COBR-BCO        PIC S9(018) COMP-3.
                20 DCOMWE7S-CNRO-SEQ-TITLO         PIC S9(003) COMP-3.
                20 DCOMWE7S-DEMIS-TITLO            PIC  X(010).
                20 DCOMWE7S-DVCTO-TITLO-DESC       PIC  X(010).
                20 DCOMWE7S-VTITLO-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWE7S-CINDCD-SELEC-DESC      PIC  X(001).
                20 DCOMWE7S-HULT-ATULZ             PIC  X(026).
                20 DCOMWE7S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWE7S-CTERM                  PIC  X(008).
                20 DCOMWE7S-DINCL-REG              PIC  X(010).
                20 DCOMWE7S-CADM-CATAO-CREDT       PIC S9(009) COMP-3.
                20 DCOMWE7S-CINDCD-VDA-RCBVL       PIC  X(001).
                20 DCOMWE7S-VORPAG-RCBVL           PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWE7S-IPSSOA-SACDO           PIC  X(060).
                20 DCOMWE7S-CCNPJ-CPF              PIC S9(009) COMP-3.
                20 DCOMWE7S-CFLIAL-CNPJ            PIC S9(005) COMP-3.
                20 DCOMWE7S-CCTRL-CNPJ-CPF         PIC S9(002) COMP-3.
                20 DCOMWE7S-CSEU-NRO-TITLO         PIC  X(015).
                20 DCOMWE7S-CESPCE-TITLO-COBR      PIC S9(003) COMP-3.
                20 DCOMWE7S-CIDTFD-PRODT-ORIGN     PIC S9(002) COMP-3.
                20 DCOMWE7S-NNOTA-PROMS-RURAL      PIC S9(011) COMP-3.
                20 DCOMWE7S-CINDCD-NOTA-PROMS      PIC  X(003).
                20 DCOMWE7S-CTPO-PROTR-RURAL       PIC S9(002) COMP-3.
                20 DCOMWE7S-CPRODT-RURAL           PIC S9(003) COMP-3.
                20 DCOMWE7S-QPRODT-RURAL           PIC S9(009) COMP-3.
                20 DCOMWE7S-CUND-MEDD              PIC S9(003) COMP-3.
                20 DCOMWE7S-CINDCD-PGDOR-JURO      PIC  X(001).
                20 DCOMWE7S-CINDCD-INSTR-DESC      PIC S9(001) COMP-3.
                20 DCOMWE7S-CCEP                   PIC S9(005) COMP-3.
                20 DCOMWE7S-CCEP-COMPL             PIC S9(003) COMP-3.
                20 DCOMWE7S-ELOGDR                 PIC  X(030).
                20 DCOMWE7S-IBAIRO                 PIC  X(040).
                20 DCOMWE7S-IMUN                   PIC  X(030).
                20 DCOMWE7S-CSGL-UF                PIC  X(002).
                20 DCOMWE7S-VABTMT-TITLO-DESC      PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWE7S-CORIGE-REC             PIC S9(003) COMP-3.
                20 DCOMWE7S-CBCO-DEPOS             PIC S9(003) COMP-3.
                20 DCOMWE7S-CAG-BCRIA-DEPOS        PIC S9(005) COMP-3.
                20 DCOMWE7S-RANLSE-CREDT-CLI       PIC  X(020).
                20 DCOMWE7S-DSOLTC-ANLSE-CREDT     PIC  X(010).
                20 DCOMWE7S-CSIT-ANLSE-CREDT       PIC S9(003) COMP-3.
                20 DCOMWE7S-CINDCD-ISENC-ANLSE     PIC  X(001).
                20 DCOMWE7S-CINDCD-PROTE-TITLO     PIC  X(001).
                20 DCOMWE7S-QDIA-PROTE-TITLO       PIC S9(003) COMP-3.
                20 DCOMWE7S-CBCO-DEB-AUTOM         PIC S9(003) COMP-3.
                20 DCOMWE7S-CAG-DEB-AUTOM          PIC S9(005) COMP-3.
                20 DCOMWE7S-CCTA-DEB-AUTOM         PIC S9(013) COMP-3.
      *
             15 DCOMWE7S-FILLER                    PIC  X(474).
