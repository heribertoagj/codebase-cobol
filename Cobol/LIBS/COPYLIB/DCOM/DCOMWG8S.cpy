      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWG8S-HEADER.
          10 DCOMWG8S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG8S'.
          10 DCOMWG8S-TAM-LAYOUT       PIC  9(005) VALUE 24000.
      *
          10 DCOMWG8S-BLOCO-SAIDA.
             15 DCOMWG8S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWG8S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWG8S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWG8S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWG8S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWG8S-NTITLO-DESC-COML       PIC S9(005) COMP-3.
                20 DCOMWG8S-CBCO-PRODT-COBR        PIC S9(003) COMP-3.
                20 DCOMWG8S-CIDTFD-PRODT-COBR      PIC S9(002) COMP-3.
                20 DCOMWG8S-CNEGOC-COBR            PIC S9(018) COMP-3.
                20 DCOMWG8S-CTITLO-COBR-BCO        PIC S9(018) COMP-3.
                20 DCOMWG8S-CNRO-SEQ-TITLO         PIC S9(003) COMP-3.
                20 DCOMWG8S-NNOTA-PROMS-RURAL      PIC S9(011) COMP-3.
                20 DCOMWG8S-CINDCD-NOTA-PROMS      PIC  X(003).
                20 DCOMWG8S-CTPO-PROTR-RURAL       PIC S9(002) COMP-3.
                20 DCOMWG8S-CPRODT-RURAL           PIC S9(003) COMP-3.
                20 DCOMWG8S-QPRODT-RURAL           PIC S9(009) COMP-3.
                20 DCOMWG8S-CUND-MEDD              PIC S9(003) COMP-3.
                20 DCOMWG8S-CINDCD-PGDOR-JURO      PIC  X(001).
                20 DCOMWG8S-DEMIS-TITLO            PIC  X(010).
                20 DCOMWG8S-DVCTO-TITLO-DESC       PIC  X(010).
                20 DCOMWG8S-VTITLO-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG8S-CCNPJ-CPF              PIC S9(009) COMP-3.
                20 DCOMWG8S-CFLIAL-CNPJ            PIC S9(005) COMP-3.
                20 DCOMWG8S-CCTRL-CNPJ-CPF         PIC S9(002) COMP-3.
                20 DCOMWG8S-IPSSOA-SACDO           PIC  X(060).
                20 DCOMWG8S-CSEU-NRO-TITLO         PIC  X(015).
                20 DCOMWG8S-CESPCE-TITLO-COBR      PIC S9(003) COMP-3.
                20 DCOMWG8S-CINDCD-INSTR-DESC      PIC S9(001) COMP-3.
                20 DCOMWG8S-CCEP                   PIC S9(005) COMP-3.
                20 DCOMWG8S-CCEP-COMPL             PIC S9(003) COMP-3.
                20 DCOMWG8S-ELOGDR                 PIC  X(040).
                20 DCOMWG8S-IBAIRO                 PIC  X(040).
                20 DCOMWG8S-IMUN                   PIC  X(030).
                20 DCOMWG8S-CSGL-UF                PIC  X(002).
                20 DCOMWG8S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWG8S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWG8S-VABTMT-TITLO-DESC      PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG8S-VORPAG-RCBVL           PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG8S-NPCELA-DESC-COML       PIC S9(005) COMP-3.
                20 DCOMWG8S-CINDCD-SELEC-DESC      PIC  X(001).
                20 DCOMWG8S-HULT-ATULZ             PIC  X(026).
                20 DCOMWG8S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWG8S-CTERM                  PIC  X(008).
                20 DCOMWG8S-DINCL-REG              PIC  X(0010).
                20 DCOMWG8S-CORIGE-REC             PIC S9(003) COMP-3.
                20 DCOMWG8S-CBCO-DEPOS             PIC S9(003) COMP-3.
                20 DCOMWG8S-CAG-BCRIA-DEPOS        PIC S9(005) COMP-3.
                20 DCOMWG8S-RANLSE-CREDT-CLI       PIC  X(020).
                20 DCOMWG8S-DSOLTC-ANLSE-CREDT     PIC  X(010).
                20 DCOMWG8S-CSIT-ANLSE-CREDT       PIC S9(003) COMP-3.
                20 DCOMWG8S-CINDCD-ISENC-ANLSE     PIC  X(001).
                20 DCOMWG8S-CINDCD-PROTE-TITLO     PIC X(001).
                20 DCOMWG8S-QDIA-PROTE-TITLO       PIC S9(003) COMP-3.
                20 DCOMWG8S-CBCO-DEB-AUTOM         PIC S9(003) COMP-3.
                20 DCOMWG8S-CAG-DEB-AUTOM          PIC S9(005) COMP-3.
                20 DCOMWG8S-CCTA-DEB-AUTOM         PIC S9(013) COMP-3.
                20 DCOMWG8S-CIDTFD-PRODT-ORIGN     PIC S9(002) COMP-3.
                20 DCOMWG8S-CADM-CATAO-CREDT       PIC S9(009) COMP-3.
                20 DCOMWG8S-CINDCD-VDA-RCBVL       PIC  X(001).

             15 DCOMWG8S-FILLER                    PIC  X(1274).
      *
      *================================================================*
