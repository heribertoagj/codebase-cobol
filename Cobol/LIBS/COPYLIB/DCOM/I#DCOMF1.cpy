      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F1 - DB2PRD.TTITLO_DESC_COML       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F1-ARGUMENTOS-ENTRADA.

       05  F1-COLUNAS-DA-TABELA.
           10  F1-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
           10  F1-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
           10  F1-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
           10  F1-CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
           10  F1-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
           10  F1-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
           10  F1-CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
           10  F1-CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
           10  F1-NNOTA-PROMS-RURAL    PIC S9(11)V USAGE COMP-3.
           10  F1-CINDCD-NOTA-PROMS    PIC X(3).
           10  F1-CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
           10  F1-CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
           10  F1-QPRODT-RURAL         PIC S9(9)V USAGE COMP-3.
           10  F1-CUND-MEDD            PIC S9(3)V USAGE COMP-3.
           10  F1-CINDCD-PGDOR-JURO    PIC X(1).
           10  F1-DEMIS-TITLO          PIC X(10).
           10  F1-DVCTO-TITLO-DESC     PIC X(10).
           10  F1-VTITLO-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
           10  F1-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
           10  F1-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
           10  F1-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
           10  F1-IPSSOA-SACDO         PIC X(60).
           10  F1-CSEU-NRO-TITLO       PIC X(15).
           10  F1-CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
           10  F1-CINDCD-INSTR-DESC    PIC S9(1)V USAGE COMP-3.
           10  F1-CCEP                 PIC S9(5)V USAGE COMP-3.
           10  F1-CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
           10  F1-ELOGDR               PIC X(40).
           10  F1-IBAIRO               PIC X(40).
           10  F1-IMUN                 PIC X(30).
           10  F1-CSGL-UF              PIC X(2).
           10  F1-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10  F1-HSIT-DESC-COML       PIC X(26).
           10  F1-VABTMT-TITLO-DESC    PIC S9(15)V9(2) USAGE COMP-3.
           10  F1-VORPAG-RCBVL         PIC S9(15)V9(2) USAGE COMP-3.
           10  F1-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
           10  F1-HULT-ATULZ           PIC X(26).
           10  F1-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  F1-CTERM                PIC X(8).
           10  F1-DINCL-REG            PIC X(10).
           10  F1-CORIGE-REC           PIC S9(3)V USAGE COMP-3.
           10  F1-CBCO-DEPOS           PIC S9(3)V USAGE COMP-3.
           10  F1-CAG-BCRIA-DEPOS      PIC S9(5)V USAGE COMP-3.
           10  F1-RANLSE-CREDT-CLI     PIC X(20).
           10  F1-DSOLTC-ANLSE-CREDT   PIC X(10).
           10  F1-CSIT-ANLSE-CREDT     PIC S9(3)V USAGE COMP-3.
           10  F1-CINDCD-ISENC-ANLSE   PIC X(1).
           10  F1-CINDCD-PROTE-TITLO   PIC X(1).
           10  F1-QDIA-PROTE-TITLO     PIC S9(3)V USAGE COMP-3.
           10  F1-CBCO-DEB-AUTOM       PIC S9(3)V USAGE COMP-3.
           10  F1-CAG-DEB-AUTOM        PIC S9(5)V USAGE COMP-3.
           10  F1-CCTA-DEB-AUTOM       PIC S9(13)V USAGE COMP-3.
           10  F1-CIDTFD-PRODT-ORIGN   PIC S9(2)V USAGE COMP-3.
           10  F1-CADM-CATAO-CREDT     PIC S9(9)V USAGE COMP-3.
           10  F1-CINDCD-VDA-RCBVL     PIC  X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F1-RETORNO.
       05  F1-COD-RETORNO                         PIC 9(04).
       05  F1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F1-PROXIMO-RESTART                     PIC 9(05).
       05  F1-QTDE-RETORNADA                      PIC 9(03).
       05  F1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF1.
           10  F1RT-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
           10  F1RT-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
           10  F1RT-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
           10  F1RT-CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
           10  F1RT-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
           10  F1RT-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
           10  F1RT-CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
           10  F1RT-CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
           10  F1RT-NNOTA-PROMS-RURAL    PIC S9(11)V USAGE COMP-3.
           10  F1RT-CINDCD-NOTA-PROMS    PIC X(3).
           10  F1RT-CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
           10  F1RT-CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
           10  F1RT-QPRODT-RURAL         PIC S9(9)V USAGE COMP-3.
           10  F1RT-CUND-MEDD            PIC S9(3)V USAGE COMP-3.
           10  F1RT-CINDCD-PGDOR-JURO    PIC X(1).
           10  F1RT-DEMIS-TITLO          PIC X(10).
           10  F1RT-DVCTO-TITLO-DESC     PIC X(10).
           10  F1RT-VTITLO-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
           10  F1RT-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
           10  F1RT-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
           10  F1RT-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
           10  F1RT-IPSSOA-SACDO         PIC X(60).
           10  F1RT-CSEU-NRO-TITLO       PIC X(15).
           10  F1RT-CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
           10  F1RT-CINDCD-INSTR-DESC    PIC S9(1)V USAGE COMP-3.
           10  F1RT-CCEP                 PIC S9(5)V USAGE COMP-3.
           10  F1RT-CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
           10  F1RT-ELOGDR               PIC X(40).
           10  F1RT-IBAIRO               PIC X(40).
           10  F1RT-IMUN                 PIC X(30).
           10  F1RT-CSGL-UF              PIC X(2).
           10  F1RT-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10  F1RT-HSIT-DESC-COML       PIC X(26).
           10  F1RT-VABTMT-TITLO-DESC    PIC S9(15)V9(2) USAGE COMP-3.
           10  F1RT-VORPAG-RCBVL         PIC S9(15)V9(2) USAGE COMP-3.
           10  F1RT-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
           10  F1RT-HULT-ATULZ           PIC X(26).
           10  F1RT-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  F1RT-CTERM                PIC X(8).
           10  F1RT-DINCL-REG            PIC X(10).
           10  F1RT-CORIGE-REC           PIC S9(3)V USAGE COMP-3.
           10  F1RT-CBCO-DEPOS           PIC S9(3)V USAGE COMP-3.
           10  F1RT-CAG-BCRIA-DEPOS      PIC S9(5)V USAGE COMP-3.
           10  F1RT-RANLSE-CREDT-CLI     PIC X(20).
           10  F1RT-DSOLTC-ANLSE-CREDT   PIC X(10).
           10  F1RT-CSIT-ANLSE-CREDT     PIC S9(3)V USAGE COMP-3.
           10  F1RT-CINDCD-ISENC-ANLSE   PIC X(1).
           10  F1RT-CINDCD-PROTE-TITLO   PIC X(1).
           10  F1RT-QDIA-PROTE-TITLO     PIC S9(3)V USAGE COMP-3.
           10  F1RT-CBCO-DEB-AUTOM       PIC S9(3)V USAGE COMP-3.
           10  F1RT-CAG-DEB-AUTOM        PIC S9(5)V USAGE COMP-3.
           10  F1RT-CCTA-DEB-AUTOM       PIC S9(13)V USAGE COMP-3.
           10  F1RT-CIDTFD-PRODT-ORIGN   PIC S9(2)V USAGE COMP-3.
           10  F1RT-CADM-CATAO-CREDT     PIC S9(9)V USAGE COMP-3.
           10  F1RT-CINDCD-VDA-RCBVL     PIC  X(1).
