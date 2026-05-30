      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C0 - DB2PRD.THIST_TITLO_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C0-ARGUMENTOS-ENTRADA.

       05  C0-COLUNAS-DA-TABELA.
           10  C0-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C0-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C0-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  C0-HULT-ATULZ                   PIC X(26).
           10  C0-NPSSOA-DESC-COML             PIC S9(9)V COMP-3.
           10  C0-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  C0-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  C0-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  C0-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  C0-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  C0-NNOTA-PROMS-RURAL            PIC S9(11)V COMP-3.
           10  C0-CINDCD-NOTA-PROMS            PIC X(3).
           10  C0-CTPO-PROTR-RURAL             PIC S9(2)V COMP-3.
           10  C0-CPRODT-RURAL                 PIC S9(3)V COMP-3.
           10  C0-QPRODT-RURAL                 PIC S9(9)V COMP-3.
           10  C0-CUND-MEDD                    PIC S9(3)V COMP-3.
           10  C0-CINDCD-PGDOR-JURO            PIC X(1).
           10  C0-DEMIS-TITLO                  PIC X(10).
           10  C0-DVCTO-TITLO-DESC             PIC X(10).
           10  C0-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  C0-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  C0-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  C0-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  C0-IPSSOA-SACDO                 PIC X(60).
           10  C0-CSEU-NRO-TITLO               PIC X(15).
           10  C0-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  C0-CINDCD-INSTR-DESC            PIC S9(1)V COMP-3.
           10  C0-CCEP                         PIC S9(5)V COMP-3.
           10  C0-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  C0-ELOGDR                       PIC X(40).
           10  C0-IBAIRO                       PIC X(40).
           10  C0-IMUN                         PIC X(30).
           10  C0-CSGL-UF                      PIC X(2).
           10  C0-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  C0-HSIT-DESC-COML               PIC X(26).
           10  C0-VABTMT-TITLO-DESC            PIC S9(15)V9(2) COMP-3.
           10  C0-VORPAG-RCBVL                 PIC S9(15)V9(2) COMP-3.
           10  C0-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  C0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C0-CTERM                        PIC X(8).
           10  C0-DINCL-REG                    PIC X(10).
           10  C0-CORIGE-REC                   PIC S9(3)V COMP-3.
           10  C0-CBCO-DEPOS                   PIC S9(3)V COMP-3.
           10  C0-CAG-BCRIA-DEPOS              PIC S9(5)V COMP-3.
           10  C0-RANLSE-CREDT-CLI             PIC X(20).
           10  C0-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  C0-CSIT-ANLSE-CREDT             PIC S9(3)V COMP-3.
           10  C0-CINDCD-ISENC-ANLSE           PIC X(1).
           10  C0-CINDCD-PROTE-TITLO           PIC X(1).
           10  C0-QDIA-PROTE-TITLO             PIC S9(3)V COMP-3.
           10  C0-CBCO-DEB-AUTOM               PIC S9(3)V COMP-3.
           10  C0-CAG-DEB-AUTOM                PIC S9(5)V COMP-3.
           10  C0-CCTA-DEB-AUTOM               PIC S9(13)V COMP-3.
           10  C0-CIDTFD-PRODT-ORIGN           PIC S9(2)V COMP-3.
           10  C0-CADM-CATAO-CREDT             PIC S9(9)V COMP-3.
           10  C0-CINDCD-VDA-RCBVL             PIC  X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C0-RETORNO.
       05  C0-COD-RETORNO                         PIC 9(04).
       05  C0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C0-PROXIMO-RESTART                     PIC 9(05).
       05  C0-QTDE-RETORNADA                      PIC 9(03).
       05  C0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC0.
           10  C0RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C0RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C0RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  C0RT-HULT-ATULZ                 PIC X(26).
           10  C0RT-NPSSOA-DESC-COML           PIC S9(9)V COMP-3.
           10  C0RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  C0RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  C0RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  C0RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  C0RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  C0RT-NNOTA-PROMS-RURAL          PIC S9(11)V COMP-3.
           10  C0RT-CINDCD-NOTA-PROMS          PIC X(3).
           10  C0RT-CTPO-PROTR-RURAL           PIC S9(2)V COMP-3.
           10  C0RT-CPRODT-RURAL               PIC S9(3)V COMP-3.
           10  C0RT-QPRODT-RURAL               PIC S9(9)V COMP-3.
           10  C0RT-CUND-MEDD                  PIC S9(3)V COMP-3.
           10  C0RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  C0RT-DEMIS-TITLO                PIC X(10).
           10  C0RT-DVCTO-TITLO-DESC           PIC X(10).
           10  C0RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  C0RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  C0RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  C0RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  C0RT-IPSSOA-SACDO               PIC X(60).
           10  C0RT-CSEU-NRO-TITLO             PIC X(15).
           10  C0RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  C0RT-CINDCD-INSTR-DESC          PIC S9(1)V COMP-3.
           10  C0RT-CCEP                       PIC S9(5)V COMP-3.
           10  C0RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  C0RT-ELOGDR                     PIC X(40).
           10  C0RT-IBAIRO                     PIC X(40).
           10  C0RT-IMUN                       PIC X(30).
           10  C0RT-CSGL-UF                    PIC X(2).
           10  C0RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  C0RT-HSIT-DESC-COML             PIC X(26).
           10  C0RT-VABTMT-TITLO-DESC          PIC S9(15)V9(2) COMP-3.
           10  C0RT-VORPAG-RCBVL               PIC S9(15)V9(2) COMP-3.
           10  C0RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  C0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C0RT-CTERM                      PIC X(8).
           10  C0RT-DINCL-REG                  PIC X(10).
           10  C0RT-CORIGE-REC                 PIC S9(3)V COMP-3.
           10  C0RT-CBCO-DEPOS                 PIC S9(3)V COMP-3.
           10  C0RT-CAG-BCRIA-DEPOS            PIC S9(5)V COMP-3.
           10  C0RT-RANLSE-CREDT-CLI           PIC X(20).
           10  C0RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  C0RT-CSIT-ANLSE-CREDT           PIC S9(3)V COMP-3.
           10  C0RT-CINDCD-ISENC-ANLSE         PIC X(1).
           10  C0RT-CINDCD-PROTE-TITLO         PIC X(1).
           10  C0RT-QDIA-PROTE-TITLO           PIC S9(3)V COMP-3.
           10  C0RT-CBCO-DEB-AUTOM             PIC S9(3)V COMP-3.
           10  C0RT-CAG-DEB-AUTOM              PIC S9(5)V COMP-3.
           10  C0RT-CCTA-DEB-AUTOM             PIC S9(13)V COMP-3.
           10  C0RT-CIDTFD-PRODT-ORIGN         PIC S9(2)V COMP-3.
           10  C0RT-CADM-CATAO-CREDT           PIC S9(9)V COMP-3.
           10  C0RT-CINDCD-VDA-RCBVL           PIC  X(1).
