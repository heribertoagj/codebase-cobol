      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G8 - DB2PRD.TTRASI_TITLO_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G8-ARGUMENTOS-ENTRADA.

       05  G8-COLUNAS-DA-TABELA.
           10  G8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G8-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  G8-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  G8-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  G8-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  G8-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  G8-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  G8-NNOTA-PROMS-RURAL            PIC S9(11)V COMP-3.
           10  G8-CINDCD-NOTA-PROMS            PIC X(3).
           10  G8-CTPO-PROTR-RURAL             PIC S9(2)V COMP-3.
           10  G8-CPRODT-RURAL                 PIC S9(3)V COMP-3.
           10  G8-QPRODT-RURAL                 PIC S9(9)V COMP-3.
           10  G8-CUND-MEDD                    PIC S9(3)V COMP-3.
           10  G8-CINDCD-PGDOR-JURO            PIC X(1).
           10  G8-DEMIS-TITLO                  PIC X(10).
           10  G8-DVCTO-TITLO-DESC             PIC X(10).
           10  G8-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  G8-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  G8-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  G8-CCTRL-CNPJ-CPF               PIC S9(2)V USAGE COMP-3.
           10  G8-IPSSOA-SACDO                 PIC X(60).
           10  G8-CSEU-NRO-TITLO               PIC X(15).
           10  G8-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  G8-CINDCD-INSTR-DESC            PIC S9(1)V COMP-3.
           10  G8-CCEP                         PIC S9(5)V COMP-3.
           10  G8-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  G8-ELOGDR                       PIC X(40).
           10  G8-IBAIRO                       PIC X(40).
           10  G8-IMUN                         PIC X(30).
           10  G8-CSGL-UF                      PIC X(2).
           10  G8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  G8-HSIT-DESC-COML               PIC X(26).
           10  G8-VABTMT-TITLO-DESC            PIC S9(15)V9(2) COMP-3.
           10  G8-VORPAG-RCBVL                 PIC S9(15)V9(2) COMP-3.
           10  G8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  G8-CINDCD-SELEC-DESC            PIC X(1).
           10  G8-HULT-ATULZ                   PIC X(26).
           10  G8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G8-CTERM                        PIC X(8).
           10  G8-DINCL-REG                    PIC X(10).
           10  G8-CORIGE-REC                   PIC S9(3)V USAGE COMP-3.
           10  G8-CBCO-DEPOS                   PIC S9(3)V USAGE COMP-3.
           10  G8-CAG-BCRIA-DEPOS              PIC S9(5)V USAGE COMP-3.
           10  G8-RANLSE-CREDT-CLI             PIC X(20).
           10  G8-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  G8-CSIT-ANLSE-CREDT             PIC S9(3)V USAGE COMP-3.
           10  G8-CINDCD-ISENC-ANLSE           PIC X(1).
           10  G8-CINDCD-PROTE-TITLO           PIC X(1).
           10  G8-QDIA-PROTE-TITLO             PIC S9(3)V USAGE COMP-3.
           10  G8-CBCO-DEB-AUTOM               PIC S9(3)V USAGE COMP-3.
           10  G8-CAG-DEB-AUTOM                PIC S9(5)V USAGE COMP-3.
           10  G8-CCTA-DEB-AUTOM               PIC S9(13)V USAGE COMP-3.
           10  G8-CIDTFD-PRODT-ORIGN           PIC S9(2)V USAGE COMP-3.
WPG        10  G8-CADM-CATAO-CREDT             PIC S9(9)V USAGE COMP-3.
WPG        10  G8-CINDCD-VDA-RCBVL             PIC  X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G8-RETORNO.
       05  G8-COD-RETORNO                         PIC 9(04).
       05  G8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G8-PROXIMO-RESTART                     PIC 9(05).
       05  G8-QTDE-RETORNADA                      PIC 9(03).
       05  G8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG8.
           10  G8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G8RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  G8RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  G8RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  G8RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  G8RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  G8RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  G8RT-NNOTA-PROMS-RURAL          PIC S9(11)V COMP-3.
           10  G8RT-CINDCD-NOTA-PROMS          PIC X(3).
           10  G8RT-CTPO-PROTR-RURAL           PIC S9(2)V COMP-3.
           10  G8RT-CPRODT-RURAL               PIC S9(3)V COMP-3.
           10  G8RT-QPRODT-RURAL               PIC S9(9)V COMP-3.
           10  G8RT-CUND-MEDD                  PIC S9(3)V COMP-3.
           10  G8RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  G8RT-DEMIS-TITLO                PIC X(10).
           10  G8RT-DVCTO-TITLO-DESC           PIC X(10).
           10  G8RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  G8RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  G8RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  G8RT-CCTRL-CNPJ-CPF             PIC S9(2)V USAGE COMP-3.
           10  G8RT-IPSSOA-SACDO               PIC X(60).
           10  G8RT-CSEU-NRO-TITLO             PIC X(15).
           10  G8RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  G8RT-CINDCD-INSTR-DESC          PIC S9(1)V COMP-3.
           10  G8RT-CCEP                       PIC S9(5)V COMP-3.
           10  G8RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  G8RT-ELOGDR                     PIC X(40).
           10  G8RT-IBAIRO                     PIC X(40).
           10  G8RT-IMUN                       PIC X(30).
           10  G8RT-CSGL-UF                    PIC X(2).
           10  G8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  G8RT-HSIT-DESC-COML             PIC X(26).
           10  G8RT-VABTMT-TITLO-DESC          PIC S9(15)V9(2) COMP-3.
           10  G8RT-VORPAG-RCBVL               PIC S9(15)V9(2) COMP-3.
           10  G8RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  G8RT-CINDCD-SELEC-DESC          PIC X(1).
           10  G8RT-HULT-ATULZ                 PIC X(26).
           10  G8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G8RT-CTERM                      PIC X(8).
           10  G8RT-DINCL-REG                  PIC X(10).
           10  G8RT-CORIGE-REC                 PIC S9(3)V USAGE COMP-3.
           10  G8RT-CBCO-DEPOS                 PIC S9(3)V USAGE COMP-3.
           10  G8RT-CAG-BCRIA-DEPOS            PIC S9(5)V USAGE COMP-3.
           10  G8RT-RANLSE-CREDT-CLI           PIC X(20).
           10  G8RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  G8RT-CSIT-ANLSE-CREDT           PIC S9(3)V USAGE COMP-3.
           10  G8RT-CINDCD-ISENC-ANLSE         PIC X(1).
           10  G8RT-CINDCD-PROTE-TITLO         PIC X(1).
           10  G8RT-QDIA-PROTE-TITLO           PIC S9(3)V USAGE COMP-3.
           10  G8RT-CBCO-DEB-AUTOM             PIC S9(3)V USAGE COMP-3.
           10  G8RT-CAG-DEB-AUTOM              PIC S9(5)V USAGE COMP-3.
           10  G8RT-CCTA-DEB-AUTOM            PIC S9(13)V USAGE COMP-3.
           10  G8RT-CIDTFD-PRODT-ORIGN         PIC S9(2)V USAGE COMP-3.
WPG        10  G8RT-CADM-CATAO-CREDT           PIC S9(9)V USAGE COMP-3.
WPG        10  G8RT-CINDCD-VDA-RCBVL           PIC X(1).
