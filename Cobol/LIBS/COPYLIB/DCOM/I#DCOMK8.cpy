      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K8 - DB2PRD.TDESPR_TITLO_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K8-ARGUMENTOS-ENTRADA.
       05  K8-COLUNAS-DA-TABELA.
           10  K8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  K8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  K8-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  K8-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  K8-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  K8-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  K8-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  K8-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  K8-NNOTA-PROMS-RURAL            PIC S9(11)V COMP-3.
           10  K8-CINDCD-NOTA-PROMS            PIC X(3).
           10  K8-CTPO-PROTR-RURAL             PIC S9(2)V COMP-3.
           10  K8-CPRODT-RURAL                 PIC S9(3)V COMP-3.
           10  K8-QPRODT-RURAL                 PIC S9(9)V COMP-3.
           10  K8-CUND-MEDD                    PIC S9(3)V COMP-3.
           10  K8-CINDCD-PGDOR-JURO            PIC X(1).
           10  K8-DEMIS-TITLO                  PIC X(10).
           10  K8-DVCTO-TITLO-DESC             PIC X(10).
           10  K8-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  K8-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  K8-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  K8-CCTRL-CNPJ-CPF               PIC S9(2)V USAGE COMP-3.
           10  K8-IPSSOA-SACDO                 PIC X(60).
           10  K8-CSEU-NRO-TITLO               PIC X(15).
           10  K8-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  K8-CINDCD-INSTR-DESC            PIC S9(1)V COMP-3.
           10  K8-CCEP                         PIC S9(5)V COMP-3.
           10  K8-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  K8-ELOGDR                       PIC X(30).
           10  K8-IBAIRO                       PIC X(40).
           10  K8-IMUN                         PIC X(30).
           10  K8-CSGL-UF                      PIC X(2).
           10  K8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  K8-HSIT-DESC-COML               PIC X(26).
           10  K8-VABTMT-TITLO-DESC            PIC S9(15)V9(2) COMP-3.
           10  K8-VORPAG-RCBVL                 PIC S9(15)V9(2) COMP-3.
           10  K8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  K8-CINDCD-SELEC-DESC            PIC X(1).
           10  K8-HULT-ATULZ                   PIC X(26).
           10  K8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  K8-CTERM                        PIC X(8).
           10  K8-DINCL-REG                    PIC X(10).
           10  K8-CORIGE-REC                   PIC S9(3)V USAGE COMP-3.
           10  K8-CBCO-DEPOS                   PIC S9(3)V USAGE COMP-3.
           10  K8-CAG-BCRIA-DEPOS              PIC S9(5)V USAGE COMP-3.
           10  K8-RANLSE-CREDT-CLI             PIC X(20).
           10  K8-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  K8-CSIT-ANLSE-CREDT             PIC S9(3)V USAGE COMP-3.
           10  K8-CINDCD-ISENC-ANLSE           PIC X(1).
           10  K8-CINDCD-PROTE-TITLO           PIC X(1).
           10  K8-QDIA-PROTE-TITLO             PIC S9(3)V USAGE COMP-3.
           10  K8-CBCO-DEB-AUTOM               PIC S9(3)V USAGE COMP-3.
           10  K8-CAG-DEB-AUTOM                PIC S9(5)V USAGE COMP-3.
           10  K8-CCTA-DEB-AUTOM               PIC S9(13)V USAGE COMP-3.
           10  K8-CIDTFD-PRODT-ORIGN           PIC S9(2)V USAGE COMP-3.
           10  K8-CADM-CATAO-CREDT             PIC S9(9)V USAGE COMP-3.
           10  K8-CINDCD-VDA-RCBVL             PIC  X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K8-QTDE-A-DESPREZAR                 PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K8-INSTRUCAO                        PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K8-RETORNO.
       05  K8-COD-RETORNO                      PIC 9(04).
       05  K8-MENSAGEM                         PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K8-PROXIMO-RESTART                  PIC 9(05).
       05  K8-QTDE-RETORNADA                   PIC 9(03).
       05  K8-QTDE-TOTAL                       PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK8.
           10  K8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  K8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  K8RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  K8RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  K8RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  K8RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  K8RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  K8RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  K8RT-NNOTA-PROMS-RURAL          PIC S9(11)V COMP-3.
           10  K8RT-CINDCD-NOTA-PROMS          PIC X(3).
           10  K8RT-CTPO-PROTR-RURAL           PIC S9(2)V COMP-3.
           10  K8RT-CPRODT-RURAL               PIC S9(3)V COMP-3.
           10  K8RT-QPRODT-RURAL               PIC S9(9)V COMP-3.
           10  K8RT-CUND-MEDD                  PIC S9(3)V COMP-3.
           10  K8RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  K8RT-DEMIS-TITLO                PIC X(10).
           10  K8RT-DVCTO-TITLO-DESC           PIC X(10).
           10  K8RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  K8RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  K8RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  K8RT-CCTRL-CNPJ-CPF             PIC S9(2)V USAGE COMP-3.
           10  K8RT-IPSSOA-SACDO               PIC X(60).
           10  K8RT-CSEU-NRO-TITLO             PIC X(15).
           10  K8RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  K8RT-CINDCD-INSTR-DESC          PIC S9(1)V COMP-3.
           10  K8RT-CCEP                       PIC S9(5)V COMP-3.
           10  K8RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  K8RT-ELOGDR                     PIC X(40).
           10  K8RT-IBAIRO                     PIC X(40).
           10  K8RT-IMUN                       PIC X(30).
           10  K8RT-CSGL-UF                    PIC X(2).
           10  K8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  K8RT-HSIT-DESC-COML             PIC X(26).
           10  K8RT-VABTMT-TITLO-DESC          PIC S9(15)V9(2) COMP-3.
           10  K8RT-VORPAG-RCBVL               PIC S9(15)V9(2) COMP-3.
           10  K8RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  K8RT-CINDCD-SELEC-DESC          PIC X(1).
           10  K8RT-HULT-ATULZ                 PIC X(26).
           10  K8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  K8RT-CTERM                      PIC X(8).
           10  K8RT-DINCL-REG                  PIC X(10).
           10  K8RT-CORIGE-REC                 PIC S9(3)V USAGE COMP-3.
           10  K8RT-CBCO-DEPOS                 PIC S9(3)V USAGE COMP-3.
           10  K8RT-CAG-BCRIA-DEPOS            PIC S9(5)V USAGE COMP-3.
           10  K8RT-RANLSE-CREDT-CLI           PIC X(20).
           10  K8RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  K8RT-CSIT-ANLSE-CREDT           PIC S9(3)V USAGE COMP-3.
           10  K8RT-CINDCD-ISENC-ANLSE         PIC X(1).
           10  K8RT-CINDCD-PROTE-TITLO         PIC X(1).
           10  K8RT-QDIA-PROTE-TITLO           PIC S9(3)V USAGE COMP-3.
           10  K8RT-CBCO-DEB-AUTOM             PIC S9(3)V USAGE COMP-3.
           10  K8RT-CAG-DEB-AUTOM              PIC S9(5)V USAGE COMP-3.
           10  K8RT-CCTA-DEB-AUTOM            PIC S9(13)V USAGE COMP-3.
           10  K8RT-CIDTFD-PRODT-ORIGN         PIC S9(2)V USAGE COMP-3.
           10  K8RT-CADM-CATAO-CREDT           PIC S9(9)V USAGE COMP-3.
           10  K8RT-CINDCD-VDA-RCBVL           PIC X(1).

