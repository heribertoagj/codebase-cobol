      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K7 - DB2PRD.TDESPR_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K7-ARGUMENTOS-ENTRADA.
       05  K7-COLUNAS-DA-TABELA.
           10  K7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  K7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  K7-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  K7-HSIT-DESC-COML               PIC X(26).
           10  K7-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  K7-VLIBRC-ANTCP-OPER            PIC S9(15)V9(2) COMP-3.
           10  K7-CBCO                         PIC S9(3)V COMP-3.
           10  K7-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  K7-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  K7-CBCO-OPER-OPER               PIC S9(3)V COMP-3.
           10  K7-CAG-OPER-OPER                PIC S9(5)V COMP-3.
           10  K7-CPRODT                       PIC S9(3)V COMP-3.
           10  K7-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  K7-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  K7-CCONTR-CONVE-DESC            PIC S9(9)V COMP-3.
           10  K7-CVRSAO-CONTR-CONVE           PIC S9(3)V COMP-3.
           10  K7-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  K7-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  K7-CINDCD-DIGTC-AG              PIC X(1).
           10  K7-CINDCD-ISENC-OPER            PIC X(1).
           10  K7-CINDCD-AUTRZ-OPER            PIC X(1).
           10  K7-CINDCD-MODLD-INCL            PIC X(1).
           10  K7-CCTRO-CUSTO                  PIC X(4).
           10  K7-DANO-BASE                    PIC S9(4)V COMP-3.
           10  K7-CDEPDC                       PIC S9(5)V COMP-3.
           10  K7-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  K7-CINDCD-APROV-CREDT           PIC X(1).
           10  K7-VOPER-DESC-COML              PIC S9(15)V9(2) COMP-3.
           10  K7-DINIC-OPER-DESC              PIC X(10).
           10  K7-DVCTO-FNAL-OPER              PIC X(10).
           10  K7-VOPER-DESC-APURA             PIC S9(15)V9(2) COMP-3.
           10  K7-QTITLO-OPER-APURA            PIC S9(5)V COMP-3.
           10  K7-DVCTO-FNAL-APURA             PIC X(10).
           10  K7-TOPER-DESC-APURA             PIC S9(3)V COMP-3.
           10  K7-TMED-PONDE-OPER              PIC S9(3)V COMP-3.
           10  K7-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  K7-QPCELA-OPER                  PIC S9(5)V COMP-3.
           10  K7-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  K7-NAUTRZ-TRNSM-ESCR            PIC S9(7)V COMP-3.
           10  K7-CINDCD-PGDOR-JURO            PIC X(1).
           10  K7-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  K7-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  K7-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  K7-PALIQT-IOF-DESC              PIC S9(3)V9(5) COMP-3.
           10  K7-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  K7-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  K7-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  K7-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  K7-VJURO-OPER-DESC              PIC S9(15)V9(2) COMP-3.
           10  K7-VJURO-CUSTO-OPER             PIC S9(15)V9(2) COMP-3.
           10  K7-VTARIF-REG-TITLO             PIC S9(15)V9(2) COMP-3.
           10  K7-VTAC-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  K7-VPSENT-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  K7-VIOF-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  K7-VLIQ-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  K7-CINDCD-VALDC-ROTRO           PIC X(1).
           10  K7-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  K7-CFUNC-VALDC-ROTRO            PIC S9(9)V COMP-3.
           10  K7-DULT-ADTTO-OPER              PIC X(10).
           10  K7-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  K7-HULT-ATULZ                   PIC X(26).
           10  K7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  K7-CTERM                        PIC X(8).
           10  K7-CCONTR-CORP-OPER             PIC S9(7)V COMP-3.
           10  K7-DINCL-REG                    PIC X(10).
           10  K7-CCART-LIM-CREDT              PIC X(5).
           10  K7-CCHAVE-ELMTO-DESC            PIC X(45).
           10  K7-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  K7-CINDCD-ALIQT-REDZD           PIC X(1).
           10  K7-PTX-JURO-DIA                 PIC S9(3)V9(5) COMP-3.
           10  K7-CINDCD-FREQ-TX               PIC X(2).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K7-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K7-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K7-RETORNO.
       05  K7-COD-RETORNO                         PIC 9(04).
       05  K7-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K7-PROXIMO-RESTART                     PIC 9(05).
       05  K7-QTDE-RETORNADA                      PIC 9(03).
       05  K7-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK7.
           10  K7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  K7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  K7RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  K7RT-HSIT-DESC-COML             PIC X(26).
           10  K7RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  K7RT-VLIBRC-ANTCP-OPER          PIC S9(15)V9(2) COMP-3.
           10  K7RT-CBCO                       PIC S9(3)V COMP-3.
           10  K7RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  K7RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  K7RT-CBCO-OPER-OPER             PIC S9(3)V COMP-3.
           10  K7RT-CAG-OPER-OPER              PIC S9(5)V COMP-3.
           10  K7RT-CPRODT                     PIC S9(3)V COMP-3.
           10  K7RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  K7RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  K7RT-CCONTR-CONVE-DESC          PIC S9(9)V COMP-3.
           10  K7RT-CVRSAO-CONTR-CONVE         PIC S9(3)V COMP-3.
           10  K7RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  K7RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  K7RT-CINDCD-DIGTC-AG            PIC X(1).
           10  K7RT-CINDCD-ISENC-OPER          PIC X(1).
           10  K7RT-CINDCD-AUTRZ-OPER          PIC X(1).
           10  K7RT-CINDCD-MODLD-INCL          PIC X(1).
           10  K7RT-CCTRO-CUSTO                PIC X(4).
           10  K7RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  K7RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  K7RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  K7RT-CINDCD-APROV-CREDT         PIC X(1).
           10  K7RT-VOPER-DESC-COML            PIC S9(15)V9(2) COMP-3.
           10  K7RT-DINIC-OPER-DESC            PIC X(10).
           10  K7RT-DVCTO-FNAL-OPER            PIC X(10).
           10  K7RT-VOPER-DESC-APURA           PIC S9(15)V9(2) COMP-3.
           10  K7RT-QTITLO-OPER-APURA          PIC S9(5)V COMP-3.
           10  K7RT-DVCTO-FNAL-APURA           PIC X(10).
           10  K7RT-TOPER-DESC-APURA           PIC S9(3)V COMP-3.
           10  K7RT-TMED-PONDE-OPER            PIC S9(3)V COMP-3.
           10  K7RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  K7RT-QPCELA-OPER                PIC S9(5)V COMP-3.
           10  K7RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  K7RT-NAUTRZ-TRNSM-ESCR          PIC S9(7)V COMP-3.
           10  K7RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  K7RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  K7RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  K7RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  K7RT-PALIQT-IOF-DESC            PIC S9(3)V9(5) COMP-3.
           10  K7RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  K7RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  K7RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  K7RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  K7RT-VJURO-OPER-DESC            PIC S9(15)V9(2) COMP-3.
           10  K7RT-VJURO-CUSTO-OPER           PIC S9(15)V9(2) COMP-3.
           10  K7RT-VTARIF-REG-TITLO           PIC S9(15)V9(2) COMP-3.
           10  K7RT-VTAC-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  K7RT-VPSENT-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  K7RT-VIOF-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  K7RT-VLIQ-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  K7RT-CINDCD-VALDC-ROTRO         PIC X(1).
           10  K7RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  K7RT-CFUNC-VALDC-ROTRO          PIC S9(9)V COMP-3.
           10  K7RT-DULT-ADTTO-OPER            PIC X(10).
           10  K7RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  K7RT-HULT-ATULZ                 PIC X(26).
           10  K7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  K7RT-CTERM                      PIC X(8).
           10  K7RT-CCONTR-CORP-OPER           PIC S9(7)V COMP-3.
           10  K7RT-DINCL-REG                  PIC X(10).
           10  K7RT-CCART-LIM-CREDT            PIC X(5).
           10  K7RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  K7RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  K7RT-CINDCD-ALIQT-REDZD         PIC X(1).
           10  K7RT-PTX-JURO-DIA               PIC S9(3)V9(5) COMP-3.
           10  K7RT-CINDCD-FREQ-TX             PIC X(2).

