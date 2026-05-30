      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C5 - DB2PRD.TOPER_DESC_COML        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C5-ARGUMENTOS-ENTRADA.

       05  C5-COLUNAS-DA-TABELA.
           10  C5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C5-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  C5-HSIT-DESC-COML               PIC X(26).
           10  C5-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  C5-VLIBRC-ANTCP-OPER            PIC S9(15)V9(2) COMP-3.
           10  C5-CBCO                         PIC S9(3)V COMP-3.
           10  C5-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  C5-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  C5-CBCO-OPER-OPER               PIC S9(3)V COMP-3.
           10  C5-CAG-OPER-OPER                PIC S9(5)V COMP-3.
           10  C5-CPRODT                       PIC S9(3)V COMP-3.
           10  C5-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  C5-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  C5-CCONTR-CONVE-DESC            PIC S9(9)V COMP-3.
           10  C5-CVRSAO-CONTR-CONVE           PIC S9(3)V COMP-3.
           10  C5-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  C5-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  C5-CINDCD-DIGTC-AG              PIC X(1).
           10  C5-CINDCD-ISENC-OPER            PIC X(1).
           10  C5-CINDCD-AUTRZ-OPER            PIC X(1).
           10  C5-CINDCD-MODLD-INCL            PIC X(1).
           10  C5-CCTRO-CUSTO                  PIC X(4).
           10  C5-DANO-BASE                    PIC S9(4)V COMP-3.
           10  C5-CDEPDC                       PIC S9(5)V COMP-3.
           10  C5-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  C5-CINDCD-APROV-CREDT           PIC X(1).
           10  C5-VOPER-DESC-COML              PIC S9(15)V9(2) COMP-3.
           10  C5-DINIC-OPER-DESC              PIC X(10).
           10  C5-DVCTO-FNAL-OPER              PIC X(10).
           10  C5-TOPER-DESC-COML              PIC S9(5)V COMP-3.
           10  C5-TMED-PONDE-OPER              PIC S9(3)V COMP-3.
           10  C5-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  C5-QPCELA-OPER                  PIC S9(5)V COMP-3.
           10  C5-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  C5-NAUTRZ-TRNSM-ESCR            PIC S9(7)V COMP-3.
           10  C5-CINDCD-PGDOR-JURO            PIC X(1).
           10  C5-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  C5-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  C5-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  C5-PALIQT-IOF-DESC              PIC S9(3)V9(5) COMP-3.
           10  C5-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  C5-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  C5-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  C5-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  C5-VJURO-OPER-DESC              PIC S9(15)V9(2) COMP-3.
           10  C5-VJURO-CUSTO-OPER             PIC S9(15)V9(2) COMP-3.
           10  C5-VTARIF-REG-TITLO             PIC S9(15)V9(2) COMP-3.
           10  C5-VTAC-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  C5-VPSENT-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  C5-VIOF-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  C5-VLIQ-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  C5-CINDCD-VALDC-ROTRO           PIC X(1).
           10  C5-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  C5-CFUNC-VALDC-ROTRO            PIC S9(9)V COMP-3.
           10  C5-DULT-ADTTO-OPER              PIC X(10).
           10  C5-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  C5-HULT-ATULZ                   PIC X(26).
           10  C5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C5-CTERM                        PIC X(8).
           10  C5-CCONTR-CORP-OPER             PIC S9(7)V COMP-3.
           10  C5-DINCL-REG                    PIC X(10).
           10  C5-CCART-LIM-CREDT              PIC X(05).
WPG        10  C5-CCHAVE-ELMTO-DESC            PIC X(45).
           10  C5-CINDCD-ALIQT-REDZD           PIC X(1).
           10  C5-PTX-JURO-DIA                 PIC S9(3)V9(5) COMP-3.
           10  C5-CINDCD-FREQ-TX               PIC X(2).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C5-RETORNO.
       05  C5-COD-RETORNO                         PIC 9(04).
       05  C5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C5-PROXIMO-RESTART                     PIC 9(05).
       05  C5-QTDE-RETORNADA                      PIC 9(03).
       05  C5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC5.
           10  C5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C5RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  C5RT-HSIT-DESC-COML             PIC X(26).
           10  C5RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  C5RT-VLIBRC-ANTCP-OPER          PIC S9(15)V9(2) COMP-3.
           10  C5RT-CBCO                       PIC S9(3)V COMP-3.
           10  C5RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  C5RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  C5RT-CBCO-OPER-OPER             PIC S9(3)V COMP-3.
           10  C5RT-CAG-OPER-OPER              PIC S9(5)V COMP-3.
           10  C5RT-CPRODT                     PIC S9(3)V COMP-3.
           10  C5RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  C5RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  C5RT-CCONTR-CONVE-DESC          PIC S9(9)V COMP-3.
           10  C5RT-CVRSAO-CONTR-CONVE         PIC S9(3)V COMP-3.
           10  C5RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  C5RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  C5RT-CINDCD-DIGTC-AG            PIC X(1).
           10  C5RT-CINDCD-ISENC-OPER          PIC X(1).
           10  C5RT-CINDCD-AUTRZ-OPER          PIC X(1).
           10  C5RT-CINDCD-MODLD-INCL          PIC X(1).
           10  C5RT-CCTRO-CUSTO                PIC X(4).
           10  C5RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  C5RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  C5RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  C5RT-CINDCD-APROV-CREDT         PIC X(1).
           10  C5RT-VOPER-DESC-COML            PIC S9(15)V9(2) COMP-3.
           10  C5RT-DINIC-OPER-DESC            PIC X(10).
           10  C5RT-DVCTO-FNAL-OPER            PIC X(10).
           10  C5RT-TOPER-DESC-COML            PIC S9(5)V COMP-3.
           10  C5RT-TMED-PONDE-OPER            PIC S9(3)V COMP-3.
           10  C5RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  C5RT-QPCELA-OPER                PIC S9(5)V COMP-3.
           10  C5RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  C5RT-NAUTRZ-TRNSM-ESCR          PIC S9(7)V COMP-3.
           10  C5RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  C5RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  C5RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  C5RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  C5RT-PALIQT-IOF-DESC            PIC S9(3)V9(5) COMP-3.
           10  C5RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  C5RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  C5RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  C5RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  C5RT-VJURO-OPER-DESC            PIC S9(15)V9(2) COMP-3.
           10  C5RT-VJURO-CUSTO-OPER           PIC S9(15)V9(2) COMP-3.
           10  C5RT-VTARIF-REG-TITLO           PIC S9(15)V9(2) COMP-3.
           10  C5RT-VTAC-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  C5RT-VPSENT-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  C5RT-VIOF-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  C5RT-VLIQ-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  C5RT-CINDCD-VALDC-ROTRO         PIC X(1).
           10  C5RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  C5RT-CFUNC-VALDC-ROTRO          PIC S9(9)V COMP-3.
           10  C5RT-DULT-ADTTO-OPER            PIC X(10).
           10  C5RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  C5RT-HULT-ATULZ                 PIC X(26).
           10  C5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C5RT-CTERM                      PIC X(8).
           10  C5RT-CCONTR-CORP-OPER           PIC S9(7)V COMP-3.
           10  C5RT-DINCL-REG                  PIC X(10).
           10  C5RT-CCART-LIM-CREDT            PIC X(05).
WPG        10  C5RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  C5RT-CINDCD-ALIQT-REDZD         PIC X(1).
           10  C5RT-PTX-JURO-DIA               PIC S9(3)V9(5) COMP-3.
           10  C5RT-CINDCD-FREQ-TX             PIC X(2).
