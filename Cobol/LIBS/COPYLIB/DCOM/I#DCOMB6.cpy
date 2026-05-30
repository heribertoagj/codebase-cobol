      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B6 - DB2PRD.THIST_OPER_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B6-ARGUMENTOS-ENTRADA.

       05  B6-COLUNAS-DA-TABELA.
           10  B6-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B6-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B6-HULT-ATULZ                   PIC X(26).
           10  B6-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  B6-HSIT-DESC-COML               PIC X(26).
           10  B6-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  B6-VLIBRC-ANTCP-OPER            PIC S9(15)V9(2) COMP-3.
           10  B6-CBCO                         PIC S9(3)V COMP-3.
           10  B6-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  B6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  B6-CBCO-OPER-OPER               PIC S9(3)V COMP-3.
           10  B6-CAG-OPER-OPER                PIC S9(5)V COMP-3.
           10  B6-CPRODT                       PIC S9(3)V COMP-3.
           10  B6-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  B6-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  B6-CCONTR-CONVE-DESC            PIC S9(9)V COMP-3.
           10  B6-CVRSAO-CONTR-CONVE           PIC S9(3)V COMP-3.
           10  B6-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  B6-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  B6-CINDCD-DIGTC-AG              PIC X(1).
           10  B6-CINDCD-ISENC-OPER            PIC X(1).
           10  B6-CINDCD-AUTRZ-OPER            PIC X(1).
           10  B6-CINDCD-MODLD-INCL            PIC X(1).
           10  B6-CCTRO-CUSTO                  PIC X(4).
           10  B6-DANO-BASE                    PIC S9(4)V COMP-3.
           10  B6-CDEPDC                       PIC S9(5)V COMP-3.
           10  B6-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  B6-CINDCD-APROV-CREDT           PIC X(1).
           10  B6-VOPER-DESC-COML              PIC S9(15)V9(2) COMP-3.
           10  B6-DINIC-OPER-DESC              PIC X(10).
           10  B6-DVCTO-FNAL-OPER              PIC X(10).
           10  B6-TOPER-DESC-COML              PIC S9(5)V COMP-3.
           10  B6-TMED-PONDE-OPER              PIC S9(3)V COMP-3.
           10  B6-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  B6-QPCELA-OPER                  PIC S9(5)V COMP-3.
           10  B6-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  B6-NAUTRZ-TRNSM-ESCR            PIC S9(7)V COMP-3.
           10  B6-CINDCD-PGDOR-JURO            PIC X(1).
           10  B6-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  B6-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  B6-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  B6-PALIQT-IOF-DESC              PIC S9(3)V9(5) COMP-3.
           10  B6-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  B6-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  B6-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  B6-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  B6-VJURO-OPER-DESC              PIC S9(15)V9(2) COMP-3.
           10  B6-VJURO-CUSTO-OPER             PIC S9(15)V9(2) COMP-3.
           10  B6-VTARIF-REG-TITLO             PIC S9(15)V9(2) COMP-3.
           10  B6-VTAC-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  B6-VPSENT-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  B6-VIOF-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  B6-VLIQ-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  B6-CINDCD-VALDC-ROTRO           PIC X(1).
           10  B6-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  B6-CFUNC-VALDC-ROTRO            PIC S9(9)V COMP-3.
           10  B6-DULT-ADTTO-OPER              PIC X(10).
           10  B6-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  B6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B6-CTERM                        PIC X(8).
           10  B6-CCONTR-CORP-OPER             PIC S9(7)V COMP-3.
           10  B6-DINCL-REG                    PIC X(10).
           10  B6-CCART-LIM-CREDT              PIC X(5).
           10  B6-CINDCD-ALIQT-REDZD           PIC X(1).
           10  B6-PTX-JURO-DIA                 PIC S9(3)V9(5) COMP-3.
           10  B6-CINDCD-FREQ-TX               PIC X(2).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B6-INSTRUCAO                          PIC X(10).

       05  B6-COLUNAS-AUXILIARES.
           10  B6-CCNPJ-CPF                    PIC S9(9)V USAGE COMP-3.
           10  B6-CFLIAL-CNPJ                  PIC S9(5)V USAGE COMP-3.
           10  B6-CCTRL-CNPJ-CPF               PIC S9(2)V USAGE COMP-3.


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B6-RETORNO.
       05  B6-COD-RETORNO                         PIC 9(04).
       05  B6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B6-PROXIMO-RESTART                     PIC 9(05).
       05  B6-QTDE-RETORNADA                      PIC 9(03).
       05  B6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB6.
           10  B6RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B6RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B6RT-HULT-ATULZ                 PIC X(26).
           10  B6RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  B6RT-HSIT-DESC-COML             PIC X(26).
           10  B6RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  B6RT-VLIBRC-ANTCP-OPER          PIC S9(15)V9(2) COMP-3.
           10  B6RT-CBCO                       PIC S9(3)V COMP-3.
           10  B6RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  B6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  B6RT-CBCO-OPER-OPER             PIC S9(3)V COMP-3.
           10  B6RT-CAG-OPER-OPER              PIC S9(5)V COMP-3.
           10  B6RT-CPRODT                     PIC S9(3)V COMP-3.
           10  B6RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  B6RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  B6RT-CCONTR-CONVE-DESC          PIC S9(9)V COMP-3.
           10  B6RT-CVRSAO-CONTR-CONVE         PIC S9(3)V COMP-3.
           10  B6RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  B6RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  B6RT-CINDCD-DIGTC-AG            PIC X(1).
           10  B6RT-CINDCD-ISENC-OPER          PIC X(1).
           10  B6RT-CINDCD-AUTRZ-OPER          PIC X(1).
           10  B6RT-CINDCD-MODLD-INCL          PIC X(1).
           10  B6RT-CCTRO-CUSTO                PIC X(4).
           10  B6RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  B6RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  B6RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  B6RT-CINDCD-APROV-CREDT         PIC X(1).
           10  B6RT-VOPER-DESC-COML            PIC S9(15)V9(2) COMP-3.
           10  B6RT-DINIC-OPER-DESC            PIC X(10).
           10  B6RT-DVCTO-FNAL-OPER            PIC X(10).
           10  B6RT-TOPER-DESC-COML            PIC S9(5)V COMP-3.
           10  B6RT-TMED-PONDE-OPER            PIC S9(3)V COMP-3.
           10  B6RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  B6RT-QPCELA-OPER                PIC S9(5)V COMP-3.
           10  B6RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  B6RT-NAUTRZ-TRNSM-ESCR          PIC S9(7)V COMP-3.
           10  B6RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  B6RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  B6RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  B6RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  B6RT-PALIQT-IOF-DESC            PIC S9(3)V9(5) COMP-3.
           10  B6RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  B6RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  B6RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  B6RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  B6RT-VJURO-OPER-DESC            PIC S9(15)V9(2) COMP-3.
           10  B6RT-VJURO-CUSTO-OPER           PIC S9(15)V9(2) COMP-3.
           10  B6RT-VTARIF-REG-TITLO           PIC S9(15)V9(2) COMP-3.
           10  B6RT-VTAC-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  B6RT-VPSENT-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  B6RT-VIOF-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  B6RT-VLIQ-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  B6RT-CINDCD-VALDC-ROTRO         PIC X(1).
           10  B6RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  B6RT-CFUNC-VALDC-ROTRO          PIC S9(9)V COMP-3.
           10  B6RT-DULT-ADTTO-OPER            PIC X(10).
           10  B6RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  B6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B6RT-CTERM                      PIC X(8).
           10  B6RT-CCONTR-CORP-OPER           PIC S9(7)V COMP-3.
           10  B6RT-DINCL-REG                  PIC X(10).
           10  B6RT-CCART-LIM-CREDT            PIC X(5).
           10  B6RT-CINDCD-ALIQT-REDZD         PIC X(1).
           10  B6RT-PTX-JURO-DIA               PIC S9(3)V9(5) COMP-3.
           10  B6RT-CINDCD-FREQ-TX             PIC X(2).
