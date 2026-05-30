      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G4 - DB2PRD.TTRASI_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G4-ARGUMENTOS-ENTRADA.

       05  G4-COLUNAS-DA-TABELA.
           10  G4-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G4-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G4-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  G4-HSIT-DESC-COML               PIC X(26).
           10  G4-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  G4-VLIBRC-ANTCP-OPER            PIC S9(15)V9(2) COMP-3.
           10  G4-CBCO                         PIC S9(3)V COMP-3.
           10  G4-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  G4-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  G4-CBCO-OPER-OPER               PIC S9(3)V COMP-3.
           10  G4-CAG-OPER-OPER                PIC S9(5)V COMP-3.
           10  G4-CPRODT                       PIC S9(3)V COMP-3.
           10  G4-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  G4-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  G4-CCONTR-CONVE-DESC            PIC S9(9)V COMP-3.
           10  G4-CVRSAO-CONTR-CONVE           PIC S9(3)V COMP-3.
           10  G4-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  G4-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  G4-CINDCD-DIGTC-AG              PIC X(1).
           10  G4-CINDCD-ISENC-OPER            PIC X(1).
           10  G4-CINDCD-AUTRZ-OPER            PIC X(1).
           10  G4-CINDCD-MODLD-INCL            PIC X(1).
           10  G4-CCTRO-CUSTO                  PIC X(4).
           10  G4-DANO-BASE                    PIC S9(4)V COMP-3.
           10  G4-CDEPDC                       PIC S9(5)V COMP-3.
           10  G4-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  G4-CINDCD-APROV-CREDT           PIC X(1).
           10  G4-VOPER-DESC-COML              PIC S9(15)V9(2) COMP-3.
           10  G4-DINIC-OPER-DESC              PIC X(10).
           10  G4-DVCTO-FNAL-OPER              PIC X(10).
           10  G4-VOPER-DESC-APURA             PIC S9(15)V9(2) COMP-3.
           10  G4-QTITLO-OPER-APURA            PIC S9(5)V COMP-3.
           10  G4-DVCTO-FNAL-APURA             PIC X(10).
           10  G4-TOPER-DESC-APURA             PIC S9(3)V COMP-3.
           10  G4-TMED-PONDE-OPER              PIC S9(3)V COMP-3.
           10  G4-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  G4-QPCELA-OPER                  PIC S9(5)V COMP-3.
           10  G4-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  G4-NAUTRZ-TRNSM-ESCR            PIC S9(7)V COMP-3.
           10  G4-CINDCD-PGDOR-JURO            PIC X(1).
           10  G4-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  G4-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  G4-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  G4-PALIQT-IOF-DESC              PIC S9(3)V9(5) COMP-3.
           10  G4-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  G4-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  G4-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  G4-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  G4-VJURO-OPER-DESC              PIC S9(15)V9(2) COMP-3.
           10  G4-VJURO-CUSTO-OPER             PIC S9(15)V9(2) COMP-3.
           10  G4-VTARIF-REG-TITLO             PIC S9(15)V9(2) COMP-3.
           10  G4-VTAC-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  G4-VPSENT-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  G4-VIOF-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  G4-VLIQ-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  G4-CINDCD-VALDC-ROTRO           PIC X(1).
           10  G4-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  G4-CFUNC-VALDC-ROTRO            PIC S9(9)V COMP-3.
           10  G4-DULT-ADTTO-OPER              PIC X(10).
           10  G4-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  G4-HULT-ATULZ                   PIC X(26).
           10  G4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G4-CTERM                        PIC X(8).
           10  G4-CCONTR-CORP-OPER             PIC S9(7)V COMP-3.
           10  G4-DINCL-REG                    PIC X(10).
           10  G4-CCART-LIM-CREDT              PIC X(5).
           10  G4-CCHAVE-ELMTO-DESC            PIC X(45).
           10  G4-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  G4-CINDCD-ALIQT-REDZD           PIC X(1).
           10  G4-PTX-JURO-DIA                 PIC S9(3)V9(5) COMP-3.
           10  G4-CINDCD-FREQ-TX               PIC X(2).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G4-RETORNO.
       05  G4-COD-RETORNO                         PIC 9(04).
       05  G4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G4-PROXIMO-RESTART                     PIC 9(05).
       05  G4-QTDE-RETORNADA                      PIC 9(03).
       05  G4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG4.
           10  G4RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G4RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G4RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  G4RT-HSIT-DESC-COML             PIC X(26).
           10  G4RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  G4RT-VLIBRC-ANTCP-OPER          PIC S9(15)V9(2) COMP-3.
           10  G4RT-CBCO                       PIC S9(3)V COMP-3.
           10  G4RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  G4RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  G4RT-CBCO-OPER-OPER             PIC S9(3)V COMP-3.
           10  G4RT-CAG-OPER-OPER              PIC S9(5)V COMP-3.
           10  G4RT-CPRODT                     PIC S9(3)V COMP-3.
           10  G4RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  G4RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  G4RT-CCONTR-CONVE-DESC          PIC S9(9)V COMP-3.
           10  G4RT-CVRSAO-CONTR-CONVE         PIC S9(3)V COMP-3.
           10  G4RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  G4RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  G4RT-CINDCD-DIGTC-AG            PIC X(1).
           10  G4RT-CINDCD-ISENC-OPER          PIC X(1).
           10  G4RT-CINDCD-AUTRZ-OPER          PIC X(1).
           10  G4RT-CINDCD-MODLD-INCL          PIC X(1).
           10  G4RT-CCTRO-CUSTO                PIC X(4).
           10  G4RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  G4RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  G4RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  G4RT-CINDCD-APROV-CREDT         PIC X(1).
           10  G4RT-VOPER-DESC-COML            PIC S9(15)V9(2) COMP-3.
           10  G4RT-DINIC-OPER-DESC            PIC X(10).
           10  G4RT-DVCTO-FNAL-OPER            PIC X(10).
           10  G4RT-VOPER-DESC-APURA           PIC S9(15)V9(2) COMP-3.
           10  G4RT-QTITLO-OPER-APURA          PIC S9(5)V COMP-3.
           10  G4RT-DVCTO-FNAL-APURA           PIC X(10).
           10  G4RT-TOPER-DESC-APURA           PIC S9(3)V COMP-3.
           10  G4RT-TMED-PONDE-OPER            PIC S9(3)V COMP-3.
           10  G4RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  G4RT-QPCELA-OPER                PIC S9(5)V COMP-3.
           10  G4RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  G4RT-NAUTRZ-TRNSM-ESCR          PIC S9(7)V COMP-3.
           10  G4RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  G4RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  G4RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  G4RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  G4RT-PALIQT-IOF-DESC            PIC S9(3)V9(5) COMP-3.
           10  G4RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  G4RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  G4RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  G4RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  G4RT-VJURO-OPER-DESC            PIC S9(15)V9(2) COMP-3.
           10  G4RT-VJURO-CUSTO-OPER           PIC S9(15)V9(2) COMP-3.
           10  G4RT-VTARIF-REG-TITLO           PIC S9(15)V9(2) COMP-3.
           10  G4RT-VTAC-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  G4RT-VPSENT-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  G4RT-VIOF-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  G4RT-VLIQ-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  G4RT-CINDCD-VALDC-ROTRO         PIC X(1).
           10  G4RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  G4RT-CFUNC-VALDC-ROTRO          PIC S9(9)V COMP-3.
           10  G4RT-DULT-ADTTO-OPER            PIC X(10).
           10  G4RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  G4RT-HULT-ATULZ                 PIC X(26).
           10  G4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G4RT-CTERM                      PIC X(8).
           10  G4RT-CCONTR-CORP-OPER           PIC S9(7)V COMP-3.
           10  G4RT-DINCL-REG                  PIC X(10).
           10  G4RT-CCART-LIM-CREDT            PIC X(5).
           10  G4RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  G4RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  G4RT-CINDCD-ALIQT-REDZD         PIC X(1).
           10  G4RT-PTX-JURO-DIA               PIC S9(3)V9(5) COMP-3.
           10  G4RT-CINDCD-FREQ-TX             PIC X(2).

