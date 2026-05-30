      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E5 - DB2PRD.TSMULA_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E5-ARGUMENTOS-ENTRADA.

       05  E5-COLUNAS-DA-TABELA.
           10  E5-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E5-DINIC-OPER-DESC              PIC X(10).
           10  E5-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  E5-HSIT-DESC-COML               PIC X(26).
           10  E5-CPRODT                       PIC S9(3)V COMP-3.
           10  E5-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  E5-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  E5-CBCO                         PIC S9(3)V COMP-3.
           10  E5-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  E5-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  E5-CBCO-OPER-OPER               PIC S9(3)V COMP-3.
           10  E5-CAG-OPER-OPER                PIC S9(5)V COMP-3.
           10  E5-CCONTR-CONVE-DESC            PIC S9(9)V COMP-3.
           10  E5-CVRSAO-CONTR-CONVE           PIC S9(3)V COMP-3.
           10  E5-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  E5-CINDCD-DIGTC-AG              PIC X(1).
           10  E5-CINDCD-MODLD-INCL            PIC X(1).
           10  E5-CCTRO-CUSTO                  PIC X(4).
           10  E5-VOPER-DESC-COML              PIC S9(15)V9(2) COMP-3.
           10  E5-TOPER-DESC-APURA             PIC S9(3)V COMP-3.
           10  E5-VOPER-DESC-APURA             PIC S9(15)V9(2) COMP-3.
           10  E5-QTITLO-OPER-APURA            PIC S9(5)V COMP-3.
           10  E5-DVCTO-FNAL-APURA             PIC X(10).
           10  E5-TMED-PONDE-OPER              PIC S9(3)V COMP-3.
           10  E5-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  E5-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  E5-PALIQT-IOF-DESC              PIC S9(3)V9(5) COMP-3.
           10  E5-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  E5-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  E5-VJURO-OPER-DESC              PIC S9(15)V9(2) COMP-3.
           10  E5-VTARIF-REG-TITLO             PIC S9(15)V9(2) COMP-3.
           10  E5-VTAC-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  E5-VIOF-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  E5-VLIQ-OPER-DESC               PIC S9(15)V9(2) COMP-3.
           10  E5-HULT-ATULZ                   PIC X(26).
           10  E5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E5-CTERM                        PIC X(8).
           10  E5-DINCL-REG                    PIC X(10).
           10  E5-CCHAVE-ELMTO-DESC            PIC X(45).
  LT       10  E5-PTX-CUSTO-SPREAD             PIC S9(3)V9(3) COMP-3.
           10  E5-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  E5-CINDCD-ALIQT-REDZD           PIC X(1).
           10  E5-PTX-JURO-DIA                 PIC S9(3)V9(5) COMP-3.
           10  E5-CINDCD-FREQ-TX               PIC X(2).
DTS001     10  E5-DANO-BASE                    PIC S9(4)V COMP-3.
DTS001     10  E5-CDEPDC                       PIC S9(5)V COMP-3.
DTS001     10  E5-CSEQ-STUDO                   PIC S9(9)V COMP-3.
DTS001     10  E5-CCART-LIM-CREDT              PIC X(5).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E5-RETORNO.
       05  E5-COD-RETORNO                         PIC 9(04).
       05  E5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E5-PROXIMO-RESTART                     PIC 9(05).
       05  E5-QTDE-RETORNADA                      PIC 9(03).
       05  E5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE5.
           10  E5RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E5RT-DINIC-OPER-DESC            PIC X(10).
           10  E5RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  E5RT-HSIT-DESC-COML             PIC X(26).
           10  E5RT-CPRODT                     PIC S9(3)V COMP-3.
           10  E5RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  E5RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  E5RT-CBCO                       PIC S9(3)V COMP-3.
           10  E5RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  E5RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  E5RT-CBCO-OPER-OPER             PIC S9(3)V COMP-3.
           10  E5RT-CAG-OPER-OPER              PIC S9(5)V COMP-3.
           10  E5RT-CCONTR-CONVE-DESC          PIC S9(9)V COMP-3.
           10  E5RT-CVRSAO-CONTR-CONVE         PIC S9(3)V COMP-3.
           10  E5RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  E5RT-CINDCD-DIGTC-AG            PIC X(1).
           10  E5RT-CINDCD-MODLD-INCL          PIC X(1).
           10  E5RT-CCTRO-CUSTO                PIC X(4).
           10  E5RT-VOPER-DESC-COML            PIC S9(15)V9(2) COMP-3.
           10  E5RT-TOPER-DESC-APURA           PIC S9(3)V COMP-3.
           10  E5RT-VOPER-DESC-APURA           PIC S9(15)V9(2) COMP-3.
           10  E5RT-QTITLO-OPER-APURA          PIC S9(5)V COMP-3.
           10  E5RT-DVCTO-FNAL-APURA           PIC X(10).
           10  E5RT-TMED-PONDE-OPER            PIC S9(3)V COMP-3.
           10  E5RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  E5RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  E5RT-PALIQT-IOF-DESC            PIC S9(3)V9(5) COMP-3.
           10  E5RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  E5RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  E5RT-VJURO-OPER-DESC            PIC S9(15)V9(2) COMP-3.
           10  E5RT-VTARIF-REG-TITLO           PIC S9(15)V9(2) COMP-3.
           10  E5RT-VTAC-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  E5RT-VIOF-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  E5RT-VLIQ-OPER-DESC             PIC S9(15)V9(2) COMP-3.
           10  E5RT-HULT-ATULZ                 PIC X(26).
           10  E5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E5RT-CTERM                      PIC X(8).
           10  E5RT-DINCL-REG                  PIC X(10).
           10  E5RT-CCHAVE-ELMTO-DESC          PIC X(45).
  LT       10  E5RT-PTX-CUSTO-SPREAD           PIC S9(3)V9(3) COMP-3.
           10  E5RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  E5RT-CINDCD-ALIQT-REDZD         PIC X(1).
           10  E5RT-PTX-JURO-DIA               PIC S9(3)V9(5) COMP-3.
           10  E5RT-CINDCD-FREQ-TX             PIC X(2).
DTS001     10  E5RT-DANO-BASE                  PIC S9(4)V COMP-3.
DTS001     10  E5RT-CDEPDC                     PIC S9(5)V COMP-3.
DTS001     10  E5RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
DTS001     10  E5RT-CCART-LIM-CREDT            PIC X(5).

