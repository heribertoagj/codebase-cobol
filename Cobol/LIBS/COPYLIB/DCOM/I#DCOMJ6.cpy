      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0J6 - DB2PRD.TCOMPV_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  J6-ARGUMENTOS-ENTRADA.

       05  J6-COLUNAS-DA-TABELA.
           10  J6-CBCO                     PIC S9(3)V COMP-3.
           10  J6-CAG-BCRIA                PIC S9(5)V COMP-3.
           10  J6-CCTA-BCRIA-CLI           PIC S9(13)V COMP-3.
           10  J6-DANO-OPER-DESC           PIC S9(4)V COMP-3.
           10  J6-NSEQ-OPER-DESC           PIC S9(9)V COMP-3.
           10  J6-CPRODT                   PIC S9(3)V COMP-3.
           10  J6-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  J6-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  J6-CADM-CATAO-CREDT         PIC S9(9)V COMP-3.
           10  J6-CINDCD-VDA-RCBVL         PIC X(1).
           10  J6-VOPER-DESC-COML          PIC S9(15)V9(2) COMP-3.
           10  J6-VIOF-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6-VTAC-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6-VJURO-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           10  J6-VLIQ-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6-CINDCD-FREQ-TX           PIC X(2).
           10  J6-PTX-JURO-OPER-DESC       PIC S9(3)V9(5) COMP-3.
           10  J6-DINCL-REG                PIC X(10).
           10  J6-DINIC-OPER-DESC          PIC X(10).
           10  J6-DVCTO-FNAL-OPER          PIC X(10).
           10  J6-TMED-PONDE-OPER          PIC S9(3)V COMP-3.
           10  J6-HULT-ATULZ               PIC X(26).
           10  J6-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  J6-CTERM                    PIC X(8).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  J6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J6-RETORNO.
       05  J6-COD-RETORNO                         PIC 9(04).
       05  J6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J6-PROXIMO-RESTART                     PIC 9(05).
       05  J6-QTDE-RETORNADA                      PIC 9(03).
       05  J6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ6.
           10  J6RT-CBCO                     PIC S9(3)V COMP-3.
           10  J6RT-CAG-BCRIA                PIC S9(5)V COMP-3.
           10  J6RT-CCTA-BCRIA-CLI           PIC S9(13)V COMP-3.
           10  J6RT-DANO-OPER-DESC           PIC S9(4)V COMP-3.
           10  J6RT-NSEQ-OPER-DESC           PIC S9(9)V COMP-3.
           10  J6RT-CPRODT                   PIC S9(3)V COMP-3.
           10  J6RT-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  J6RT-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  J6RT-CADM-CATAO-CREDT         PIC S9(9)V COMP-3.
           10  J6RT-CINDCD-VDA-RCBVL         PIC X(1).
           10  J6RT-VOPER-DESC-COML          PIC S9(15)V9(2) COMP-3.
           10  J6RT-VIOF-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6RT-VTAC-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6RT-VJURO-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           10  J6RT-VLIQ-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  J6RT-CINDCD-FREQ-TX           PIC X(2).
           10  J6RT-PTX-JURO-OPER-DESC       PIC S9(3)V9(5) COMP-3.
           10  J6RT-DINCL-REG                PIC X(10).
           10  J6RT-DINIC-OPER-DESC          PIC X(10).
           10  J6RT-DVCTO-FNAL-OPER          PIC X(10).
           10  J6RT-TMED-PONDE-OPER          PIC S9(3)V COMP-3.
           10  J6RT-HULT-ATULZ               PIC X(26).
           10  J6RT-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  J6RT-CTERM                    PIC X(8).
