      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0J7 - DB2PRD.TTX_OPER_DESC_CLI      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  J7-ARGUMENTOS-ENTRADA.

       05  J7-COLUNAS-DA-TABELA.
           10  J7-CBCO                 PIC S9(3)V COMP-3.
           10  J7-CAG-BCRIA            PIC S9(5)V COMP-3.
           10  J7-CCTA-BCRIA-CLI       PIC S9(13)V COMP-3.
           10  J7-CPRODT               PIC S9(3)V COMP-3.
           10  J7-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  J7-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  J7-DINIC-TX-OPER-DESC   PIC X(26).
           10  J7-CINDCD-FREQ-TX       PIC X(2).
           10  J7-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  J7-PTX-JURO-OPER-DESC   PIC S9(3)V9(5) COMP-3.
           10  J7-DFIM-TX-OPER-DESC    PIC X(26).
           10  J7-HULT-ATULZ           PIC X(26).
           10  J7-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  J7-CTERM                PIC X(8).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  J7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J7-RETORNO.
       05  J7-COD-RETORNO                         PIC 9(04).
       05  J7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J7-PROXIMO-RESTART                     PIC 9(05).
       05  J7-QTDE-RETORNADA                      PIC 9(03).
       05  J7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ7.
           10  J7RT-CBCO                 PIC S9(3)V COMP-3.
           10  J7RT-CAG-BCRIA            PIC S9(5)V COMP-3.
           10  J7RT-CCTA-BCRIA-CLI       PIC S9(13)V COMP-3.
           10  J7RT-CPRODT               PIC S9(3)V COMP-3.
           10  J7RT-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  J7RT-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  J7RT-DINIC-TX-OPER-DESC   PIC X(26).
           10  J7RT-CINDCD-FREQ-TX       PIC X(2).
           10  J7RT-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  J7RT-PTX-JURO-OPER-DESC   PIC S9(3)V9(5) COMP-3.
           10  J7RT-DFIM-TX-OPER-DESC    PIC X(26).
           10  J7RT-HULT-ATULZ           PIC X(26).
           10  J7RT-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  J7RT-CTERM                PIC X(8).
