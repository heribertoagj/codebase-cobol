      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I9 - DB2PRD.TTPO_SGMTO_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I9-ARGUMENTOS-ENTRADA.

       05  I9-COLUNAS-DA-TABELA.
           10  I9-CSGMTO-CLI                   PIC S9(3) COMP-3.
           10  I9-CTPO-PSSOA                   PIC X(1).
           10  I9-ISGMTO-CLI                   PIC X(20).
           10  I9-HULT-ATULZ                   PIC X(26).
           10  I9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  I9-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I9-RETORNO.
       05  I9-COD-RETORNO                         PIC 9(04).
       05  I9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I9-PROXIMO-RESTART                     PIC 9(05).
       05  I9-QTDE-RETORNADA                      PIC 9(03).
       05  I9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI9.
           10  I9RT-CSGMTO-CLI                  PIC S9(3) COMP-3.
           10  I9RT-CTPO-PSSOA                  PIC X(1).
           10  I9RT-ISGMTO-CLI                  PIC X(20).
           10  I9RT-HULT-ATULZ                  PIC X(26).
           10  I9RT-CFUNC-BDSCO                 PIC S9(9)V COMP-3.
           10  I9RT-CTERM                       PIC X(8).
