      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB092 - DB2PRD.TAG_SGMTO_CLI          *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  92-ARGUMENTOS-ENTRADA.

       05  92-COLUNAS-DA-TABELA.
           10  92-CBCO                         PIC S9(3)V COMP-3.
           10  92-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  92-CSGMTO-CLI                   PIC S9(3)V COMP-3.
           10  92-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  92-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  92-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  92-RETORNO.
       05  92-COD-RETORNO                         PIC 9(04).
       05  92-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  92-PROXIMO-RESTART                     PIC 9(05).
       05  92-QTDE-RETORNADA                      PIC 9(03).
       05  92-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  92-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T92.
           10  92RT-CBCO                       PIC S9(3)V COMP-3.
           10  92RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  92RT-CSGMTO-CLI                 PIC S9(3)V COMP-3.
           10  92RT-HULT-ATULZ                 PIC X(26).
