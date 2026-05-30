      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K0 - DB2PRD.TERRO_PGTO_BASIL       *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K0-ARGUMENTOS-ENTRADA.

       05  K0-COLUNAS-DA-TABELA.
           10  K0-CERRO-PGTO-INCOT     PIC S9(002)       COMP-3.
           10  K0-RERRO-PGTO-INCOT     PIC  X(070).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K0-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K0-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K0-RETORNO.
       05  K0-COD-RETORNO              PIC 9(04).
       05  K0-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K0-PROXIMO-RESTART          PIC 9(05).
       05  K0-QTDE-RETORNADA           PIC 9(03).
       05  K0-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K0RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK0.
           10  K0RT-CERRO-PGTO-INCOT     PIC S9(002)       COMP-3.
           10  K0RT-RERRO-PGTO-INCOT     PIC  X(070).

