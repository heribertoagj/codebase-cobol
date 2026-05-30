      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0J9 - DB2PRD.TCTCIA_PGTO_BASIL      *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  J9-ARGUMENTOS-ENTRADA.

       05  J9-COLUNAS-DA-TABELA.
           10  J9-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  J9-CERRO-PGTO-INCOT     PIC S9(002)       COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J9-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  J9-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J9-RETORNO.
       05  J9-COD-RETORNO              PIC 9(04).
       05  J9-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J9-PROXIMO-RESTART          PIC 9(05).
       05  J9-QTDE-RETORNADA           PIC 9(03).
       05  J9-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J9RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ9.
           10  J9RT-NSEQ-PGTO-INCOT      PIC S9(012)       COMP-3.
           10  J9RT-CERRO-PGTO-INCOT     PIC S9(002)       COMP-3.

