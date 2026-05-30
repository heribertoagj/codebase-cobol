      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K3 - DB2PRD.TTPO_PGTO_BASIL        *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K3-ARGUMENTOS-ENTRADA.

       05  K3-COLUNAS-DA-TABELA.
           10  K3-CTPO-PGTO-BASIL      PIC S9(002)       COMP-3.
           10  K3-RTPO-PGTO-BASIL      PIC  X(040).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K3-QTDE-A-DESPREZAR         PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K3-INSTRUCAO                PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K3-RETORNO.
       05  K3-COD-RETORNO              PIC 9(04).
       05  K3-MENSAGEM                 PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K3-PROXIMO-RESTART          PIC 9(05).
       05  K3-QTDE-RETORNADA           PIC 9(03).
       05  K3-QTDE-TOTAL               PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K3RT-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK3.
           10  K3RT-CTPO-PGTO-BASIL      PIC S9(002)       COMP-3.
           10  K3RT-RTPO-PGTO-BASIL      PIC  X(040).

