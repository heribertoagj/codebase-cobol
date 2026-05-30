      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0L0 - DB2PRD.TSIT_ANLSE_CREDT       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  L0-ARGUMENTOS-ENTRADA.

       05  L0-COLUNAS-DA-TABELA.
           10  L0-CSIT-ANLSE-TITLO             PIC S9(3)V      COMP-3.
           10  L0-ISIT-ANLSE-CREDT             PIC X(30).
           10  L0-IRSUMO-SIT-ANLSE             PIC X(15).

      * AREA AUXILIAR PARA ACESSOS ESPECIAIS
      *-------------------------------------*
       05  L0-VAGO                               PIC X(100).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  L0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  L0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  L0-RETORNO.
       05  L0-COD-RETORNO                         PIC 9(04).
       05  L0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  L0-PROXIMO-RESTART                     PIC 9(05).
       05  L0-QTDE-RETORNADA                      PIC 9(03).
       05  L0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  L0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TL0.
           10  L0RT-CSIT-ANLSE-TITLO           PIC S9(3)V      COMP-3.
           10  L0RT-ISIT-ANLSE-CREDT           PIC X(30).
           10  L0RT-IRSUMO-SIT-ANLSE           PIC X(15).
