      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  84-ARGUMENTOS-ENTRADA.

       05  84-COLUNAS-DA-TABELA.
           10  84-CTPO-DOCTO-FORML         PIC S9(3)V COMP-3.
           10  84-ITPO-DOCTO-FORML         PIC X(40).
           10  84-IRSUMO-TPO-DOCTO         PIC X(15).
           10  84-HULT-ATULZ               PIC X(26).
           10  84-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  84-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  84-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  84-RETORNO.
       05  84-COD-RETORNO                         PIC 9(04).
       05  84-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  84-PROXIMO-RESTART                     PIC 9(05).
       05  84-QTDE-RETORNADA                      PIC 9(03).
       05  84-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  84-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T84.
           10  84RT-CTPO-DOCTO-FORML       PIC S9(3)V COMP-3.
           10  84RT-ITPO-DOCTO-FORML       PIC X(40).
           10  84RT-IRSUMO-TPO-DOCTO       PIC X(15).
           10  84RT-HULT-ATULZ             PIC X(26).
           10  84RT-CTRANS-PROG-ATULZ      PIC X(8).
