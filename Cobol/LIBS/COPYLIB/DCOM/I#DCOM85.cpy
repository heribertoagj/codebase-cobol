      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  85-ARGUMENTOS-ENTRADA.

       05  85-COLUNAS-DA-TABELA.
           10  85-CTPO-ISENC-DESC          PIC S9(3)V COMP-3.
           10  85-ITPO-ISENC-DESC          PIC X(30).
           10  85-IRSUMO-TPO-ISENC         PIC X(15).
           10  85-HULT-ATULZ               PIC X(26).
           10  85-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  85-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  85-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  85-RETORNO.
       05  85-COD-RETORNO                         PIC 9(04).
       05  85-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  85-PROXIMO-RESTART                     PIC 9(05).
       05  85-QTDE-RETORNADA                      PIC 9(03).
       05  85-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  85-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T85.
           10  85RT-CTPO-ISENC-DESC        PIC S9(3)V COMP-3.
           10  85RT-ITPO-ISENC-DESC        PIC X(30).
           10  85RT-IRSUMO-TPO-ISENC       PIC X(15).
           10  85RT-HULT-ATULZ             PIC X(26).
           10  85RT-CTRANS-PROG-ATULZ      PIC X(8).
