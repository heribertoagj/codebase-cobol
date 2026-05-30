      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  81-ARGUMENTOS-ENTRADA.

       05  81-COLUNAS-DA-TABELA.
           10  81-CTPO-COBR-TAC            PIC S9(3)V COMP-3.
           10  81-ITPO-COBR-TAC            PIC X(30).
           10  81-IRSUMO-COBR-TAC          PIC X(15).
           10  81-CSGL-TPO-COBR-TAC        PIC X(6).
           10  81-HULT-ATULZ               PIC X(26).
           10  81-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  81-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  81-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  81-RETORNO.
       05  81-COD-RETORNO                         PIC 9(04).
       05  81-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  81-PROXIMO-RESTART                     PIC 9(05).
       05  81-QTDE-RETORNADA                      PIC 9(03).
       05  81-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  81-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T81.
           10  81RT-CTPO-COBR-TAC          PIC S9(3)V COMP-3.
           10  81RT-ITPO-COBR-TAC          PIC X(30).
           10  81RT-IRSUMO-COBR-TAC        PIC X(15).
           10  81RT-CSGL-TPO-COBR-TAC      PIC X(6).
           10  81RT-HULT-ATULZ             PIC X(26).
           10  81RT-CTRANS-PROG-ATULZ      PIC X(8).
