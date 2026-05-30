      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  30-ARGUMENTOS-ENTRADA.

       05  30-COLUNAS-DA-TABELA.
           10  30-CFATO-GERDR-CTBIL        PIC S9(3)V COMP-3.
           10  30-IFATO-GERDR-CTBIL        PIC X(40).
           10  30-IRSUMO-FATO-GERDR        PIC X(15).
           10  30-HULT-ATULZ               PIC X(26).
           10  30-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  30-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  30-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  30-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  30-RETORNO.
       05  30-COD-RETORNO                         PIC 9(04).
       05  30-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  30-PROXIMO-RESTART                     PIC 9(05).
       05  30-QTDE-RETORNADA                      PIC 9(03).
       05  30-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  30-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T30.
           10  30RT-CFATO-GERDR-CTBIL      PIC S9(3)V COMP-3.
           10  30RT-IFATO-GERDR-CTBIL      PIC X(40).
           10  30RT-IRSUMO-FATO-GERDR      PIC X(15).
           10  30RT-HULT-ATULZ             PIC X(26).
           10  30RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  30RT-CTERM                  PIC X(8).
