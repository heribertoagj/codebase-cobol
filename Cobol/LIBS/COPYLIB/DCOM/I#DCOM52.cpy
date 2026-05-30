      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  52-ARGUMENTOS-ENTRADA.

       05  52-COLUNAS-DA-TABELA.
           10  52-CREGRA-MORA-LEGAL        PIC S9(3)V COMP-3.
           10  52-IREGRA-MORA-LEGAL        PIC X(40).
           10  52-CIDTFD-CORRC-DIVDA       PIC S9(5)V COMP-3.
           10  52-CINDCD-CALC-MORA         PIC S9(1)V COMP-3.
           10  52-CIDTFD-JURO-MORA         PIC S9(5)V COMP-3.
           10  52-HULT-ATULZ               PIC X(26).
           10  52-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  52-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  52-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  52-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  52-RETORNO.
       05  52-COD-RETORNO                         PIC 9(04).
       05  52-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  52-PROXIMO-RESTART                     PIC 9(05).
       05  52-QTDE-RETORNADA                      PIC 9(03).
       05  52-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  52-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T52.
           10  52RT-CREGRA-MORA-LEGAL      PIC S9(3)V COMP-3.
           10  52RT-IREGRA-MORA-LEGAL      PIC X(40).
           10  52RT-CIDTFD-CORRC-DIVDA     PIC S9(5)V COMP-3.
           10  52RT-CINDCD-CALC-MORA       PIC S9(1)V COMP-3.
           10  52RT-CIDTFD-JURO-MORA       PIC S9(5)V COMP-3.
           10  52RT-HULT-ATULZ             PIC X(26).
           10  52RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  52RT-CTERM                  PIC X(8).
