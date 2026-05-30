      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  L3-ARGUMENTOS-ENTRADA.

       05  L3-COLUNAS-DA-TABELA.
           10  L3-CPRODT                PIC S9(3)V COMP-3.
           10  L3-CSPROD-DESC-COML      PIC S9(3)V COMP-3.
           10  L3-CINDCD-PSSOA-TOMAD    PIC X(01).
           10  L3-CFUNC-BDSCO           PIC S9(9)V COMP-3.
           10  L3-HATULZ                PIC X(26).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  L3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  L3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  L3-RETORNO.
       05  L3-COD-RETORNO                         PIC 9(04).
       05  L3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  L3-PROXIMO-RESTART                     PIC 9(05).
       05  L3-QTDE-RETORNADA                      PIC 9(03).
       05  L3-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  L3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TL3.
           10  L3RT-CPRODT                PIC S9(3)V COMP-3.
           10  L3RT-CSPROD-DESC-COML      PIC S9(3)V COMP-3.
           10  L3RT-CINDCD-PSSOA-TOMAD    PIC X(01).
           10  L3RT-CFUNC-BDSCO           PIC S9(9)V COMP-3.
           10  L3RT-HATULZ                PIC X(26).
