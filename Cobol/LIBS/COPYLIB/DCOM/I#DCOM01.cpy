      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  01-ARGUMENTOS-ENTRADA.

       05  01-COLUNAS-DA-TABELA.
           10  01-CCONVE-GRAL-DESC         PIC S9(9)V COMP-3.
           10  01-CVRSAO-CONVE-GRAL        PIC S9(3)V COMP-3.
           10  01-CSGL-UF                  PIC X(2).
           10  01-HULT-ATULZ               PIC X(26).
           10  01-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  01-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  01-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  01-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  01-RETORNO.
       05  01-COD-RETORNO                         PIC 9(04).
       05  01-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  01-PROXIMO-RESTART                     PIC 9(05).
       05  01-QTDE-RETORNADA                      PIC 9(03).
       05  01-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  01-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T01.
           10  01RT-CCONVE-GRAL-DESC       PIC S9(9)V COMP-3.
           10  01RT-CVRSAO-CONVE-GRAL      PIC S9(3)V COMP-3.
           10  01RT-CSGL-UF                PIC X(2).
           10  01RT-HULT-ATULZ             PIC X(26).
           10  01RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  01RT-CTERM                  PIC X(8).
