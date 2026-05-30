      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  21-ARGUMENTOS-ENTRADA.

       05  21-COLUNAS-DA-TABELA.
           10  21-CCONVE-GRAL-DESC         PIC S9(9)V COMP-3.
           10  21-CVRSAO-CONVE-GRAL        PIC S9(3)V COMP-3.
           10  21-CEMPR-INC                PIC S9(5)V COMP-3.
           10  21-CDEPDC                   PIC S9(5)V COMP-3.
           10  21-HULT-ATULZ               PIC X(26).
           10  21-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  21-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  21-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  21-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  21-RETORNO.
       05  21-COD-RETORNO                         PIC 9(04).
       05  21-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  21-PROXIMO-RESTART                     PIC 9(05).
       05  21-QTDE-RETORNADA                      PIC 9(03).
       05  21-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  21-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T21.
           10  21RT-CCONVE-GRAL-DESC       PIC S9(9)V COMP-3.
           10  21RT-CVRSAO-CONVE-GRAL      PIC S9(3)V COMP-3.
           10  21RT-CEMPR-INC              PIC S9(5)V COMP-3.
           10  21RT-CDEPDC                 PIC S9(5)V COMP-3.
           10  21RT-HULT-ATULZ             PIC X(26).
           10  21RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  21RT-CTERM                  PIC X(8).
