      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  61-ARGUMENTOS-ENTRADA.

       05  61-COLUNAS-DA-TABELA.
           10  61-CCONTR-LIM-DESC          PIC S9(9)V COMP-3.
           10  61-CVRSAO-CONTR-LIM         PIC S9(3)V COMP-3.
           10  61-CPRODT                   PIC S9(3)V COMP-3.
           10  61-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  61-HULT-ATULZ               PIC X(26).
           10  61-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  61-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  61-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  61-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  61-RETORNO.
       05  61-COD-RETORNO                         PIC 9(04).
       05  61-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  61-PROXIMO-RESTART                     PIC 9(05).
       05  61-QTDE-RETORNADA                      PIC 9(03).
       05  61-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  61-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T61.
           10  61RT-CCONTR-LIM-DESC        PIC S9(9)V COMP-3.
           10  61RT-CVRSAO-CONTR-LIM       PIC S9(3)V COMP-3.
           10  61RT-CPRODT                 PIC S9(3)V COMP-3.
           10  61RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  61RT-HULT-ATULZ             PIC X(26).
           10  61RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  61RT-CTERM                  PIC X(8).
