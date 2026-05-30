      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  15-ARGUMENTOS-ENTRADA.

       05  15-COLUNAS-DA-TABELA.
           10  15-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  15-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  15-CCONDC-PGTO-FORNC        PIC S9(5)V COMP-3.
           10  15-HULT-ATULZ               PIC X(26).
           10  15-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  15-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  15-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  15-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  15-RETORNO.
       05  15-COD-RETORNO                         PIC 9(04).
       05  15-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  15-PROXIMO-RESTART                     PIC 9(05).
       05  15-QTDE-RETORNADA                      PIC 9(03).
       05  15-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  15-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T15.
           10  15RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  15RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  15RT-CCONDC-PGTO-FORNC      PIC S9(5)V COMP-3.
           10  15RT-HULT-ATULZ             PIC X(26).
           10  15RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  15RT-CTERM                  PIC X(8).
