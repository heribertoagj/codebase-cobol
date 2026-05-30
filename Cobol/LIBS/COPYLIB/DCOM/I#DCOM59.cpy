      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  59-ARGUMENTOS-ENTRADA.

       05  59-COLUNAS-DA-TABELA.
           10  59-CPRODT                   PIC S9(3)V COMP-3.
           10  59-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  59-CAVISO-CLI-DESC          PIC S9(5)V COMP-3.
           10  59-CPERDC-DESC-COML         PIC X(3).
           10  59-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  59-HSIT-DESC-COML           PIC X(26).
           10  59-HULT-ATULZ               PIC X(26).
           10  59-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  59-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  59-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  59-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  59-RETORNO.
       05  59-COD-RETORNO                         PIC 9(04).
       05  59-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  59-PROXIMO-RESTART                     PIC 9(05).
       05  59-QTDE-RETORNADA                      PIC 9(03).
       05  59-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  59-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T59.
           10  59RT-CPRODT                 PIC S9(3)V COMP-3.
           10  59RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  59RT-CAVISO-CLI-DESC        PIC S9(5)V COMP-3.
           10  59RT-CPERDC-DESC-COML       PIC X(3).
           10  59RT-CSIT-DESC-COML         PIC S9(3)V COMP-3.
           10  59RT-HSIT-DESC-COML         PIC X(26).
           10  59RT-HULT-ATULZ             PIC X(26).
           10  59RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  59RT-CTERM                  PIC X(8).
