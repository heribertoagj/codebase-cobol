      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H2 - DB2PRD.TCONVE_HORA_REMSS      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H2-ARGUMENTOS-ENTRADA.

       05  H2-COLUNAS-DA-TABELA.
           10  H2-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  H2-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  H2-HARQ-REMSS-CONVN             PIC X(8).
           10  H2-HULT-ATULZ                   PIC X(26).
           10  H2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H2-RETORNO.
       05  H2-COD-RETORNO                         PIC 9(04).
       05  H2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H2-PROXIMO-RESTART                     PIC 9(05).
       05  H2-QTDE-RETORNADA                      PIC 9(03).
       05  H2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH2.
           10  H2RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  H2RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  H2RT-HARQ-REMSS-CONVN           PIC X(8).
           10  H2RT-HULT-ATULZ                 PIC X(26).
           10  H2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H2RT-CTERM                      PIC X(8).
