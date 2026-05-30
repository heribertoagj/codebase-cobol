      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A5 - DB2PRD.TGARNT_CONVE_CLI       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  A5-ARGUMENTOS-ENTRADA.

       05  A5-COLUNAS-DA-TABELA.
           10  A5-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  A5-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  A5-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  A5-CTERM                        PIC X(8).
           10  A5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A5-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A5-RETORNO.
       05  A5-COD-RETORNO                         PIC 9(04).
       05  A5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A5-PROXIMO-RESTART                     PIC 9(05).
       05  A5-QTDE-RETORNADA                      PIC 9(03).
       05  A5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA5.
           10  A5RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  A5RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  A5RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  A5RT-CTERM                      PIC X(8).
           10  A5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A5RT-HULT-ATULZ                 PIC X(26).
