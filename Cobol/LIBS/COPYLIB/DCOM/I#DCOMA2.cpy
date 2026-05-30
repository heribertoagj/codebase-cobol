      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A2 - DB2PRD.TFATO_GERDR_SIT        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  A2-ARGUMENTOS-ENTRADA.

       05  A2-COLUNAS-DA-TABELA.
           10  A2-CFATO-GERDR-CTBIL            PIC S9(3)V COMP-3.
           10  A2-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  A2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A2-HULT-ATULZ                   PIC X(26).
           10  A2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A2-RETORNO.
       05  A2-COD-RETORNO                         PIC 9(04).
       05  A2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A2-PROXIMO-RESTART                     PIC 9(05).
       05  A2-QTDE-RETORNADA                      PIC 9(03).
       05  A2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA2.
           10  A2RT-CFATO-GERDR-CTBIL          PIC S9(3)V COMP-3.
           10  A2RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  A2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A2RT-HULT-ATULZ                 PIC X(26).
           10  A2RT-CTERM                      PIC X(8).
