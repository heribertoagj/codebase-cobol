      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I2 - DB2PRD.TIMPLT_SIST_AG         *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I2-ARGUMENTOS-ENTRADA.

       05  I2-COLUNAS-DA-TABELA.
           10  I2-CBCO                         PIC S9(3)V COMP-3.
           10  I2-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  I2-DINIC-DESC-COML              PIC X(10).
           10  I2-DFNAL-DESC-ONLINE            PIC X(10).
           10  I2-HULT-ATULZ                   PIC X(26).
           10  I2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  I2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I2-RETORNO.
       05  I2-COD-RETORNO                         PIC 9(04).
       05  I2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I2-PROXIMO-RESTART                     PIC 9(05).
       05  I2-QTDE-RETORNADA                      PIC 9(03).
       05  I2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI2.
           10  I2RT-CBCO                       PIC S9(3)V COMP-3.
           10  I2RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  I2RT-DINIC-DESC-COML            PIC X(10).
           10  I2RT-DFNAL-DESC-ONLINE          PIC X(10).
           10  I2RT-HULT-ATULZ                 PIC X(26).
           10  I2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  I2RT-CTERM                      PIC X(8).
