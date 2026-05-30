      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I5 - DB2PRD.TPAREC_FORML_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I5-ARGUMENTOS-ENTRADA.

       05  I5-COLUNAS-DA-TABELA.
           10  I5-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  I5-HPAREC-FORML-DESC            PIC X(26).
           10  I5-CCHAVE-ELMTO-DESC            PIC X(45).
           10  I5-RPRCER-FORML-DESC.
               49  I5-RPRCER-FORML-DESC-LEN    PIC S9(4) USAGE COMP.
               49  I5-RPRCER-FORML-DESC-TEXT   PIC X(240).
           10  I5-HULT-ATULZ                   PIC X(26).
           10  I5-CFUNC-BDSCO                  PIC S9(9)V USAGE COMP-3.
           10  I5-CTERM                        PIC X(8).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I5-RETORNO.
       05  I5-COD-RETORNO                         PIC 9(04).
       05  I5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I5-PROXIMO-RESTART                     PIC 9(05).
       05  I5-QTDE-RETORNADA                      PIC 9(03).
       05  I5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI5.
           10  I5RT-CELMTO-DESC-COML            PIC S9(3)V COMP-3.
           10  I5RT-HPAREC-FORML-DESC           PIC X(26).
           10  I5RT-CCHAVE-ELMTO-DESC           PIC X(45).
           10  I5RT-RPRCER-FORML-DESC.
               49  I5RT-RPRCER-FORML-DESC-LEN   PIC S9(4) USAGE COMP.
               49  I5RT-RPRCER-FORML-DESC-TEXT  PIC X(240).
           10  I5RT-HULT-ATULZ                  PIC X(26).
           10  I5RT-CFUNC-BDSCO                 PIC S9(9)V USAGE COMP-3.
           10  I5RT-CTERM                       PIC X(8).
