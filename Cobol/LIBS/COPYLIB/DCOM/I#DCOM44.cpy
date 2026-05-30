      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  44-ARGUMENTOS-ENTRADA.

       05  44-COLUNAS-DA-TABELA.
           10  44-CPRODT                   PIC S9(3)V COMP-3.
           10  44-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  44-CDOCTO-FORML-DESC        PIC X(10).
           10  44-HULT-ATULZ               PIC X(26).
           10  44-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  44-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  44-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  44-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  44-RETORNO.
       05  44-COD-RETORNO                         PIC 9(04).
       05  44-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  44-PROXIMO-RESTART                     PIC 9(05).
       05  44-QTDE-RETORNADA                      PIC 9(03).
       05  44-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  44-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T44.
           10  44RT-CPRODT                 PIC S9(3)V COMP-3.
           10  44RT-CTPO-DESC-COML         PIC S9(3)V COMP-3.
           10  44RT-CDOCTO-FORML-DESC      PIC X(10).
           10  44RT-HULT-ATULZ             PIC X(26).
           10  44RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  44RT-CTERM                  PIC X(8).
