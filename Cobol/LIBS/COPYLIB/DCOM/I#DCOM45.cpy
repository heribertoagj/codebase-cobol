      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB045 - DB2PRD.TPRODT_MEIO_ENTRD      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  45-ARGUMENTOS-ENTRADA.

       05  45-COLUNAS-DA-TABELA.
           10  45-CPRODT                       PIC S9(3)V COMP-3.
           10  45-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  45-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  45-CINDCD-MODLD-DESC            PIC X(1).
           10  45-CINDCD-SELEC-CART            PIC X(1).
           10  45-CINDCD-REG-CART              PIC X(1).
           10  45-HULT-ATULZ                   PIC X(26).
           10  45-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  45-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  45-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  45-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  45-RETORNO.
       05  45-COD-RETORNO                         PIC 9(04).
       05  45-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  45-PROXIMO-RESTART                     PIC 9(05).
       05  45-QTDE-RETORNADA                      PIC 9(03).
       05  45-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  45-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T45.
           10  45RT-CPRODT                     PIC S9(3)V COMP-3.
           10  45RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  45RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  45RT-CINDCD-MODLD-DESC          PIC X(1).
           10  45RT-CINDCD-SELEC-CART          PIC X(1).
           10  45RT-CINDCD-REG-CART            PIC X(1).
           10  45RT-HULT-ATULZ                 PIC X(26).
           10  45RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  45RT-CTERM                      PIC X(8).
