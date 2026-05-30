      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  24-ARGUMENTOS-ENTRADA.

       05  24-COLUNAS-DA-TABELA.
           10  24-CPRODT                   PIC S9(3)V COMP-3.
           10  24-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  24-CESPCE-TITLO-COBR        PIC S9(3)V COMP-3.
           10  24-HULT-ATULZ               PIC X(26).
           10  24-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  24-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  24-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  24-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  24-RETORNO.
       05  24-COD-RETORNO                         PIC 9(04).
       05  24-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  24-PROXIMO-RESTART                     PIC 9(05).
       05  24-QTDE-RETORNADA                      PIC 9(03).
       05  24-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  24-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T24.
           10  24RT-CPRODT                 PIC S9(3)V COMP-3.
           10  24RT-CTPO-DESC-COML         PIC S9(3)V COMP-3.
           10  24RT-CESPCE-TITLO-COBR      PIC S9(3)V COMP-3.
           10  24RT-HULT-ATULZ             PIC X(26).
           10  24RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  24RT-CTERM                  PIC X(8).
