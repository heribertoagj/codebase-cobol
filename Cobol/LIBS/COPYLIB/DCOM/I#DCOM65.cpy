      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  65-ARGUMENTOS-ENTRADA.

       05  65-COLUNAS-DA-TABELA.
           10  65-CPRODT                   PIC S9(3)V COMP-3.
           10  65-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  65-CMEIO-LIBRC-DESC         PIC S9(3)V COMP-3.
           10  65-HINIC-MEIO-LIBRC         PIC X(26).
           10  65-HFIM-MEIO-LIBRC          PIC X(26).
           10  65-HULT-ATULZ               PIC X(26).
           10  65-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  65-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  65-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  65-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  65-RETORNO.
       05  65-COD-RETORNO                         PIC 9(04).
       05  65-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  65-PROXIMO-RESTART                     PIC 9(05).
       05  65-QTDE-RETORNADA                      PIC 9(03).
       05  65-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  65-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T65.
           10  65RT-CPRODT                 PIC S9(3)V COMP-3.
           10  65RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  65RT-CMEIO-LIBRC-DESC       PIC S9(3)V COMP-3.
           10  65RT-HINIC-MEIO-LIBRC       PIC X(26).
           10  65RT-HFIM-MEIO-LIBRC        PIC X(26).
           10  65RT-HULT-ATULZ             PIC X(26).
           10  65RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  65RT-CTERM                  PIC X(8).
