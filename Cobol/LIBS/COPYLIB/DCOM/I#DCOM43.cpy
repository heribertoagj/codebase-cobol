      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  43-ARGUMENTOS-ENTRADA.

       05  43-COLUNAS-DA-TABELA.
           10  43-CPRODT                   PIC S9(3)V COMP-3.
           10  43-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  43-CCONDC-PGTO-FORNC        PIC S9(5)V COMP-3.
           10  43-HULT-ATULZ               PIC X(26).
           10  43-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  43-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  43-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  43-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  43-RETORNO.
       05  43-COD-RETORNO                         PIC 9(04).
       05  43-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  43-PROXIMO-RESTART                     PIC 9(05).
       05  43-QTDE-RETORNADA                      PIC 9(03).
       05  43-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  43-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T43.
           10  43RT-CPRODT                 PIC S9(3)V COMP-3.
           10  43RT-CTPO-DESC-COML         PIC S9(3)V COMP-3.
           10  43RT-CCONDC-PGTO-FORNC      PIC S9(5)V COMP-3.
           10  43RT-HULT-ATULZ             PIC X(26).
           10  43RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  43RT-CTERM                  PIC X(8).
