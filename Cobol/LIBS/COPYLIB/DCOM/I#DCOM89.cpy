      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  89-ARGUMENTOS-ENTRADA.

       05  89-COLUNAS-DA-TABELA.
           10  89-CCONVE-GRAL-DESC          PIC S9(9)V COMP-3.
           10  89-CVRSAO-CONVE-GRAL         PIC S9(3)V COMP-3.
FVB   *    10  89-CEVNTO-WORKF-DESC         PIC S9(3)V COMP-3.
FVB        10  89-CEVNTO-DESC-COML          PIC S9(3)V COMP-3.
           10  89-HWORKF-CONVE-GRAL         PIC X(26).
FVB        10  89-CSIT-INIC-DESC            PIC S9(3)V COMP-3.
FVB        10  89-HSIT-INIC-DESC            PIC X(26).
FVB        10  89-CSIT-FNAL-DESC            PIC S9(3)V COMP-3.
FVB        10  89-HSIT-FNAL-DESC            PIC X(26).
           10  89-RWORKF-CONVE-GRAL.
              49  89-RWORKF-CONVE-GRAL-LEN  PIC S9(4) COMP.
              49  89-RWORKF-CONVE-GRAL-TEXT PIC X(240).
           10  89-HULT-ATULZ                PIC X(26).
           10  89-CFUNC-BDSCO               PIC S9(9)V COMP-3.
           10  89-CTERM                     PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  89-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  89-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  89-RETORNO.
       05  89-COD-RETORNO                         PIC 9(04).
       05  89-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  89-PROXIMO-RESTART                     PIC 9(05).
       05  89-QTDE-RETORNADA                      PIC 9(03).
       05  89-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  89-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T89.
           10  89RT-CCONVE-GRAL-DESC          PIC S9(9)V COMP-3.
           10  89RT-CVRSAO-CONVE-GRAL         PIC S9(3)V COMP-3.
FVB   *    10  89RT-CEVNTO-WORKF-DESC         PIC S9(3)V COMP-3.
FVB        10  89RT-CEVNTO-DESC-COML          PIC S9(3)V COMP-3.
           10  89RT-HWORKF-CONVE-GRAL         PIC X(26).
FVB        10  89RT-CSIT-INIC-DESC            PIC S9(3)V COMP-3.
FVB        10  89RT-HSIT-INIC-DESC            PIC X(26).
FVB        10  89RT-CSIT-FNAL-DESC            PIC S9(3)V COMP-3.
FVB        10  89RT-HSIT-FNAL-DESC            PIC X(26).
           10  89RT-RWORKF-CONVE-GRAL.
              49  89RT-RWORKF-CONVE-GRAL-LEN  PIC S9(4) COMP.
              49  89RT-RWORKF-CONVE-GRAL-TEXT PIC X(240).
           10  89RT-HULT-ATULZ                PIC X(26).
           10  89RT-CFUNC-BDSCO               PIC S9(9)V COMP-3.
           10  89RT-CTERM                     PIC X(8).
